(defpackage #:coalton-eff/pool
  (:use #:cl #:bordeaux-threads)
  (:import-from #:alexandria
    #:when-let
    #:if-let)
  (:export
   #:future

   #:thread-pool
   #:make-thread-pool
   #:make-dynamic-thread-pool
   #:submit-task
   #:shutdown-pool
   #:with-thread-pool
   #:with-dynamic-thread-pool
   #:future-ready-p
   #:get-future-value))

(declaim (optimize (speed 0) (space 0) (debug 3)))

(in-package #:coalton-eff/pool)

;;;; FUTURE ------------------------------------------------------------------
;; TODO: These should *REALLY* be private to the package, and expose read-only ones.
;; TODO: This needs to have a lock to use for waiting on the condition.
(defclass future ()
  ((value :initform nil :accessor future-value)
   (ready-p :initform nil :accessor future-ready-p)
   (wait-lock :initform (make-lock "future-wait") :reader future-wait-lock)
   (condition :initform (make-condition-variable) :accessor future-condition)))

(defun make-future ()
  (make-instance 'future))

(defun resolve-future (f resolved-value)
  (setf (future-value f) resolved-value)
  (setf (future-ready-p f) t)
  (condition-notify (future-condition f)))

(defun get-future-value (f &key (timeout nil))
  (unless (future-ready-p f)
    (with-lock-held ((future-wait-lock f))
      (condition-wait (future-condition f) (future-wait-lock f) :timeout timeout)))
  (if (future-ready-p f)
      (future-value f)
      (error "Future timed out")))

;;;; BASE CLASS (Fixed size) -------------------------------------------------
(defstruct task
  thunk
  future)

(defclass thread-pool ()
  ((task-queue :initform (list)  :accessor task-queue)
   (mutex      :initform (make-lock "pool-mutex") :reader pool-mutex
               :documentation "Protects adding to the queue and (for dynamic pools) adding and removing workers.")
   (work-mutex :initform (make-lock "pool-work-mutex") :reader pool-work-mutex
               :documentation "Used for the waiting on new tasks.")
   (condition  :initform (make-condition-variable) :reader pool-cond)
   (workers    :initform (list) :accessor workers)
   (running    :initform t :accessor pool-running-p)))

(defun %spawn-worker (pool fn)
  (let ((thr (make-thread fn :name (format nil "~a-worker-~a" (class-name (class-of pool)) (gensym)))))
    (push thr (workers pool))))

(defun pop-next-task-safe (pool)
  "Pop and return the next task from POOL, or NIL if pool is empty. Threadsafe!"
  (with-lock-held ((pool-mutex pool))
    (pop (task-queue pool))))

(defgeneric submit-task (pool thunk))

;;;; Fixed‑size implementation ----------------------------------------------
(defun make-thread-pool (size)
  "Return a fixed‑size thread pool with SIZE workers."
  (let ((pool (make-instance 'thread-pool)))
    (loop repeat size
          do (%spawn-worker pool (lambda () (static-worker-loop pool))))
    pool))

(defun static-worker-loop (pool)
  (let ((id (gensym "pool")))
    (loop
      (if-let (task (pop-next-task-safe pool))
        (progn
          (resolve-future (task-future task)
                          (handler-case (funcall (task-thunk task))
                            (error (e) e))))
        (with-lock-held ((pool-work-mutex pool))
          (condition-wait (pool-cond pool) (pool-work-mutex pool))))
      ;; Note: This should *probably* be wrapped in a lock. But in theory, with-lock-held
      ;; is undefined behavior when re-acquiring a lock held by the same thread. It should
      ;; probably be a contract that pools can only be turned off, not turned back on, so
      ;; this should probably be fine.
      (unless (pool-running-p pool)
        (return)))))

(defmethod submit-task ((pool thread-pool) thunk)
  (assert (functionp thunk))
  (let ((fut (make-future)))
    (with-lock-held ((pool-mutex pool))
      (push (make-task :thunk thunk :future fut) (task-queue pool)))
    (with-lock-held ((pool-work-mutex pool))
      (condition-notify (pool-cond pool)))
    fut))

;;;; Dynamic pool subclass ---------------------------------------------------
(defclass dynamic-thread-pool (thread-pool)
  ((min-threads :initarg :min :reader min-threads)
   (max-threads :initarg :max :initform nil :reader max-threads) ; NIL means no cap
   (idle-count  :initform 0 :accessor idle-count)))

(defun make-dynamic-thread-pool (min &key max)
  "Create a dynamic thread pool.
   MIN — threads kept alive at all times (≥1).
   MAX — optional cap (NIL means unlimited)."
  (when (and max (< max min)) (error "MAX must be ≥ MIN"))
  (let ((pool (make-instance 'dynamic-thread-pool :min min :max max)))
    (loop repeat min
          do (%spawn-worker pool (lambda () (dynamic-worker-loop pool))))
    pool))

(defun prune-dynamic-pool (pool thread)
  "Remove THREAD from dynamic thread pool POOL if appropriate. Returns T if pruned.
Not thread safe!"
  (when (and (not (task-queue pool))
             (> (length (workers pool)) (min-threads pool)))
    (setf (workers pool) (remove thread (workers pool)))
    t))

(defun dynamic-worker-loop (pool)
  (loop
    (if-let (task (pop-next-task-safe pool))
      (resolve-future (task-future task)
                      (handler-case (funcall (task-thunk task))
                        (error (e) e)))
      (progn
        (with-lock-held ((pool-mutex pool))
          (incf (idle-count pool)))
        (unwind-protect (with-lock-held ((pool-work-mutex pool))
                          (condition-wait (pool-cond pool) (pool-work-mutex pool)))
          (with-lock-held ((pool-mutex pool))
            (decf (idle-count pool))
            (when (prune-dynamic-pool pool (current-thread))
              (return))))))
    (unless (pool-running-p pool)
      (return))
    (with-lock-held ((pool-mutex pool))
      (when (prune-dynamic-pool pool (current-thread))
        (return)))))

(defmethod submit-task ((pool dynamic-thread-pool) thunk)
  (assert (functionp thunk))
  (let ((fut (make-future)))
    (with-lock-held ((pool-mutex pool))
      (push (make-task :thunk thunk :future fut) (task-queue pool))
      ;; Spawn a new worker if nobody is idle and we haven’t hit MAX yet.
      (when (and (>= (length (task-queue pool)) (idle-count pool) 0)
                 (or (null (max-threads pool)) (< (length (workers pool)) (max-threads pool))))
        (%spawn-worker pool (lambda () (dynamic-worker-loop pool))))
      (with-lock-held ((pool-work-mutex pool))
        (condition-notify (pool-cond pool))))
    fut))


(defun shutdown-pool (pool &key (await t))
  (with-lock-held ((pool-mutex pool))
    (setf (pool-running-p pool) nil)
    (condition-notify (pool-cond pool)))
  (when await (mapc #'join-thread (workers pool))))

;;;; Convenience Macros ------------------------------------------------------
(defmacro with-thread-pool ((var size) &body body)
  `(let ((,var (make-thread-pool ,size)))
     (unwind-protect (progn ,@body) (shutdown-pool ,var))))

(defmacro with-dynamic-thread-pool ((var min &key max) &body body)
  `(let ((,var (make-dynamic-thread-pool ,min :max ,max)))
     (unwind-protect (progn ,@body) (shutdown-pool ,var))))

;;; Test function
(defun demo-static-pool ()
  "Compute the squares of 0–9 in parallel with a fixed-size pool of four workers.
Returns the results in order."
  (with-thread-pool (pool 4)
    ;; Launch ten tasks.
    (print
     (let ((futs (loop for i from 0 below 8
                       collect (submit-task pool
                                            (lambda ()
                                              ;; Simulate some work
                                              (sleep 0.5)
                                              (* i i))))))
       (mapcar #'get-future-value futs)))))

;; (demo-static-pool )
