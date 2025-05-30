(defpackage #:coalton-eff/pool-coalton
  (:use #:coalton #:coalton-prelude
        #:private-coalton.io/monad)
  (:local-nicknames
   (#:p #:coalton-eff/pool))
  (:export
   #:ThreadPool
   #:make-thread-pool
   #:submit-task
   #:shutdown-pool))
(in-package #:coalton-eff/pool-coalton)

(coalton-toplevel
  (repr :native p:future)
  (define-type (Future :a))

  (declare get-future-value (Future :a -> :a))
  (define (get-future-value fut)
    (lisp :a (fut)
      (p:get-future-value fut)))

  (repr :native p:thread-pool)
  (define-type ThreadPool)

  (declare make-thread-pool (Integer -> ThreadPool))
  (define (make-thread-pool workers)
    (lisp ThreadPool (workers)
      (p:make-thread-pool workers)))

  ;; Note: Should this take a Unit -> IO Unit?? Or maybe just an IO Unit?
  (declare submit-task (ThreadPool -> (Unit -> :a) -> IO (Future :a)))
  (define (submit-task pool task)
    (IO (fn ()
          (lisp :a (pool task)
            (p:submit-task pool (cl:lambda () (coalton ((lisp (Unit -> :a) ()
                                                         task)))))))))

  (declare shutdown-pool (ThreadPool -> Unit))
  (define (shutdown-pool pool)
    (lisp :a (pool)
      (p:shutdown-pool pool))
    Unit))
