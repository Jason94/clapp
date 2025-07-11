(defpackage #:coalton-eff/main
  (:use #:coalton #:coalton-prelude
        #:private-coalton.io/monad
        #:private-coalton.io/term)
  (:local-nicknames
   (#:l #:coalton-library/list)
   (#:pl #:coalton-eff/pool-coalton)
   (#:f #:coalton-library/monad/free)
   (#:ft #:coalton-library/monad/freet)))
(in-package #:coalton-eff/main)

(coalton-toplevel
  (declare push-end (:a -> List :a -> List :a))
  (define (push-end a lst)
    (l:reverse (Cons a (l:reverse lst)))))

(coalton-toplevel
  (define-type (SpawnF :next)
    (Fork% :next :next)
    (Yield% :next)
    Done%)

  (define-instance (Functor SpawnF)
    (define (map f ast)
      (match ast
        ((Fork% a b) (Fork% (f a) (f b)))
        ((Yield% a) (Yield% (f a)))
        ((Done%) Done%))))

  (define-type-alias Spawn (ft:FreeT SpawnF)))

(coalton-toplevel

  (declare yield (Monad :m => Spawn :m Unit))
  (define yield (f:liftF (Yield% Unit)))

  (declare done (Monad :m => Spawn :m :r))
  (define done (f:liftF Done%))

  (declare c-fork (Monad :m => Spawn :m Boolean))
  (define c-fork (f:liftF (Fork% False True)))

  (declare fork (Monad :m => Spawn :m :a -> Spawn :m Unit))
  (define (fork thread)
    (do
     (child <- c-fork)
     (if child
       (do
        thread
        done)
       (pure Unit)))))

(coalton-toplevel
  (define (run threads)
    (match threads
      ((Nil) (pure Unit))
      ((Cons t ts)
       (do
        ;; Run this thread's effects
        (x <- (ft:run-freeT t))
        (match x
          ((ft:FreeF (Fork% t1 t2))
           (run (push-end t2 (Cons t1 ts))))
          ((ft:FreeF (Yield% t-next))
           (run (push-end t-next ts)))
          ((ft:FreeF (Done%))
           (run ts))
          ((ft:Val _)
           (run ts)))))))

  (declare round-robin (Monad :m => Spawn :m :a -> :m Unit))
  (define (round-robin t)
    (run (make-list t))))

(coalton-toplevel
  (define (run-in-par pool t)
    (do
      (x <- (ft:run-freeT t))
      (match x
        ((ft:FreeF (Fork% t1 t2))
         (do
          (pl:submit-task pool
                          (fn ()
                            ;; TODO: This should definitely *not* call run! here
                            (run! (run-in-par pool t2))
                            Unit))
          (run-in-par pool t1)))
        ((ft:FreeF (Yield% t-next))
         (run-in-par pool t-next))
        ;; TODO: learn how to stop
        ((ft:FReeF (Done%))
         (pure Unit))
        ((ft:Val _)
         (pure Unit)))))

  (declare run-in-pool (Integer -> Spawn IO :a -> IO Unit))
  (define (run-in-pool workers t)
    (let ((pool (pl:make-thread-pool workers)))
      (run-in-par pool t))))

(coalton-toplevel
  (declare thread1 (Integer -> Spawn IO Unit))
  (define (thread1 x)
    (do
     (lift (write-line x))
     yield))

  (declare thread2 (Spawn IO Unit))
  (define thread2
    (do
     (lift (write-line "Hello from thread 2"))
     (fork (thread1 5))
     ;; yield
     (lift (write-line "Thread 2 still going!"))))

  (declare main-thread (Spawn IO Unit))
  (define main-thread
    (do
     (lift (write-line "Forking thread #1"))
     yield
     (fork (thread1 10))
     (lift (write-line "Forking thread #2"))
     (lift (sleep 2))
     (fork thread2)
     (fork thread2)
     yield
     (lift (write-line "Forking thread #3"))))
  )

(coalton (run! (round-robin main-thread)))
(coalton (run! (run-in-pool 4 main-thread)))

(coalton (run! (run-in-pool 4 thread2)))
