(defpackage coalton-eff/tests/pool
  (:use :cl
        :coalton-eff/pool
        :rove))
(in-package :coalton-eff/tests/pool)

(deftest fixed-single-task
  (with-thread-pool (pool 2)
    (let ((f (submit-task pool (lambda () 42))))
      (ok (= 42 (future-value f)))))

(deftest fixed-multiple-tasks
  (with-thread-pool (pool 2)
    (let* ((futs (loop for i from 1 to 10 collect (submit-task pool (lambda () (* i i)))))
           (res  (mapcar #'future-value futs)))
      (ok (equal res '(1 4 9 16 25 36 49 64 81 100)))))

(deftest fixed-error-propagation
  (with-thread-pool (pool 2)
    (let ((f (submit-task pool (lambda () (error "Boom")))))
      (ok (typep (future-value f) 'error))))

(deftest dynamic-grow-cap
  (with-dynamic-thread-pool (dpool 1 :max 4)
    (let* ((futs (loop repeat 8 collect (submit-task dpool (lambda () (sleep 0.02) t)))))
      (mapc #'future-value futs)
      (ok (<= (length (workers dpool)) 4))))

(deftest dynamic-shrink
  (with-dynamic-thread-pool (dpool 1 :max 4)
    (let* ((futs (loop repeat 4 collect (submit-task dpool (lambda () t)))))
      (mapc #'future-value futs))
    (sleep 0.1)
    (ok (= (length (workers dpool)) 1)))

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
