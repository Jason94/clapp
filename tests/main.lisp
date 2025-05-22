(defpackage coalton-eff/tests/main
  (:use :cl
        :coalton-eff
        :rove))
(in-package :coalton-eff/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :coalton-eff)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
