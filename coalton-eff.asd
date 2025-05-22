(defsystem "coalton-eff"
  :long-name "coalton-effects"
  :version "0.0.1"
  :author "Jason Walker"
  :maintainer "Jason Walker"
  :mailto "jason0@pm.me"
  :license ""
  :depends-on ("bt"
               "coalton")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "A typesafe effect system in Common Lisp Coalton."
  :in-order-to ((test-op (test-op "coalton-eff/tests"))))

(defsystem "coalton-eff/tests"
  :author "Jason Walker"
  :license ""
  :depends-on ("coalton-eff"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for coalton-eff"
  :perform (test-op (op c) (symbol-call :rove :run c)))
