;;;; unitstat-tests.asd

(asdf:defsystem #:unitstat-tests
  :version "1.0"
  :licence "MIT"
  :serial t
  :depends-on (#:unitstat #:cl-ppcre #:lisp-unit)
  :components ((:file "unitstat-tests")))

(defmethod perform ((o test-op) (c (eql (find-system :unitstat-tests))))
  (flet ((run-tests (&rest args)
           (apply (intern (string '#:run-all-tests) '#:unitstat-tests) args)))
    (run-tests :compiled nil)
    (run-tests :compiled t)))
