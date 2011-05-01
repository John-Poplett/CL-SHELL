(defsystem cl-shell-tests
  :depends-on (:cl-shell :lisp-unit)
  :components ((:file "tests")))

(defmethod operation-done-p
    ((o test-op) (c (eql (find-system :cl-shell-tests))))
  nil)

(defmethod perform ((o test-op) (c (eql (find-system :cl-shell-tests))))
  (flet ((run-tests (&rest args)
           (apply (intern (string '#:run-tests) '#:cl-shell-tests) args)))
    (run-tests :compiled nil)
    (run-tests :compiled t)))
