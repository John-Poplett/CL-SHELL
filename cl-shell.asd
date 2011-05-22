;;;; cl-shell.asd

(asdf:defsystem #:cl-shell
  :version "1.0"
  :licence "MIT"
  :serial t
  :depends-on (#:bordeaux-threads
               #:cl-fad
	       #:chanl)
  :components ((:static-file "README.txt")
               (:static-file "tests.lisp")
               (:file "package")
	       (:file "macro-helper")
               (:file "cl-shell")))

(defmethod operation-done-p ((o test-op) (c (eql (find-system :cl-shell))))
  nil)

(defmethod perform ((o test-op) (c (eql (find-system :cl-shell))))
  (operate 'load-op :cl-shell-tests)
  (operate 'test-op :cl-shell-tests))

