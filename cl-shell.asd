;;;; cl-shell.asd

(asdf:defsystem #:cl-shell
  :serial t
  :depends-on (#:bordeaux-threads
               #:cl-fad
	       #:chanl
               #:lisp-unit)
  :components ((:file "package")
               (:file "cl-shell")))

