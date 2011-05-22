(in-package #:cl-shell)

(defun mkstr (&rest args)
   (with-output-to-string (s)
	(dolist (a args) (princ a s))))

(defun symb (&rest args)
   (values (intern (apply #'mkstr args) :cl-shell)))
