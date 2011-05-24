;;; hello-world.lisp

(in-package #:cl-shell)

(defun hello-world () 
  "The pipeline version of the hello world program."
  (-> (send-list '("hello world!"))))

