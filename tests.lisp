(in-package :cl-user)

(defpackage :cl-shell-tests
  (:use :cl :cl-shell :lisp-unit))

(in-package :cl-shell-tests)

(define-test simple-test ()
	     (let ((last-value nil))
	       (-> (send-list '("hello world!" "goodbye, world")) (sink #'(lambda (value) (setf last-value value))))
	       (assert-true (string= "goodbye, world" last-value))))

(define-test okay ()
	     (assert-true t))

(define-test message-count-test ()
	     (assert-equal (-> (send-list (list 1 2 3 4 5)) (message-count)) 5))


