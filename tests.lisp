;; Copyright (c) 2010-2011 John H. Poplett.
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;

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

(define-test filter-test ()
	     (let ((alist (list "we" "the" "people")))
	       (assert-equal (-> (send-list alist) (filter #'(lambda (string) (not (search "the" string)))) (message-count)) 2)
	       (assert-equal (-> (send-list alist) (filter #'(lambda (string) (search "peach" string))) (message-count)) 0)
	       (assert-equal (-> (send-list alist) (null-filter) (message-count)) (length alist))
	       (assert-equal (-> (send-list alist) (filter #'(lambda (string) (search "the" string))) (message-count)) 1)))

  (define-test observer-test ()
	       (let ((counter-1 0)
		     (counter-2 0))
		 (assert-true (-> (send-list (list 1 2 3)) (observer #'(lambda (value) (incf counter-1))) (sink #'(lambda (value) (incf counter-2)))) (= 6 (+ counter-1 counter-2)))))

