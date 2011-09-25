;;; cl-shell.lisp
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


(in-package #:cl-shell)

;;; "cl-shell" goes here. Hacks and glory await!

(defun find-files (directory &key (test (constantly t)))
  "Create a producer lambda expression for finding files that meet certain criteria."
  (lambda (output-channel) (cl-fad:walk-directory directory #'(lambda (file) (send output-channel file)) :test test)
	  (send output-channel nil)))

(defun process (process args)
  "Create a producer lambda expression for sending the output of a process."
  (lambda (output-channel)
    (let ((process (sb-ext:run-program process args :output :stream :wait nil)))
      (unwind-protect
	   (with-open-stream (input (sb-ext:process-output process))
	     (do ((line (read-line input nil)
		    (read-line input nil)))
		 ((null line))
	       (send output-channel line)))
	(send output-channel nil)
	(sb-ext:process-close process)))))

(defun send-list (list &key (test (constantly t)))
  "Create a producer lambda expression for sending the elements of a list."
  (lambda (output-channel) (dolist (ele list) (if (funcall test ele) (send output-channel ele)))
	  (send output-channel nil)))

(defun message-count ()
  "Create a consumer lambda expression that counts the number of messages it recieves."
  (lambda (input-channel) 
    (loop for i from 1
       while (not (null (recv input-channel))) count t)))

(defun filter (test)
  "Filter out elements from the stream based on the results of the test function."
  (lambda (input-channel output-channel)
    (do ((value (recv input-channel) (recv input-channel)))
	((null value) t)
      (if (funcall test value)
	  (send output-channel value)))
    (send output-channel nil)))

(defun transformer (transform-function)
  "Apply transform-function to each element in the stream."
  (lambda (input-channel output-channel)
    (do ((value (recv input-channel) (recv input-channel)))
	((null value) t)
      (send output-channel (funcall transform-function value)))
    (send output-channel nil)))

(defun file-pumper ()
  (lambda (input-channel output-channel)
    (do ((filename (recv input-channel) (recv input-channel)))
	((null filename) t)
      (with-open-file (stream filename :external-format :iso-8859-1) ; FIXME - external-format hack for Mac OS X
	(do ((line (read-line stream nil)
	       (read-line stream nil)))
	    ((null line))
	  (send output-channel line))))
    (send output-channel nil)))

(defun observer (handler)
  "A filter that observes elements that flow across the stream."
  (lambda (input-channel output-channel)
    (do ((value (recv input-channel) (recv input-channel)))
	((null value) t)
      (funcall handler value)
      (send output-channel value))
    (send output-channel nil)))

(defun null-filter () 
  (filter (constantly t)))

(defun sink (handler)
  (lambda (input-channel)
    (do ((value (recv input-channel) (recv input-channel)))
	((null value) t)
      (funcall handler value))))

(defun standard-output-sink () 
  (sink #'(lambda (value) (format t "~A~%" value))))

(defun make-channel () 
  (make-instance 'channel))

#|
; Prototype of plumbing 
(defun two-stage-pipe (producer consumer) 
  (let ((input-channel (make-instance 'channel)))
    (pexec (:name "producer") (funcall producer input-channel))
    (funcall consumer input-channel)))

(defun three-stage-pipe (producer filter consumer) 
  (let ((channel1 (make-instance 'channel))
	(channel2 (make-instance 'channel)))
    (pexec (:name "producer") (funcall producer channel1))
    (pexec (:name "filter") (funcall filter channel1 channel2))
    (funcall consumer channel2)))
|#

(defmacro channel-symbol (n)
  `(symb 'channel (write-to-string ,n)))

#|
(defmacro channel-symbol (n)
  (labels ((mkstr (&rest args)
	     (with-output-to-string (s)
	       (dolist (a args) (princ a s))))
	   (symb (&rest args)
	     (values (intern (apply #'mkstr args) :cl-shell))))
    `(symb 'channel (write-to-string ,n))))
|#

(defmacro -> (producer &rest newargs)
  "Create a pipeline from threads and channels to process."
  (let ((consumer (gensym))
	(args (remove #'null-filter newargs))) ; optimization 
    (labels ((consumer-helper (args)
	       (cond ((null args)
		      '(standard-output-sink))
		     (t `(,@(car (last args))))))
	     (filter-gen ()
	       (let ((counter 1))
		 (lambda (filter) 
		   (let ((old-counter counter))
		     (setf counter (+ 1 counter))
		     `(pexec (:name "filter") (funcall ,filter ,(channel-symbol old-counter) ,(channel-symbol counter)))))))
	     (pipe-builder (producer args consumer)
	       `(let ((,consumer ,(consumer-helper args))
		      (cl-shell::channel1 (make-instance 'channel))
		      ,@(loop for i from 2 to (length args)
			   collect `(,(channel-symbol i) (make-instance 'channel))))
		  (pexec (:name "producer") (funcall ,producer cl-shell::channel1))
		  ,@(mapcar (filter-gen) (butlast args))
		  (funcall ,consumer ,(channel-symbol (max 1 (length args)))))))
      `(,@(pipe-builder producer args consumer)))))

