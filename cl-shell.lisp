;;; cl-shell.lisp

(in-package #:cl-shell)

;;; "cl-shell" goes here. Hacks and glory await!

(defun ends-with (a b) 
  "Utility predicate function used in filters."
  (let ((diff (- (length a) (length b)))) 
    (string= a b :start1 diff)))

(defun find-files (directory &key (test (constantly t)))
  "Create a producer lambda expression for finding files that meet certain criteria."
  (lambda (output-channel) (cl-fad:walk-directory directory #'(lambda (file) (send output-channel file)) :test test)
	  (send output-channel nil)))

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
  (lambda (input-channel output-channel)
    (do ((value (recv input-channel) (recv input-channel)))
	((null value) t)
      (if (funcall test value)
	  (send output-channel value)))
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

(defmacro -> (producer &rest args)
  "Create a pipeline from threads and channels to process."
  (let ((consumer (gensym)))
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

(defun count-files (directory &key (test (constantly t)))
  (-> (find-files directory :test test) (message-count)))

(defun hello-world () 
  "The pipeline version of the hello world program."
  (-> (send-list '("hello world!"))))

