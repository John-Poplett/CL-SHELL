;;;; cl-shell.lisp

(in-package #:cl-shell)

;;; "cl-shell" goes here. Hacks and glory await!

(defun ends-with (a b) 
  (let ((diff (- (length a) (length b)))) 
    (string= a b :start1 diff)))

(defun all-files-ending-in (directory suffix action) 
  (cl-fad:walk-directory directory action :test #'(lambda (file) (ends-with (namestring file) suffix))))

(defun all-java-files (directory action)
  (all-files-ending-in directory ".java" action))

(defun all-java-files-to-channel (directory output-channel)
  (all-java-files directory #'(lambda (file) (send output-channel file)))
  (send output-channel nil))

(defun test-all-java-files () 
  (all-java-files "/Users/john/Development/android/sdk" (lambda (file) (format t "~A~%" file))))

(defun count-java-files-1 (directory) 
  (let ((input-channel (make-instance 'channel)))
    (pexec (:name "java-files") (all-java-files-to-channel directory input-channel))
    (loop for i from 1
	 while (not (null (recv input-channel))) count t)))

(defgeneric producer (channel)
  (:documentation "Generate input for a pipeline"))
    
(defun find-files (directory &key (test (constantly t))) 
  (lambda (output-channel) (cl-fad:walk-directory directory #'(lambda (file) (send output-channel file)) :test test)
	  (send output-channel nil)))

(defun message-count (input-channel) 
  (loop for i from 1
	while (not (null (recv input-channel))) count t))

(defun filter (test)
  (lambda (input-channel output-channel)
    (do ((value (recv input-channel) (recv input-channel)))
      ((not (null value)) t)
      (if (funcall test value)
	  (send output-channel value)))
    (send output-channel nil)))

#|

(consumer (filter (producer)))


(defun combine-filters(filter1 filter2) 
  (lambda (input-channel output-channel) 
    (let ((binding-channel (make-instance 'channel)))
      (pexec (:name "combine-filter") (funcall filter1 input-channel binding-channel))
      (pexec (:name "combine-filter") (funcall filter2 binding-channel output-channel)))))

(defun combined-filter (filters) 
  (lambda (input-channel output-channel) 
    (let ((first-filter (car filters))
	  (mid-filters (butlast (cdr filters)))
	  (last-filter (car (last filters)))
	  (nfilters (length filters)))
      (cond ((= nfilters 0) nil)
	    ((= nfilters 1) (pexec (:name "filters") (funcall first-filter input-channel output-channel)))
	    ((= nfilters 2) (pexec (:name "filters") (funcall (combine-filters first-filter last-filter) input-channel output-channel)))))))
|#

(defun null-filter () 
  (filter (constantly t)))

(defun sink (input-channel handler)
  (do ((value (recv input-channel) (recv input-channel)))
      ((not (null value)) t)
    (funcall handler value)))

(defun standard-output-sink (input-channel) 
  (sink input-channel #'(lambda (value) (format t "~A~%" value))))

(defun two-stage-pipe (producer consumer) 
  (let ((input-channel (make-instance 'channel)))
    (pexec (:name "producer") (funcall producer input-channel))
    (funcall consumer input-channel)))

#|
(defmacro pipe (producer &rest args)
  (let ((input-channel (gensym))
	(nargs (length args)))
    `(let ((input-channel (make-instance 'channel)))
    (pexec (:name "producer") (funcall ,producer input-channel))

    (cond ((= 0 nargs) 
	   `

    (funcall ,consumer input-channel)))
|#

(defun pipe-test-1 (producer &rest args)
  (let* ((input-channel (gensym))
	 (consumer (car args)))
    `(let ((,input-channel (make-instance 'channel)))
       (pexec (:name "producer") (funcall ,producer ,input-channel))
       (funcall ,consumer ,input-channel))))


#|
(let ((current-channel (make-instance 'channel))
      (pexec funcall producer current-channel)
      (pexec funcall filter1 current-channel next-channel)
      (pexec funcall filter2 next-channel next-next-channel)
      (funcall consumer next-next-channel)

(funcall consumer (filter (filter (producer) (make-instance 'channel)) (make-instance 'channel)))


(defun pipe-test (producer &optional (consumer #'standard-output-sink) &rest filters)
  (let ((current-channel (gensym)))
    `(let ((,current-channel (make-instance 'channel)))
       (pexec (:name "producer") (funcall ,producer ,current-channel))
       ,@(loop for filter in `,filters collect
		 (let ((next-channel (gensym)))
		   `(let ((,next-channel (make-instance 'channel)))
		      (pexec (:name "filter") (funcall ,filter ,current-channel ,next-channel))
		      (setf ,current-channel ,next-channel))))
       (funcall ,consumer ,current-channel))))
|#

(defun do-filters (filters producer)
  (let ((current-channel (gensym)))
    (cond ((null filters)
	   `(let ((,current-channel (make-instance 'channel)))
	      (pexec (:none "producer") (funcall ,producer ,current-channel))
	      ,current-channel))
	  (t 
	   `(let ((,current-channel (make-instance 'channel)))
	      (pexec (:none "filter") (funcall ,(car filters) ,(do-filters (cdr filters) producer) ,current-channel))
	      ,current-channel)))))

(defun pipe-test (producer &optional (consumer #'standard-output-sink) &rest filters)
  `(,consumer ,(do-filters (reverse filters) producer)))


(defun count-files (directory &key (test (constantly t)))
  (two-stage-pipe (find-files directory :test test) #'message-count))

(defun count-files-with-suffix (directory suffix)
  (count-files directory :test #'(lambda (file) (ends-with (namestring file) suffix))))

(defun count-java-files (directory)
  (count-files-with-suffix directory ".java"))

;    (= (search seq1 seq2 :from-end t) diff)))

