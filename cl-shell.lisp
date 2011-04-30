;;;; cl-shell.lisp

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

(defun null-filter () 
  (filter (constantly t)))

(defun sink (handler)
  (lambda (input-channel)
    (do ((value (recv input-channel) (recv input-channel)))
	((null value) t)
      (funcall handler value))))

(defun standard-output-sink () 
  (sink #'(lambda (value) (format t "~A~%" value))))

(defun two-stage-pipe (producer consumer) 
  (let ((input-channel (make-instance 'channel)))
    (pexec (:name "producer") (funcall producer input-channel))
    (funcall consumer input-channel)))

(defun make-channel () 
  (make-instance 'channel))

(defun do-filters (filters producer)
  (let ((current-channel (gensym)))
    (cond ((null filters)
	   `(let ((,current-channel (make-channel)))
	      (pexec (:name "producer") (funcall ,producer ,current-channel))
	      ,current-channel))
	  (t 
	   `(let ((,current-channel (make-instance 'channel)))
	      (pexec (:name "filter") (funcall ,(car filters) ,(do-filters (cdr filters) producer) ,current-channel))
	      ,current-channel)))))

(defun pipe-test (producer &optional (consumer (standard-output-sink)) &rest filters)
  `(,consumer ,(do-filters (reverse filters) producer)))

(defmacro pipe (producer consumer &rest filters)
    `(,consumer ,(do-filters (reverse filters) producer)))

(defmacro poop (producer filters consumer) 
  `(format t "~A <-> ~{A <-> ~}~A~%" #',producer ,filters #',consumer))

(defun count-files (directory &key (test (constantly t)))
  (two-stage-pipe (find-files directory :test test) (message-count)))

(defun count-files-with-suffix (directory suffix)
  (count-files directory :test #'(lambda (file) (ends-with (namestring file) suffix))))

(defun count-java-files (directory)
  (count-files-with-suffix directory ".java"))

(defun three-stage-pipe (producer filter consumer) 
  (let ((channel1 (make-instance 'channel))
	(channel2 (make-instance 'channel)))
    (pexec (:name "producer") (funcall producer channel1))
    (pexec (:name "filter") (funcall filter channel1 channel2))
    (funcall consumer channel2)))

(defun pp-helper (args)
  (cond ((null args)
	 '(standard-output-sink))
	(t `(,@(car (last args))))))

(defun make-pipes (producer &rest args)
  (let ((consumer (pp-helper args))
	(filters (butlast args)))
    `(let ((channel1 (make-instance 'channel))
	   (channel2 (make-instance 'channel)))
       (pexec (:name "producer") (funcall producer channel1))
       (pexec (:name "filter") (funcall filter channel1 channel2))
       (funcall consumer channel2))))
    
(defmacro pp (producer &rest args)
  (let ((input-channel (gensym))
	(output-channel (gensym))
	(producer-instance (gensym))
	(consumer-instance (gensym)))
    `(let* ((,input-channel (make-instance 'channel))
	    (,output-channel ,input-channel)
	    (,producer-instance ,producer)
	    (,consumer-instance ,(pp-helper args)))
       (pexec (:name "producer") (funcall ,producer-instance ,input-channel))
       (funcall ,consumer-instance ,output-channel)
       (format t "producer: ~A~%args: ~A~%" ,producer ,(pp-helper args)))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun symb-ref%% ()
  (sym 'a 'b 'c))

(defmacro channel-symbol (n)
  `(symb 'cl-shell::channel (write-to-string ,n)))

(defun z () 
  (let* ((path "/Users/john/Development/android/sdk/samples/android-9/JetBoy")
	 (producer `(find-files ,path))
	 (args '((null-filter) (message-count))))
    `(let ((channel1 (make-instance 'channel))
      ,@(loop for i from 2 to (+ 1 (length args))
	     collect `(,(symb 'channel (write-to-string i)) (make-instance 'channel)))))))

(defmacro p% (producer &rest args) 
  (alexandria:with-gensyms (consumer filters)
    `(let ((,consumer ,(pp-helper args))
	   (,filters '(,@(butlast args)))
	   (channel1 (make-instance 'channel))
	   ,@(loop for i from 2 to (+ 1 (length args))
		collect `(,(symb 'channel (write-to-string i)) (make-instance 'channel))))
       (pexec (:name "producer") (funcall ,producer channel1))
       ;(pexec (:name "filter") (funcall ,(car filters) channel1 channel2))
       (funcall ,consumer channel2)
       (format t "~A~%" ,filters))))

(defun z% ()
 (let* ((path "/Users/john/Development/android/sdk/samples/android-9/JetBoy"))
   (pprint (macroexpand-1 (p% (find-files path) (null-filter) (message-count))))))

(defun filter-gen ()
  (let ((counter 1))
    (lambda (filter) 
      (let ((old-counter counter))
	(setf counter (+ 1 counter))
	`(pexec (:name "filter") (funcall ,filter ,(channel-symbol old-counter) ,(channel-symbol counter)))))))

(defun p-helper%% (producer args consumer filters)
    `(let ((,consumer ,(pp-helper args))
	   (,filters '(,@(butlast args)))
	   (channel1 (make-instance 'channel))
	   ,@(loop for i from 2 to (length args)
		collect `(,(symb 'cl-shell::channel (write-to-string i)) (make-instance 'channel))))
       (pexec (:name "producer") (funcall ,producer channel1))
       ;,@(mapcar (filter-gen) (butlast args))
       ;(pexec (:name "filter") (funcall ,(car filters) channel1 channel2))
       (funcall ,consumer channel2)))

(defun foo%% ()
  (p-helper%% 'a 'b 'c 'd))

(defmacro p%% (producer &rest args) 
  (alexandria:with-gensyms (consumer filters)
    `(,@(p-helper%% producer args consumer filters))))

(defun z%% ()
 (let* ((path "/Users/john/Development/android/sdk/samples/android-9/JetBoy"))
   (p-helper%% `(find-files ,path) '((null-filter) (message-count)) (gensym) (gensym))))
   ;(pprint (macroexpand-1 (p%% (find-files path) (null-filter) (message-count))))))

;(z%%)