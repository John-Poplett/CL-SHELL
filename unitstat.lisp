(in-package :cl-user)

(defpackage :unitstat
  (:use :cl :cl-fad :cl-ppcre :cl-shell :local-time :statistics)
  (:export
   #:all-files
   #:test-files
   #:lines-of-code
   #:lines-of-test-code
   #:kloc
   #:test-kloc
   #:kloc-ratio
   #:number-of-classes
   #:number-of-abstract-classes
   #:number-of-asserts
   #:file-ratio
   #:abstraction
   #:assert-ratio
   #:to-list
   #:analyze
   #:source-file-pathname-p
   #:test-file-pathname-p))

(in-package :unitstat)

(defclass analysis ()
  ((name :initarg :name :initform "unknown")
   (creation-date-time :initarg :creation-date-time :initform (get-universal-time) :reader creation-date-time)
   (directory :initarg :directory)
   (all-files :initform 0 :accessor all-files)
   (test-files :initform 0 :accessor test-files)
   (lines-of-code :initform 0 :accessor lines-of-code)
   (lines-of-test-code :initform 0 :accessor lines-of-test-code)
   (kloc :initform 0 :reader kloc)
   (test-kloc :initform 0 :reader test-kloc)
   (kloc-ratio :initform 0 :reader kloc-ratio)
   (number-of-classes :initform 0 :accessor number-of-classes)
   (number-of-abstract-classes :initform 0 :accessor number-of-abstract-classes)
   (number-of-asserts :initform 0 :accessor number-of-asserts)
   (file-ratio :initform 0 :reader file-ratio)
   (abstraction :initform 0 :reader abstraction)
   (assert-ratio :initform 0 :reader assert-ratio)
   (to-list :initform nil :reader to-list)))

(defmacro div (q d)
  "Division that avoids divide by zero exceptions."
  `(if (= ,d 0) 0 (/ ,q ,d)))

(defmacro roundif (a q d)
  "Only round when the quotient"a" is greater than the divisor."
  `(if (> ,a ,d) (round ,q ,d) (div ,q ,d)))

(defmacro pct (q d)
  `(if (= ,d 0) 0 (round (* 100 ,q) ,d)))

(defmethod to-list ((object analysis))
  (with-slots (name directory all-files test-files number-of-asserts) object
      (list name directory all-files test-files (file-ratio object) (kloc object) (test-kloc object) (kloc-ratio object) (abstraction object) number-of-asserts (assert-ratio object))))

(defmethod file-ratio ((o analysis))
  (with-slots (all-files test-files) o
      (div test-files all-files)))

(defmethod kloc ((o analysis))
  (with-slots (lines-of-code) o
    (roundif lines-of-code lines-of-code 1000)))

(defmethod test-kloc ((o analysis))
  (with-slots (lines-of-code lines-of-test-code) o
    (roundif lines-of-code lines-of-test-code 1000)))

(defmethod kloc-ratio ((o analysis))
    (div (test-kloc o) (kloc o)))

(defmethod abstraction ((o analysis))
  (with-slots (number-of-classes number-of-abstract-classes) o
    (div number-of-abstract-classes number-of-classes)))

(defmethod assert-ratio ((o analysis))
  (with-slots (lines-of-code number-of-asserts) o
    (div number-of-asserts lines-of-code)))

(defmethod print-object ((object analysis) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name directory all-files test-files number-of-asserts) object
      (format stream "~A ~A ~A ~A ~2,2f ~A ~A ~2,2f ~2,2f ~A ~2,2f" name directory all-files test-files (* 100 (file-ratio object)) (kloc object) (test-kloc object) (* 100 (kloc-ratio object)) (* 100 (abstraction object)) number-of-asserts (* 100 (assert-ratio object))))))

(defun c-file-pathname-p (file)
  (ends-with (namestring file) ".c" ".h"))

(defun c-plus-plus-file-pathname-p (file)
  (ends-with (namestring file) ".cpp" ".hpp"))

(defun java-file-pathname-p (file)
  (ends-with (namestring file) ".java"))

(defun lisp-file-pathname-p (file)
  (ends-with (namestring file) ".lisp"))

(defun java-test-file-pathname-p (file)
  (ends-with (namestring file) "Test.java"))

(defun source-file-pathname-p (file) 
  (ends-with (namestring file) ".c" ".h" ".cpp" ".hpp" ".java" ".lisp" ".pl" ".pm"))

(defun test-file-pathname-p (file)
  "Report whether the given pathname is a programming language test file."
  (let ((file-name (namestring file)))
    (and (source-file-pathname-p file) (cl-ppcre:scan "[Tt]est" file-name))))

(defun standard-class-p (line)
  "Identify all classes and interfaces"
  (not (null (scan "public.*(interface|class)" line))))

(defun abstract-class-p (line)
  "Identify interfaces and abstract classes."
  (not (null (scan "public.*(interface|abstract.*class)" line))))

(defun assert-p (line)
  "Identify some common assert statements."
  (not (null (scan "assert|ASSERT|wtf" line))))

(defun git-filter ()
  "Returns lambda expression that filters out paths to .git repositories."
  (lambda (file) (not (search ".git/" (namestring file)))))

(defmacro force-list (element)
  `(if (listp ,element)
       ,element
       (list ,element)))

(defmethod analyz ((analysis-instance analysis) &key (filter (git-filter)))
  "Analyze source-code for a given set of directories and report the findings."
  (let ((directories (force-list (slot-value analysis-instance 'directory)))
	(test-file-p nil))
    (labels ((handle-java-line (line)
	       (if (standard-class-p line)
		   (incf (number-of-classes analysis-instance)))
	       (if (abstract-class-p line)
		   (incf (number-of-abstract-classes analysis-instance)))
	       (if (and (not test-file-p) (assert-p line))
		   (incf (number-of-asserts analysis-instance))))
	     (handle-lisp-line (line)
	       (if (scan "\\(defclass " line)
		   (incf (number-of-classes analysis-instance)))
	       (if (and (not test-file-p) (scan "\\(assert " line))
		   (incf (number-of-asserts analysis-instance))))
	     (default-handle-line (line)
	       (declare (ignore line)))
	     (handle-file (file)
	       (setf test-file-p (test-file-pathname-p file))
	       (incf (all-files analysis-instance))
	       (if test-file-p
		   (incf (test-files analysis-instance)))
	       (let ((handle-line (cond ((java-file-pathname-p file) #'handle-java-line)
					((lisp-file-pathname-p file) #'handle-lisp-line)
					(t #'default-handle-line))))
		 (with-open-file (stream file :external-format :iso-8859-1)
		   (do ((line (read-line stream nil)
			      (read-line stream nil)))
		       ((null line))
		     (funcall handle-line line)
		     (if test-file-p
			 (incf (lines-of-test-code analysis-instance)))
		     (incf (lines-of-code analysis-instance)))))))
      (dolist (directory directories)
	(unless (directory-exists-p directory) (error "Directory ~A does not exist!" directory))
	(cl-fad:walk-directory (merge-pathnames directory) #'handle-file :test #'(lambda (file) (and (source-file-pathname-p file) (funcall filter file)))))))
  analysis-instance)

#|
(defun analyze-sequential (name directory &key (filter (git-filter)))
  "Analyze source-code for a given directory and report the findings."
  (let ((analysis-instance (make-instance 'analysis :name name :directory directory))
	(java-file-p nil)
	(test-file-p nil))
    (unless (directory-exists-p directory) (error "Directory does not exist!"))
    (labels ((handle-file (file)
	       (setf test-file-p (test-file-pathname-p file))
	       (setf java-file-p (java-file-pathname-p file))
	       (incf (all-files analysis-instance))
	       (if test-file-p
		   (incf (test-files analysis-instance)))
	       (with-open-file (stream file :external-format :iso-8859-1)
		 (do ((line (read-line stream nil)
			    (read-line stream nil)))
		     ((null line))
		   (if java-file-p
		       (if (standard-class-p line)
			   (incf (number-of-classes analysis-instance)))
		       (if (abstract-class-p line)
			   (incf (number-of-abstract-classes analysis-instance))))
		   (if test-file-p
		       (incf (lines-of-test-code analysis-instance))
		       (if (assert-p line)
			   (incf (number-of-asserts analysis-instance))))
		   (incf (lines-of-code analysis-instance))))))
      (cl-fad:walk-directory directory #'handle-file :test #'(lambda (file) (and (source-file-pathname-p file) (funcall filter file)))))
    analysis-instance))

(defun analyze-parallel (name directory &key (filter-function (git-filter)))
  "Analyze source-code for a given directory and report the findings."
  (let ((analysis-instance (make-instance 'analysis :name name :directory directory))
	(java-file-p nil)
	(test-file-p nil))
    (unless (directory-exists-p directory) (error "Directory does not exist!"))
    (->
     (find-files directory :test #'source-file-pathname-p)
     (filter filter-function)
     (observer #'(lambda (file)
		   (if (test-file-pathname-p file)
		       (progn (incf (test-files analysis-instance)) (setf test-file-p t))
		       (setf test-file-p nil))
		   (setf java-file-p (java-file-pathname-p file))
		   (incf (all-files analysis-instance))))
     (file-pumper)
     (sink #'(lambda (line)
	       (if java-file-p
		   (if (standard-class-p line)
		       (incf (number-of-classes analysis-instance)))
		   (if (abstract-class-p line)
		       (incf (number-of-abstract-classes analysis-instance))))
	       (if test-file-p
		   (incf (lines-of-test-code analysis-instance)))
	       (incf (lines-of-code analysis-instance)))))
    analysis-instance))
|#

(defun analyze (name directory &key (filter (git-filter)))
  (let ((analysis-instance (make-instance 'analysis :name name :directory directory)))
    (analyz analysis-instance :filter filter)
    analysis-instance))

(defun ltrim (string limit) 
  (if (> (length string) limit)
      (subseq string (- (length string) limit))
      string))

(defun println (name directory files testFiles fileRatio kloc testKloc klocRatio abstraction)
  (format t "~15A ~30A ~15A ~15A ~10A ~15A ~15A ~10A ~10A~%" (ltrim name 15) (ltrim directory 30) files testFiles fileRatio kloc testKloc klocRatio abstraction))

(defun default-header (name)
  (format t "Analysis for ~A~%" name)
  (println "Name" "Directory" "Source Files" "Test Files" "Ratio" "KLOC" "Test KLOC" "Ratio" "Abstraction"))

(defun basename (path-string)
  (let ((index (search "/" path-string :from-end t)))
    (if (and (not (null index)) (> index 0))
	(subseq path-string (+ 1 index))
	path-string)))

(defun default-analysis-handler (analysis)
  (apply #'println analysis))

(defmacro with-chdir (dir &body body)
  "Logically and physically change file system directories."
  (let ((cwd (gensym)))
    `(let ((,cwd (truename ".")))
       (unwind-protect 
	    (progn 
	      (unless (= (sb-posix:chdir ,dir) 0) (error "chdir failed!"))
	      (let ((*default-pathname-defaults* (pathname ,dir)))
		,@body))
	 (sb-posix:chdir ,cwd)))))

(defun analyze-repo (&key (dir (if (directory-exists-p "/home/john/endive/") "/home/john/endive/" "/Users/john/Development/android/master/"))
		     (repo-program-path (if (file-exists-p "/home/john/bin/repo") "/home/john/bin/repo" "/Users/john/bin/repo"))
		     (filters "bionic|blur|frameworks|kernel|motorola|packages")
		     (analysis-handler #'default-analysis-handler)
		     (header #'default-header))
  "Iterate over contents of a repo and invoke a handler with the name and path of each git repo."
  (funcall header dir)
  (with-chdir dir
    (-> 
     (process repo-program-path '("list"))
     (transformer #'(lambda (line) (subseq line 0 (search " " line))))
     (filter #'(lambda (line) (cl-ppcre:scan filters line)))
     (sink #'(lambda (line) (funcall analysis-handler (analyze (basename line) (merge-pathnames line))))))))

(defun analyze-repo-sexp ()
  (let ((results nil))
    (analyze-repo :header #'(lambda (dir) (declare (ignore dir))) :analysis-handler #'(lambda (analysis) (push analysis results)))
    results))

(defun report-prologue (&optional (stream nil))
  (format stream "<html>
  <head>
    <script type=\"text/javascript\" src=\"https://www.google.com/jsapi\"></script>
    <script type=\"text/javascript\">
      google.load(\"visualization\", \"1\", {packages:[\"corechart\"]});
      google.setOnLoadCallback(drawChart);
      function drawChart() {
"))

(defun js-value (value)
  (cond ((stringp value) (concatenate 'string "'" value "'"))
	(t value)))

(defun data-table (name header rows &key (stream nil))
  (format stream "~A = google.visualization.arrayToDataTable([" name)
  (dolist (row (cons header rows))
    (format stream "~%[~{~A~^, ~}]" (mapcar #'js-value row)))
  (format stream "], false);~%"))

(defun default-formatter (data-rows &key (stream nil))
  (dolist (row data-rows)
    (format stream "~A~%" row)))

(defvar *last-analyses*)

(defun gen-report (report-name list-to-analyze &key (formatter #'default-formatter) (stream nil))
  (declare (ignore report-name))
  (let ((analyses (mapcar #'(lambda (list) (apply #'unitstat:analyze list)) list-to-analyze)))
    (setf *last-analyses* analyses)
    (funcall formatter analyses :stream stream)))

(defun show-component-info (report-name components &key (stream t))
  (format stream "~A~%" report-name)
  (dolist (component components)
    (let ((component-name (first component))
	  (directories (second component)))
      (format stream "~A: ~{~A~^, ~}~%" component-name directories))))

(defun filter-motorola (file)
 (not (scan "bsp/test/linuxos/(ltp_full|lmbench)|.git/" (namestring file))))

(defun companies () 
    (gen-report "Companies" '(
			      ("Google" ("stingray/frameworks" "stingray/packages" "stingray/system" "stingray/bionic" "stingray/dalvik" "stingray/libcore"))
			      ; endive/motorola/system is omitted since it contains mostly hardware tests (e.g. memtest)
			      ("Motorola" ("endive/motorola/bsp" "endive/motorola/packages" "endive/motorola/frameworks" "endive/motorola/modem" "endive/motorola/hal" "endive/motorola/security") :filter filter-motorola)
			      ("Qualcomm" ("8960/device/qcom" "8960/system/qcom" "8960/hardware/qcom" "8960/vendor/qcom"))
			      ("Apache" ("httpd"))
			      ("Radvision" ("endive/vendor/radvision"))
			      ("SQLite" ("sqlite"))
			      ) :stream t))

(defun filter-motorola-modem (file)
 "Filter for motorola modem directory. Filter out cxxtest unit-test framework."
 (not (scan "test/cxxtest|.git/" (namestring file))))

(defun motorola-components (&key (reporter #'gen-report))
  (funcall reporter "Motorola Components" '(
			     ("frameworks" ("endive/motorola/frameworks"))
			     ("Blur" ("endive/motorola/packages/blur"))
			     ("IMS" ("endive/motorola/frameworks/base/ims"))
			     ("Modem" ("endive/motorola/modem") :filter filter-motorola-modem)
			     ("CDMA ril" ("endive/motorola/modem/cdma/ril"))
			     ("LTE ril" ("endive/motorola/modem/lcm/lcm/ril"))
			     ("MM ril" ("endive/motorola/modem/multimodem/lcm_qc/ril"))
			     ("Telephony" ("endive/motorola/frameworks/base/telephony" "endive/motorola/frameworks/libs/telephony"))
			     ("Motorola apps" ("endive/motorola/packages/apps"))
			     ) :stream t))

(defun google-components (&key (reporter #'gen-report))
  (funcall reporter "Google Components" '(
			     ("Frameworks" ("endive/frameworks"))
			     ("Packages" ("endive/packages"))
			     ("Telephony" ("endive/frameworks/base/telephony"))
			     ) :stream t))



			     