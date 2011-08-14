(in-package :cl-user)

(defpackage :unitstat
  (:use :cl :cl-fad :cl-ppcre :cl-shell)
  (:export
   #:source-file-pathname-p
   #:test-file-pathname-p))

(in-package :unitstat)

(defun c-file-pathname-p (file)
  (ends-with (namestring file) ".c" ".h"))

(defun c-plus-plus-file-pathname-p (file)
  (ends-with (namestring file) ".cpp" ".hpp"))

(defun java-file-pathname-p (file)
  (ends-with (namestring file) ".java"))

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

(defmacro div (q d)
  "Division that avoids divide by zero exceptions."
  `(if (= ,d 0) 0 (/ ,q ,d)))

(defmacro roundif (a q d)
  "Only round when the quotient"a" is greater than the divisor."
  `(if (> ,a ,d) (round ,q ,d) (div ,q ,d)))

(defmacro pct (q d)
  `(if (= ,d 0) 0 (round (* 100 ,q) ,d)))

(defun git-filter ()
  "Returns lambda expression that filters out paths to .git repositories."
  (filter #'(lambda (file) (not (search ".git/" (namestring file))))))

(defun analyze (name directory &optional (filter (git-filter)))
  "Analyze source-code for a given directory and report the findings."
  (let ((allFiles 0)
	(allTestFiles 0)
	(linesOfCode 0)
	(linesOfTestCode 0)
	(numberofClasses 0)
	(numberofAbstractClasses 0)
	(java-file-p nil)
	(test-file-p nil))
    (unless (directory-exists-p directory) (error "Directory does not exist!"))
    (->
     (find-files directory :test #'source-file-pathname-p)
     filter
     (observer #'(lambda (file)
		   (if (test-file-pathname-p file)
		       (progn (incf allTestFiles) (setf test-file-p t))
		       (setf test-file-p nil))
		   (setf java-file-p (java-file-pathname-p file))
		   (incf allFiles)))
     (file-pumper)
     (sink #'(lambda (line)
	       (if java-file-p
		   (if (standard-class-p line)
		       (incf numberofClasses))
		   (if (abstract-class-p line)
		       (incf numberofAbstractClasses)))
	       (if test-file-p
		   (incf linesOfTestCode))
	       (incf linesOfCode))))
    (let* ((fileRatio (div allTestFiles allFiles))
	   (kloc (roundif linesOfCode linesOfCode 1000))
	   (testKloc (roundif linesOfCode linesOfTestCode 1000))
	   (klocRatio (div testKloc kloc))
	   (abstraction (div numberOfAbstractClasses numberofClasses)))
      (list name (namestring directory) allFiles allTestFiles fileRatio kloc testKloc klocRatio abstraction))))

; printf "%-15s %30s %15s %15s %10s %15s %15s %10s %10s\n" "Company" "Directory" "Java Files" "Test Files" "Ratio" "KLOC" "Test KLOC" "Ratio" "Abstraction"

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
  "Iterate over contents of a repo and invoke a handler with the name and path of each repo."
  (funcall header dir)
  (with-chdir dir
    (-> 
     (process repo-program-path '("list"))
     (transformer #'(lambda (line) (subseq line 0 (search " " line))))
     (filter #'(lambda (line) (cl-ppcre:scan filters line)))
     (sink #'(lambda (line) (funcall analysis-handler (analyze (basename line) (merge-pathnames line))))))))

#|
(defun faux-analyze-repo (&key header analysis-handler)
  (declare (ignore header))
  (funcall analysis-handler '("bionic" "/home/john/endive/bionic" 1793 1 1/1793 184 0 0 0))
  (funcall analysis-handler '("src" "/device/motorola/modem/ste/src" 1089 104 104/1089 229 23 23/229 0)))
|#

(defun analyze-repo-sexp ()
  (let ((results nil))
    (analyze-repo :header #'(lambda (dir) (declare (ignore dir))) :analysis-handler #'(lambda (analysis) (push analysis results)))
    results))