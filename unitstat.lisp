(in-package :cl-user)

(defpackage :unitstat
  (:use :cl :cl-fad :cl-ppcre :cl-shell))

(in-package :unitstat)

(defun java-file-p ()
  (lambda (file) (ends-with (namestring file) ".java")))

(defun java-test-file-p ()
  (lambda (file) (ends-with (namestring file) "Test.java")))

(defun standard-class-p (line)
  "Identify all classes and interfaces"
  ;(and (scan "public.*class" line)  (not (scan "abstract.*class" line))))
  (not (null (scan "public.*(interface|class)" line))))

(defun abstract-class-p (line)
  "Identify interfaces and abstract classes."
  (not (null (scan "public.*(interface|abstract.*class)" line))))

(defun analyze (name directory filter)
  "Analyze source-code in a given directory and report the findings."
  (let ((allFiles 0)
	(allTestFiles 0)
	(linesOfCode 0)
	(linesOfTestCode 0)
	(numberofClasses 0)
	(numberofAbstractClasses 0)
	(test-file-p nil))
    (unless (directory-exists-p directory) (error "Directory does not exist!"))
    (-> 
     (find-files directory :test (java-file-p)) 
     filter
     (observer #'(lambda (file) (if (ends-with (namestring file) "Test.java") (progn (incf allTestFiles) (setf test-file-p t)) (setf test-file-p nil)) (incf allFiles)))
     (file-pumper)
     (sink #'(lambda (line)
	       (if (standard-class-p line)
		   (incf numberofClasses))
	       (if (abstract-class-p line)
		   (incf numberofAbstractClasses))
	       (if (not (null test-file-p))
		   (incf linesOfTestCode))
	       (incf linesOfCode))))
    (let* ((fileRatio (/ allTestFiles allFiles))
	   (kloc (round linesOfCode 1000))
	   (testKloc (round linesOfTestCode 1000))
	   (klocRatio (/ testKloc kloc))
	   (abstraction (/ numberOfAbstractClasses numberofClasses)))
    (list name directory allFiles allTestFiles fileRatio kloc testKloc klocRatio abstraction))))

; printf "%-10s %30s %15s %15s %10s %15s %15s %10s %10s\n" "Company" "Directory" "Java Files" "Test Files" "Ratio" "KLOC" "Test KLOC" "Ratio" "Abstraction"

(defun println (name directory files testFiles fileRatio kloc testKloc klocRatio abstraction)
  (format t "~10A ~30A ~15A ~15A ~10A ~15A ~15A ~10A ~10A~%" name directory files testFiles fileRatio kloc testKloc klocRatio abstraction))

(defun header (name)
  (format t "Analysis for ~A~%" name)
  (println "Company" "Directory" "Java Files" "Test Files" "Ratio" "KLOC" "Test KLOC" "Ratio" "Abstraction"))


(defun z ()
  (analyze "Google" "/Users/john/Development/android/sdk/" (null-filter)))

(defun x ()
 (analyze "Google" "/home/john/endive/frameworks/base" (null-filter)))

(defun y ()
  (let ((home "/Users/john/Development/android/sdk/")
	(work "/home/john/endive/frameworks/base"))
    (if (directory-exists-p home)
	(analyze "Google" home (null-filter)))
    (if (directory-exists-p work)
	(analyze "Google" work (null-filter)))))

#|
#!/bin/bash

function need() {
	if [[ $# -lt 2 ]] ; then
		echo "bad arg count $#" 1>&2
		exit 1
	fi
	count=$1; shift
	if [[ $# -ne $count ]] ; then
		echo "bad arg count $#" 1>&2
		exit 1
	fi
}

function all() {
	need 1 $*
	find $1 -name '*.java' -type f -print 
}

function allNonTest() {
	need 1 $*
	find $1 -name '*.java' -type f -print | egrep -v "Test.java$"
}

function allTest() {
	need 1 $*
	find $1 -name '*Test.java' -type f -print 
}

function noMoto() {
	egrep -v "private|moto"
}

function moto() {
	egrep "private|moto"
}

function noFilter() {
	cat
}

function linesOfCode() {
	local -i count=0 total=0
	while read filename
	do
		count=`wc -l < $filename`
		total=`echo "$total + $count" | bc`
	done
	echo "$total / 1000" | bc
}

function classes() {
	local -i count=0 total=0
	while read filename
	do
		count=`egrep "public.*class" $filename | egrep -v "abstract.*class" | wc -l`
		total=`echo "$total + $count" | bc`
	done
	echo $total 
}

function abstractClasses() {
	local -i count=0 total=0
	while read filename
	do
		count=`egrep "public.*(?interface|abstract.*class)" $filename | wc -l`
		total=`echo "$total + $count" | bc`

	done
	echo $total 
}

function linecount() {
	wc -l
}

function percent() {
	need 2 $*
	echo "$1 * 100 / $2" | bc -l
}

function header() {
	echo "Analysis for $1"
	printf "%-10s %30s %15s %15s %10s %15s %15s %10s %10s\n" "Company" "Directory" "Java Files" "Test Files" "Ratio" "KLOC" "Test KLOC" "Ratio" "Abstraction"
}

function analyze() {
	need 3 $*
	name=$1; cwd=$2; filter=$3
	if [[ ! -d $cwd ]] ; then
		# echo "$cwd is not a directory" 1>&2
		return
	fi
	company=`all $cwd | $filter | linecount`
	companyTest=`allTest $cwd | $filter | linecount`
	companyPercent=`percent $companyTest $company`
	printf "%-10s %30s %15d %15d %10.1f " $name $cwd $company $companyTest $companyPercent
	companyLOC=`all $cwd | $filter | linesOfCode`
	printf "%15d " $companyLOC
	companyTestLOC=`allTest $cwd | $filter | linesOfCode`
	companyLOCPercent=`percent $companyTestLOC $companyLOC`
	printf "%15d %10.1f " $companyTestLOC $companyLOCPercent
	classes=`all $cwd | $filter | classes`
	abstractClasses=`all $cwd | $filter | abstractClasses`
	abstraction=`percent $abstractClasses $classes`
	printf "%10.1f\n" $abstraction
}

function footer() {
	echo
}


for root_dir in $*
do
	header $root_dir
	cd ${HOME}/${root_dir}
	analyze "Google"   "frameworks" "cat"
	analyze "Google"   "frameworks/base/core" "cat"
	analyze "Google"   "frameworks/base/services" "cat"
	analyze "Google"   "packages" "cat"
	analyze "Motorola" "vendor/moto" "cat"
	analyze "Motorola" "private" "cat"
	analyze "Motorola" "motorola" "cat"
	analyze "Google"   .  "noMoto"
	analyze "Motorola" . "moto"
	footer
done

exit 0
|#
