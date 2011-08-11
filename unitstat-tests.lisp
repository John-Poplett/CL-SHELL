(defpackage :unitstat-tests
  (:use :common-lisp :lisp-unit :unitstat))

(in-package :unitstat-tests)

(define-test test-source-file-pathname-p
  (dolist (path '("test.c" "test.h" "test.cpp" "test.hpp" "test.java" "test.pl" "test.pm" "test.pl" "test.lisp"))
    (assert-true (unitstat:source-file-pathname-p (pathname path))))
  (dolist (path '("TODO" "README.txt" "FooBar.doc"))
    (assert-false (unitstat:source-file-pathname-p (pathname path)))))

(define-test test-test-file-pathname-p
  (dolist (path '("test.c" "test.h" "test.cpp" "test.hpp" "test.java" "test.pl" "test.pm" "test.pl" "test.lisp"))
    (assert-true (unitstat:test-file-pathname-p (pathname path)))))
