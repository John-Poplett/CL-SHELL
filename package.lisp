;;;; package.lisp

(defpackage #:cl-shell
  (:use #:cl #:cl-fad #:chanl #:lisp-unit)
  (:export #:-> #:send-list #:find-files #:sink))

