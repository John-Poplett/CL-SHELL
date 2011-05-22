;;;; package.lisp

(defpackage #:cl-shell
  (:use #:cl #:cl-fad #:chanl)
  (:export 
    #:-> 
    #:count-files 
    #:filter 
    #:ends-with 
    #:file-pumper
    #:send-list 
    #:find-files
    #:null-filter
    #:observer
    #:sink
    #:standard-output-sink))

