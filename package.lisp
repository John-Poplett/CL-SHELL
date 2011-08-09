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
    #:message-count
    #:null-filter
    #:observer
    #:process
    #:sink
    #:standard-output-sink
    #:transformer))

