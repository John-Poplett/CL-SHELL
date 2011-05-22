;;;; cl-shell.asd

(asdf:defsystem #:unitstat
  :version "1.0"
  :licence "MIT"
  :serial t
  :depends-on (#:cl-fad #:cl-ppcre #:cl-shell)
  :components ((:file "unitstat")))

