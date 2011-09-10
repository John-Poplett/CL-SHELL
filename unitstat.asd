;;;; unitstat.asd

(asdf:defsystem #:unitstat
  :version "1.0"
  :licence "MIT"
  :serial t
  :depends-on (#:cl-fad #:cl-ppcre #:cl-shell #:local-time #:statistics)
  :components ((:file "unitstat")))

(defmethod perform ((o test-op) (c (eql (find-system :unitstat))))
  (operate 'load-op :unitstat-tests)
  (operate 'test-op :unitstat-tests))
