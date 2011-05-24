;;; utils.lisp

(in-package #:cl-shell)

(defun count-files (directory &key (test (constantly t)))
  (-> (find-files directory :test test) (message-count)))

