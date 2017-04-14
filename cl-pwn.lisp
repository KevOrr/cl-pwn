;;;; cl-pwn.lisp

(in-package #:cl-pwn)

;;; "cl-pwn" goes here. Hacks and glory await!

(defun remote (host port)
  (socket-stream (socket-connect host port)))

(defclass tube ()
  ((default-timeout
    :initarg default-timeout
    :initform nil)
   (newline
    :initarg newline
    :initform (coerce #\linefeed 'string))))
