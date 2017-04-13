;;;; cl-pwn.lisp

(in-package #:cl-pwn)

;;; "cl-pwn" goes here. Hacks and glory await!

(defun remote (host port)
  (socket-stream (socket-connect host port)))
