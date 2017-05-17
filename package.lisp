;;;; package.lisp

(defpackage #:cl-pwn
  (:use #:cl #:usocket)
  (:export :tube))

(defpackage #:cl-pwn/util
  (:use #:cl #:generators)
  (:export #:*whitespace*
           #:*ascii-lowercase*
           #:*ascii-uppercase*
           #:*ascii-letters*
           #:*digits*
           #:*hex-digits*
           #:*octal-digits*
           #:*punctuation*

           #:cyclic
           #:cyclic-find
           #:cyclic-metasploit
           #:cyclic-metasploit-find

           ;; TODO pwntools uses a different naming scheme for the prebuilt packers
           #:unsigned-word
           #:ub8
           #:ub16
           #:ub32
           #:ub64
           #:ul8
           #:ul16
           #:ul32
           #:ul64
           #:pack
           #:unpack))
