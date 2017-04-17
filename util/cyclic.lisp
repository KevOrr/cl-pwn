(in-package "CL-PWN/UTIL")

;; TODO is using #'char-code portable, or do I need to use raw integers instead?
(defconstant +ascii-lowercase+
  #.(make-array 26 :element-type '(unsigned-byte 8) :initial-contents
              (loop :for i :from (char-code #\a) :to (char-code #\z) :collect i)))

(defconstant +ascii-uppercase+
  #.(make-array 26 :element-type '(unsigned-byte 8) :initial-contents
                (loop :for i :from (char-code #\A) :to (char-code #\Z) :collect i)))

(defconstant +ascii-letters+
  #.(concatenate 'vector +ascii-lowercase+ +ascii-uppercase+))

(defconstant +digits+
  #.(make-array 10 :element-type '(unsigned-byte 8) :initial-contents
                (loop :for i :from (char-code #\0) :to (char-code #\9) :collect i)))

(defconstant +hex-digits+
  #.(make-array 22 :element-type '(unsigned-byte 8) :initial-contents
                (concatenate 'list
                             +digits+
                             (loop :for i :from (char-code #\a) :to (char-code #\f) :collect i)
                             (loop :for i :from (char-code #\A) :to (char-code #\F) :collect i))))

(defconstant +punctuation+
  #.(make-array 32 :element-type '(unsigned-byte 8) :initial-contents
                (concatenate 'list
                             (loop :for i :from #x21 :to #x2f :collect i)
                             (loop :for i :from #x3a :to #x40 :collect i)
                             (loop :for i :from #x5b :to #x60 :collect i)
                             (loop :for i :from #x7b :to #x7e :collect i))))

; TODO +whitespace+
; TODO +printable+ = (concatenate +ascii-letters+ +digits+ +punctuation+ +whitespace+)
