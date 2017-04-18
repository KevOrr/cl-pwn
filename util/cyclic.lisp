(in-package "CL-PWN/UTIL")

(defvar *whitespace* (coerce (list #\Space #\Tab #\Linefeed #\Return (code-char #x0b) #\Page) 'string))
(defvar *ascii-lowercase* "abcdefghijklmnopqrstuvwxyz")
(defvar *ascii-uppercase* "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(defvar *ascii-letters* (concatenate 'vector *ascii-lowercase* *ascii-uppercase*))
(defvar *digits* "0123456789")
(defvar *hex-digits* "0123456789abcdefABCDEF")
(defvar *octal-digits* "01234567")
(defvar *punctuation* "!\"#$%&'()*+,-./:;<=>?@[\]^_`{|}~")
(defvar *printable* (concatenate 'string *digits* *ascii-letters* *punctuation* *whitespace*))

(defun de-bruijn-gen (&key (alphabet *ascii-lowercase*) (n 4))
  "Generator for a sequence of unique substrings of length n"
  ;; Taken from https://en.wikipedia.org/wiki/De_Bruijn_sequence and pwntools source
  ;; TODO figure out why this algorithm works

  (format t "(de-bruijn ~A ~A)" alphabet n)

  (let* ((k (length alphabet))
         (a (make-array (* k n)
                        :element-type 'integer
                        :initial-element 0)))
    (labels ((db (s p)
               (make-generator ()
                 (format t "(db ~A ~A)" s p)
                 (cond
                   ((> s n)
                    (print "cond 1")
                    (when (= 0 (mod n p))
                      (loop :for j :from 1 :to (+ p 1) :do
                        (format t "first loop j=~A" j)
                        (yield (aref alphabet (aref a j))))))
                   (t
                    (print "cond t")
                    (setf (aref a s) (aref a (- s p)))
                    (yielding (db (1+ s) p))
                    (loop :for j :from (aref a (- s p)) :to k :do
                      (format t "second loop j=~A" j)
                      (setf (aref a s) j)
                      (yielding (db (1+ s) s))))))))
      (db 1 1))))

(defun de-bruijn (&key (alphabet *ascii-lowercase*) (n 4))
  (let* ((k (length alphabet))
         (a (make-array (* k n)
                        :element-type 'fixnum
                        :initial-element 0))
         (seq nil))

    (labels ((db (s p)
               (cond
                 ((> s n)
                  (when (= 0 (mod n p)))
                  (setf seq (nconc
                             seq
                             (coerce (subseq a 1 (1+ p)) 'list))))
                 (t
                  (setf (aref a s) (aref a (- s p)))
                  (db (1+ s) p)
                  (loop :for j :from (1+ (aref a (- s p))) :below k :do
                    (setf (aref a s) j)
                    (db (1+ s) s))))))
      (db 1 1)
      (map 'list (lambda (i) (aref alphabet i)) seq))))

(defun cyclic (&key (length nil) (alphabet *ascii-lowercase*) (n 4))
  "cyclic &key (length nil) (alphabet *ascii-lowercase*) (n 4) => string

   A simple wrapper over #'de-bruijn. This function returns at most
   `length' elements.

   Arguments:
       length: The desired length of the list or None if the entire sequence is desired.
       alphabet: List or string to generate the sequence over.
       n(int): The length of subsequences that should be unique.

   Value:
       string: represents the de Bruijn sequence of maximum length `length',
               composed of alphabet `alhpabet', with distinct subsequences of length `n'

    Example:
        (cyclic :alphabet \"ABC\" :n 3)
        => \"AAABAACABBABCACBACCBBBCBCCC\"
        (cyclic :length 20)
        \"aaaabaaacaaadaaaeaaa\"
        (let ((alphabet (loop :for i :from 0 :below 30 :collect i))
              (n 3))
          (values
           (expt (length alphabet) n)
           (length (cyclic :alphabet alphabet :n n))))
        =>  27000, 27000
    "

  (assert (or (null length) (>= (expt (length alphabet) n) length)))

  (coerce (loop :repeat (or length (expt (length alphabet) n))
                :for i in (de-bruijn :alphabet alphabet :n n) :collect i)
          'string))
