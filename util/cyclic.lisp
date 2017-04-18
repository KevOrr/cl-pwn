(in-package "CL-PWN/UTIL")

(defvar *whitespace* (coerce (list #\Space #\Tab #\Linefeed #\Return (code-char #x0b) #\Page) 'string))
(defvar *ascii-lowercase* "abcdefghijklmnopqrstuvwxyz")
(defvar *ascii-uppercase* "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(defvar *ascii-letters* (concatenate 'string *ascii-lowercase* *ascii-uppercase*))
(defvar *digits* "0123456789")
(defvar *hex-digits* "0123456789abcdefABCDEF")
(defvar *octal-digits* "01234567")
(defvar *punctuation* "!\"#$%&'()*+,-./:;<=>?@[\]^_`{|}~")
(defvar *printable* (concatenate 'string *digits* *ascii-letters* *punctuation* *whitespace*))

(defun de-bruijn-gen (&key (alphabet *ascii-lowercase*) (n 4))
  "Generator for a sequence of unique substrings of length n"
  ;; Taken from https://en.wikipedia.org/wiki/De_Bruijn_sequence and pwntools source
  ;; TODO figure out why this algorithm works

  (let* ((k (length alphabet))
         (a (make-array (* k n)
                        :element-type 'integer
                        :initial-element 0)))

    (labels ((db (s p)
               (make-generator ()
                 (cond
                   ((> s n)
                    (when (= 0 (mod n p))
                      (loop :for i :across (subseq a 1 (1+ p)) :do
                        (yield (aref alphabet i)))))
                   (t
                    (setf (aref a s) (aref a (- s p)))
                    (yielding (db (1+ s) p))
                    (loop :for j :from (1+ (aref a (- s p))) :below k :do
                      (setf (aref a s) j)
                      (yielding (db (1+ s) s))))))))
      (db 1 1))))

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
               composed of alphabet `alphabet', with distinct subsequences of length `n'
  "

  (assert (or (null length) (<= length (expt (length alphabet) n))))

  (let ((gen (de-bruijn-gen :alphabet alphabet :n n)))
    (coerce (if length
                (loop :repeat length :collect (next gen))
                (force gen))
            'string)))

(defgeneric cyclic-find (subseq &key alphabet n)
  ;; TODO accept integer as subseq
  (:documentation
   "cyclic-find subseq &key (alphabet *ascii-lowercase*) (n nil) => integer or nil

   Calculates the position of a substring into a De Bruijn sequence.

    .. todo:

       'Calculates' is an overstatement. It simply traverses the list.

       There exists better algorithms for this, but they depend on generating
       the De Bruijn sequence in another fashion. Somebody should look at it:

       https://www.sciencedirect.com/science/article/pii/S0012365X00001175

    Arguments:
        subseq: The subsequence to look for. This can be a string, or a list
        alphabet: List or string to generate the sequence over.
        n(int): The length of subsequences that should be unique.

    Value: the position in the sequence where the subsequence was found,
           or nil if it was not found"))

(defmethod cyclic-find ((subseq list) &key (alphabet *ascii-lowercase*) n)
  ;; TODO pwntools produces a warning message when len(string) > 4

  (let ((n (if (or (null n) (= 0 n))
               (length subseq)
               n)))

    (cond
      ((every (lambda (chr) (find chr alphabet)) subseq)
       (gen-find subseq (de-bruijn-gen :alphabet alphabet :n n)))
      (t nil))))

(defmethod cyclic-find ((subseq string) &key (alphabet *ascii-lowercase*) n)
  (cyclic-find (coerce subseq 'list)))

(defun gen-find (subseq gen)
  ;; Returns the first position of `subseq' in the generator or nil if there is no such position
  ;; TODO
  )
