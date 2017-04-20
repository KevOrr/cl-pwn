(in-package :cl-pwn/util)



;; de Bruijn generation

;; TODO figure out why this algorithm works
(defun de-bruijn-gen (&key (alphabet *ascii-lowercase*) (n 4))
  "Generator for a sequence of unique substrings of length n
  Taken from https://en.wikipedia.org/wiki/De_Bruijn_sequence and pwntools source"

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
               composed of alphabet `alphabet', with distinct subsequences of length `n'"

  (assert (or (null length) (<= length (expt (length alphabet) n))))

  (let ((gen (de-bruijn-gen :alphabet alphabet :n n)))
    (coerce (if length
                (iter:iter (iter:for i in-generator gen) (iter:repeat length) (iter:collect i))
                (force gen))
            'string)))

;; TODO accept integer as subseq
;; TODO is `n' really necessary here? cyclic-metasploit-find doesn't include it
;; TODO pwntools produces a warning message when len(string) > 4
(defun cyclic-find (subseq &key (alphabet *ascii-lowercase*) (n 4))
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
           or nil if it was not found"

  (declare (sequence subseq))

  (let ((n (if (or (null n) (= 0 n))
               (length subseq)
               n)))

    (cond
      ((every (lambda (chr) (find chr alphabet)) subseq)
       (gen-find-subseq subseq (de-bruijn-gen :alphabet alphabet :n n) n))
      (t nil))))


;; Metasploit pattern

(defun metasploit-pattern-gen (&key (sets (list *ascii-uppercase* *ascii-lowercase* *digits*)))
  "Generator for a sequence of characters as per Metasploit Framework's
    `Rex::Text.pattern_create' (aka `pattern_create.rb').

    The returned generator will yield up to
    (* (length sets) (apply #'* (map 'list #'length sets))) elements.

    Arguments:
        sets: List of strings to generate the sequence over."

  (declare (sequence sets))
  (make-generator ()
    (let ((offsets (make-array (length sets)
                               :element-type 'integer
                               :initial-element 0)))

      (iter:iter
        (iter:iter
          (iter:for i :in sets)
          (iter:for j :in-vector offsets)
          (yield (elt i j)))

        (iter:iter
          (iter:for i :from (1- (length offsets)) :downto 0)
          (setf (aref offsets i)
                (mod (1+ (aref offsets i)) (length (nth i sets))))
          (iter:while (= 0 (aref offsets i))))

        (iter:until (every #'zerop offsets))))))

(defun cyclic-metasploit (&key length (sets (list *ascii-uppercase* *ascii-lowercase* *digits*)))
  "A simple wrapper over :func:`metasploit_pattern`. This function returns a
    string of length `length`.

    Arguments:
        length: The desired length of the string or None if the entire sequence is desired.
        sets: List of strings to generate the sequence over."


  (let ((gen (metasploit-pattern-gen :sets sets)))
    (coerce (if length
                (iter:iter
                  (iter:repeat length)
                  (iter:for i in-generator gen)
                  (iter:collect i))
                (iter:iter
                  (iter:for i in-generator gen)
                  (iter:collect i)))
            'string)))

;; TODO accept integer as subseq
;; TODO pwntools produces a warning message when len(string) > 4
(defun cyclic-metasploit-find (subseq &key (sets (list *ascii-uppercase* *ascii-lowercase* *digits*)))
  "Calculates the position of a substring into a Metasploit Pattern sequence.

  Arguments:
      subseq: The subsequence to look for. This can be a string or an
              integer. If an integer is provided it will be packed as a
              little endian integer.
      sets: List of strings to generate the sequence over."

  (declare (sequence subseq))

  ;; make sure each character in `subseq' is in at least one of `sets'
  (cond
    ((every (lambda (chr)
              (some (lambda (alphabet)
                      (find chr alphabet))
                    sets))
            subseq)
     (gen-find-subseq subseq (metasploit-pattern-gen :sets sets) (length subseq)))
    (t nil)))
