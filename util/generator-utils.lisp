(in-package :cl-pwn/util)

(defun gen-find-subseq (subseq gen n)
  "Returns the first position of `subseq' in the generator or nil if there is no such position"

  (let ((grouped (gen-partition-gen gen n)))
    (iter:iter (iter:for group in-generator grouped) (iter:for i from 0)
      (if (equal subseq (coerce group 'string))
          (return-from gen-find-subseq i)))
    nil))

(defun gen-partition-gen (gen n &optional (k 1) (pad nil pad-passedp))
  (make-generator ()
    (iter:iter
      ;; fill up out with n items
      (when (iter:first-iteration-p)
        (iter:nconcing (iter:iter (iter:repeat n)
                         (iter:for item :in-generator gen)
                         (iter:collect item))
                       :into out))

      (iter:while (= n (length out)))
      (yield (copy-list out))
      (iter:nconcing (iter:iter (iter:repeat k)
                       (iter:for item :in-generator gen)
                       (iter:collect item))
                     :into out)
      (setf out (nthcdr k out)))

    ;; If there are leftover items and pad-passedp, pad and yield
    (when (and out pad-passedp)
      (setf out (nconc out pad))
      (yield (copy-list (subseq out 0 (min n (length out))))))))

(defun gen-items-in (seq)
  (make-generator ()
    (loop :for i :in seq :do
      (yield i))))
