(in-package :cl-pwn/util)

;; Begin awesome PCL chapter 24 stuff
(defgeneric read-value (type stream &key &allow-other-keys)
  (:documentation "Read a value of the given type from the stream."))
(defmethod read-value ((type symbol) stream &key)
  (let ((object (make-instance type)))
    (read-object object stream)
    object))

(defgeneric write-value (type stream value &key &allow-other-keys)
  (:documentation "Write a value as the given type to the stream."))
(defmethod write-value ((type symbol) stream value &key)
  (assert (typep value type))
  (write-object value stream))

(defgeneric read-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Fill in the slots of object from stream."))

(defgeneric write-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Write out the slots of object to the stream."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mklist (x)
    (if (listp x) x (list x))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun normalize-slot-spec (spec)
    (list (first spec) (mklist (second spec)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun binary-class-slot->read-value (spec stream)
    (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
      `(setf ,name (read-value ',type ,stream ,@args)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun binary-class-slot->write-value (spec stream)
    (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
      `(write-value ',type ,stream ,name ,@args))))


(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop :for n :in names :collect `(,n (gensym)))
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun as-keyword (sym)
    (intern (string sym) :keyword)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun binary-class-slot->defclass-slot (spec)
    (let ((name (first spec)))
      `(,name :initarg ,(as-keyword name) :accessor ,name))))

(defmacro define-binary-class (name (&rest superclasses) &body body)
  (let ((slots (first body)))
    (with-gensyms (objectvar streamvar)
      `(progn
         (defclass ,name ,superclasses
           ,(mapcar #'binary-class-slot->defclass-slot slots))

         (eval-when (:compile-toplevel :load-toplevel :execute)
           (setf (get ',name 'slots) ',(mapcar #'first slots))
           (setf (get ',name 'superclasses) ',superclasses))

         (defmethod read-object progn ((,objectvar ,name) ,streamvar)
           (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
             ,@(mapcar #'(lambda (x) (binary-class-slot->read-value x streamvar)) slots)))

         (defmethod write-object progn ((,objectvar ,name) ,streamvar)
           (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
             ,@(mapcar #'(lambda (x) (binary-class-slot->write-value x streamvar)) slots)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun direct-slots (name)
    (copy-list (get name 'slots))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun inherited-slots (name)
    (loop :for super :in (get name 'superclasses)
          :nconc (direct-slots super)
          :nconc (inherited-slots super))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun all-slots (name)
    (nconc (direct-slots name) (inherited-slots name))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun new-class-all-slots (slots superclasses)
    (nconc (mapcan #'all-slots superclasses) (mapcar #'first slots))))


;; Testing above functions/macros
(define-binary-class id3-tag ()
  ((identifier      (iso-8859-1-string :length 3))
   (major-version   u1)
   (revision        u1)
   (flags           u1)
   (size            id3-tag-size)
   (frames          (id3-frames :tag-size size))))

(define-binary-class id3-tag-extra (id3-tag)
  ((extra-stuff     (id3-frames :tag-size size))))
