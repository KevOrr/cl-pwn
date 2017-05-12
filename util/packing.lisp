(in-package :cl-pwn/util)

;; Begin awesome PCL chapter 24 stuff
(defgeneric read-value (type stream &key &allow-other-keys)
  (:documentation "Read a single value of the given type from the stream."))

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

(defmacro define-binary-type (name (&rest args) &body spec)
  (with-gensyms (type)
    `(progn
       ,(destructuring-bind ((in) &body body) (rest (assoc :reader spec))
          `(defmethod read-value ((,type (eql ',name)) ,in &key ,@args)
             ,@body))
       ,(destructuring-bind ((out value) &body body) (rest (assoc :writer spec))
          `(defmethod write-value ((,type (eql ',name)) ,out ,value &key ,@args)
             ,@body)))))

(defmacro define-generic-binary-class (name (&rest superclasses) slots read-method)
  (with-gensyms (objectvar streamvar)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (get ',name 'slots) ',(mapcar #'first slots))
         (setf (get ',name 'superclasses) ',superclasses))

       (defclass ,name ,superclasses
         ,(mapcar #'binary-class-slot->defclass-slot slots))

       ,read-method

       (defmethod write-object progn ((,objectvar ,name) ,streamvar)
         (declare (ignorable ,streamvar))
         (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
           ,@(mapcar #'(lambda (x) (binary-class-slot->write-value x streamvar)) slots))))))

(defmacro define-binary-class (name (&rest superclasses) &body body)
  (let ((slots (first body)))
    (with-gensyms (objectvar streamvar)
      `(progn
         (define-generic-binary-class
             ,name ,superclasses ,slots
             (defmethod read-object progn ((,objectvar ,name) ,streamvar)
               (declare (ignorable ,streamvar))
               (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
                 ,@(mapcar #'(lambda (x) (binary-class-slot->read-value x streamvar)) slots))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun binary-class-slot->binding (spec stream)
    (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
      `(,name (read-value ',type ,stream ,@args)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun binary-class-slot->keyword-arg (spec)
    (let ((name (first spec)))
      `(,(as-keyword name) ,name))))

(defmacro define-tagged-binary-class (name (&rest superclasses) slots &rest options)
  (with-gensyms (typevar objectvar streamvar)
    `(define-generic-binary-class
         ,name ,superclasses ,slots
         (defmethod read-value ((,typevar (eql ',name)) ,streamvar &key)
           (let* ,(mapcar #'(lambda (x) (binary-class-slot->binding x streamvar)) slots)
             (let ((,objectvar (make-instance
                                ,@(or (cdr (assoc :dispatch options))
                                      (error "Must supply :dispath form."))
                                ,@(mapcan #'binary-class-slot->keyword-arg slots))))
               (read-object ,objectvar ,streamvar)
               ,objectvar))))))

;; Testing above functions/macros
(define-binary-type unsigned-big ((word-size 32))
  (:reader (in)
           (let ((result 0)
                 (max-byte-position (* 8 (floor (1- word-size) 8))))
             (loop :for i :from max-byte-position :downto 0 :by 8 :do
               (setf (ldb (byte 8 i) result) (read-byte in)))))
  (:writer (out num)
           (let ((max-byte-position (* 8 (floor (1- word-size) 8))))
             (loop :for i :from max-byte-position :downto 0 :by 8 :do
               (write-byte (ldb (byte 8 i) num) out)))))

(define-binary-type unsigned-little ((word-size 32))
  (:reader (in)
           (let ((result 0)
                 (max-byte-position (* 8 (floor (1- word-size) 8))))
             (loop :for i :from 0 :to max-byte-position :by 8 :do
               (setf (ldb (byte 8 i) result) (read-byte in)))))
  (:writer (out num)
           (let ((max-byte-position (* 8 (floor (1- word-size) 8))))
             (loop :for i :from 0 :to max-byte-position :by 8 :do
               (write-byte (ldb (byte 8 i) num) out)))))

(define-binary-type iso-8859-1-string (length)
  (:reader (in)
           (let ((string (make-string length)))
             (dotimes (i length)
               (setf (char string i) (code-char (read-byte in))))
             string))
  (:writer (out string)
           (dotimes (i length)
             (write-byte (char-code (char string i)) out))))

(define-binary-class id3-tag ()
  ((identifier      (iso-8859-1-string :length 3))
   (major-version   u1)
   (revision        u1)
   (flags           u1)
   (size            id3-tag-size)
   (frames          (id3-frames :tag-size size))))

(define-binary-class id3-tag-extra (id3-tag)
  ((extra-stuff     (id3-frames :tag-size size))))
