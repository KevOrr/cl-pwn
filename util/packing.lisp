(in-package :cl-pwn/util)

(defparameter *word-size* 32)
(defparameter *endianness* :little-endian)
(defparameter *sign* nil)

;; Begin awesome PCL chapter 24 stuff

;; Object stack for nested binary classes
(defvar *in-progress-objects* nil)

(defun current-binary-object () (first *in-progress-objects*))

(defun parent-of-type (type)
  (find-if #'(lambda (x) (typep x type)) *in-progress-objects*))


;; Generic functions for reading/writing binary values
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


;; Generic functions for reading/writing binary classes
(defgeneric read-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Fill in the slots of object from stream."))

(defmethod read-object :around (object stream)
  (declare (ignore stream))
  (let ((*in-progress-objects* (cons object *in-progress-objects*)))
    (call-next-method)))

(defgeneric write-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Write out the slots of object to the stream."))

(defmethod write-object :around (object stream)
  (declare (ignore stream))
  (let ((*in-progress-objects* (cons object *in-progress-objects*)))
    (call-next-method)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mklist (x)
    (if (listp x) x (list x))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun normalize-slot-spec (spec)
    (list (first spec) (mklist (second spec)))))


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
  (ecase (length spec)
    (1
     (with-gensyms (type stream val)
       (destructuring-bind (derived-from &rest derived-args) (mklist (first spec))
         `(progn
            (defmethod read-value ((,type (eql ',name)) ,stream &key ,@args)
              (read-value ',derived-from ,stream ,@derived-args))
            (defmethod write-value ((,type (eql ',name)) ,stream ,val &key ,@args)
              (write-value ',derived-from ,stream ,val ,@derived-args))))))
    (2
     (with-gensyms (type)
       `(progn
          ,(destructuring-bind ((in) &body body) (rest (assoc :reader spec))
             `(defmethod read-value ((,type (eql ',name)) ,in &key ,@args)
                ,@body))
          ,(destructuring-bind ((out value) &body body) (rest (assoc :writer spec))
             `(defmethod write-value ((,type (eql ',name)) ,out ,value &key ,@args)
                ,@body)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun binary-class-slot->read-value (spec stream)
    (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
      `(setf ,name (read-value ',type ,stream ,@args)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun binary-class-slot->write-value (spec stream)
    (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
      `(write-value ',type ,stream ,name ,@args))))

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

;; Generic binary types

(define-binary-type unsigned-word ((word-size *word-size*) (endianness *endianness*))
  (:reader (in)
           (let* ((result 0)
                  (max-byte-position (* 8 (floor (1- word-size) 8)))
                  (first-byte-offset (if (eq endianness :little-endian) 0 max-byte-position))
                  (last-byte-offset (if (eq endianness :little-endian) max-byte-position 0))
                  (step-by (if (eq endianness :little-endian) 8 -8)))
             (loop :for i := first-byte-offset :then (+ i step-by)
                   :do (setf (ldb (byte 8 i) result) (read-byte in))
                   :until (= i last-byte-offset))
             result))
  (:writer (out num)
           (let* ((max-byte-position (* 8 (floor (1- word-size) 8)))
                  (first-byte-offset (if (eq endianness :little-endian) 0 max-byte-position))
                  (last-byte-offset (if (eq endianness :little-endian) max-byte-position 0))
                  (step-by (if (eq endianness :little-endian) 8 -8)))
             (loop :for i := first-byte-offset :then (+ i step-by)
                   :do (write-byte (ldb (byte 8 i) num) out)
                   :until (= i last-byte-offset)))))

;; Some pre-baked types

(define-binary-type ub8 ()
  (unsigned-word :endianness :big-endian :word-size 8))

(define-binary-type ub16 ()
  (unsigned-word :endianness :big-endian :word-size 16))

(define-binary-type ub32 ()
  (unsigned-word :endianness :big-endian :word-size 32))

(define-binary-type ub64 ()
  (unsigned-word :endianness :big-endian :word-size 64))

(define-binary-type ul8 ()
  (unsigned-word :endianness :little-endian :word-size 8))

(define-binary-type ul16 ()
  (unsigned-word :endianness :little-endian :word-size 16))

(define-binary-type ul32 ()
  (unsigned-word :endianness :little-endian :word-size 32))

(define-binary-type ul64 ()
  (unsigned-word :endianness :little-endian :word-size 64))

;; Test unsigned-big-word and unsigned-little-word
(define-binary-class packing-test ()
  ((ul8 ul8)
   (ul16 ul16)
   (ul32 ul32)
   (ul64 ul64)
   (ub8 ub8)
   (ub16 ub16)
   (ub32 ub32)
   (ub64 ub64)))

;; Very much not portable
#+sbcl
(defun objects-equalp (&rest objects)
  (flet ((object->hash-table (object)
           (let ((ht (make-hash-table))
                 (class-mo (class-of object)))
             (loop :for slot :in (sb-mop:class-slots class-mo)
                   :do (setf (gethash (sb-mop:slot-definition-name slot) ht)
                             (sb-mop:slot-value-using-class class-mo object slot)))
             ht)))
    (and (apply #'eq (mapcar #'type-of objects))
         (apply #'equalp (mapcar #'object->hash-table objects)))))

(defun test-packing ()
  (let ((testobj (make-instance 'packing-test
                                :ul8  #x01
                                :ul16 #x0102
                                :ul32 #x01020304
                                :ul64 #x0102030405060708
                                :ub8  #x01
                                :ub16 #x0102
                                :ub32 #x01020304
                                :ub64 #x0102030405060708)))
    (with-open-file (stream #p"testout" :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
      (write-object testobj stream))

    (with-open-file (stream #p"testout" :element-type '(unsigned-byte 8))
      (let ((*print-base* 16))
        (loop :for i := (read-byte stream nil :eof) :until (eq i :eof) :do
          (format t "~2,'0d" i))))

    (with-open-file (stream #p"testout" :element-type '(unsigned-byte 8))
      (let ((newobj (make-instance 'packing-test)))
        ;; TODO slots aren't getting filled
        (read-object newobj stream)
        #+sbcl (assert (objects-equalp newobj testobj))
        (list newobj testobj)))))
