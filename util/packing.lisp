(in-package :cl-pwn/util)

;; Begin awesome PCL chapter 24 stuff
(defun as-keyword (sym) (intern (string sym) :keyword))

(defun binary-class-slot->defclass-slot (spec)
  (let ((name (first spec)))
    `(,name :initarg ,(as-keyword name) :accessor ,name)))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro define-binary-class (name &body body)
  (let ((slots (first body)))
    (with-gensyms (typevar objectvar streamvar)
      `(progn
         (defclass ,name ()
           ,(mapcar #'binary-class-slot->defclass-slot slots))

         (defmethod read-value ((,typevar (eql ',name)) ,streamvar &key)
           (let ((,objectvar (make-instance ',name)))
             (with-slots ,(mapcar #'first slots) ,objectvar
               ,@(mapcar #'(lambda (x) (slot->read-value x streamvar)) slots))
             ,objectvar))))))

(defun mklist (x) (if (listp x) x (list x)))

(defun normalize-slot-spec (spec)
  (list (first spec) (mklist (second spec))))

(defun binary-class-slot->read-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(setf ,name (read-value ',type ,stream ,@args))))

;; Testing above functions/macros
#+()
(define-binary-class id3-tag
  ((identifier      (iso-8859-1-string :length 3))
   (major-version   u1)
   (revision        u1)
   (flags           u1)
   (size            id3-tag-size)
   (frames          (id3-frames :tag-size size))))
