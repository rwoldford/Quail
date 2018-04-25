;;; -*- Mode: LISP / Syntax: Common Lisp / Package: :test

(in-package "TEST")

(defgeneric adjust-array (array new-dimensions &rest keyword-args)
  (:documentation
   "Returns an array of the same type and rank as array, ~
    with the specified new-dimensions. ~
    This function may either alter the given array ~
    or create and return a new one."))

(defmethod adjust-array ((a array) new-dimensions &rest keyword-args)
  (apply #'common-lisp:array a new-dimensions keyword-args))

(defmethod adjust-array ((a ref-array) new-dimensions &rest keyword-args)
  (let ((contents (contents-of a)))
    (with-slots (specs dimensions) a
      (setf (slot-value a 'contents)
            (if 
              (adjustable-array-p contents)
              (apply #'adjust-array contents new-dimensions keyword-args)
              (apply #'adjust-array
                     (make-array (array-dimensions contents) 
                                 :displaced-to contents
                                 :adjustable t)
                     new-dimensions
                     keyword-args)))
      (setf dimensions new-dimensions)
      (setf specs (mapcar #'min specs new-dimensions)))))

(defmethod min-object ((x cons) (y number))
  (mapcar #'(lambda (z)
              (min z y))
          x))

(defmethod min-object ((x number) (y cons))
  (mapcar #'(lambda (z)
              (min z y))
          y))

