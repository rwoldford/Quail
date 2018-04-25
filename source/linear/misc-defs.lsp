;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               misc-defs.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1994 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Michael Lewis 1991.
;;;     R.W. Oldford 1994.
;;;
;;;
;;;--------------------------------------------------------------------------------
;;;

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(ref-variable add-ones quail-reciprocal multiple-position-if
          mat1dp mat2dp nrows ncols)))

(defgeneric nrows (thing)
  (:documentation "Returns the number of rows in the thing. ~
                   (:see-also (ncols :generic-function) ~
                   (dimensions-of :generic-function) ~
                   )"
                  )
  )

(defmethod nrows ((thing T))
  (or (first (dimensions-of thing)) 1))

(defgeneric ncols (thing)
  (:documentation "Returns the number of columns in the thing. ~
                   (:see-also (nrows :generic-function) ~
                   (dimensions-of :generic-function) ~
                   )"
                  )
  )

(defmethod ncols ((thing T))
  (or (second (dimensions-of thing)) 1))

;-------------------------------------------------------------------
;METHODS: ref-variable, (setf ref-variable)
;-------------------------------------------------------------------

(defgeneric ref-variable (matrix key)
  
  (:documentation
   
   "Args: matrix, a quail matrix, and key, a number ~
   for now, possibly a label later.  If the matrix ~
   has one dimension (i.e.; a vector), the function ~
   returns the key-th element; if the matrix has ~
   rows and columns, it returns the key-th column."))

(defmethod ref-variable ((self matrix) key)
  (cond ((mat1dp self) (ref self key))
        (t (ref self t key))))

(defgeneric (setf ref-variable) (new-value matrix key)
  
  (:documentation
   
   "Args: new-value; matrix, a quail matrix, and key, a number ~
   for now, possibly a label later.  Sets the portion ~
   of matrix relating to the key-th element (if matrix is actually ~
   a vector) or key-th column to new-value."))

(defmethod (setf ref-variable) (new-value (self matrix) key)
  (cond ((mat1dp self)
         (setf (ref self key) new-value))
        (t
         (setf (ref self t key) new-value))))

;--------------------------------------------------------------
;DEMETHODS: add-ones
;--------------------------------------------------------------

(defgeneric add-ones (X)
  (:documentation 
   "Adds a column of ones before a matrix X."))

(defmethod add-ones (X)
  (missing-method 'add-ones X))

(defmethod add-ones ((X matrix))
  (cglue (array 1
                 :dimensions (list (first (dimensions-of x)))) 
         X))

;-----

(defgeneric multiple-position-if (sequence predicate)
  (:documentation
   "Returns a list of the positions of elements of sequence ~
    that satisfy the predicate, which must take one argument."))

(defmethod multiple-position-if ((sequence matrix) predicate)
  (do ((i 0 (+ i 1))
       (n (first (dimensions-of sequence)))
       (z '()))
      ((= i n) (nreverse z))
    (when (funcall predicate (eref sequence i))
      (push i z))))

(defmethod multiple-position-if :around ((sequence matrix) predicate)
  (declare (ignore predicate))
  (if (mat1dp sequence) (call-next-method)
      (quail-error "~s is not a one-dimensional matrix (vector)."
                   sequence)))

(defmethod multiple-position-if ((sequence sequence) predicate)
  (do ((i 0 (+ i 1))
       (n (first (dimensions-of sequence)))
       (z '()))
      ((= i n) (nreverse z))
    (when (funcall predicate (eref sequence i))
      (push i z))))

;-----

(defun mat2dp (z)
  (and (or (typep z 'array)
           (typep z 'matrix))
       (= (array-rank z) 2)))

(defun mat1dp (z)
  (or (typep z 'sequence)
      (and (or (typep z 'array)
               (typep z 'matrix))
           (= (array-rank z) 1))))

