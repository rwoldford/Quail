;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               pivot.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1994 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Michael Lewis 1991.
;;;
;;;
;;;--------------------------------------------------------------------------------
;;;

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(pivot-in-place unpivot-in-place)))

(defgeneric pivot-in-place (unpivoted-matrix pivots)
  (:documentation 
   "Returns a matrix whose columns are selected
    from unpivoted-matrix in the order given by 
    pivots.  The column numbers in pivots
    start with 1 for the first column, not 0.
    The computations are done in place, i.e.
    the unpivoted-matrix is modified to become pivoted."))

(defgeneric unpivot-in-place (pivoted-matrix pivots)
  (:documentation 
   "The inverse of pivot-in-place.
    The column numbers in pivots
    start with 1 for the first column, not 0.
    The computations are done in place, i.e.
    the pivoted-matrix is modified to become unpivoted."))

;;; should try this using with-ref !!

(defmethod-multi-one-body pivot-in-place 
  ((the-matrix (vector array matrix)) pivots)
  (let ((unpivoted (sel the-matrix))
        (cols (first (dimensions-of pivots))))
    (loop for i from 0 to (- cols 1)
          do (setf (ref-variable the-matrix i)
                   (ref-variable unpivoted (- (eref pivots i) 1))))
    the-matrix))

(defmethod-multi-one-body unpivot-in-place
  ((the-matrix (vector array matrix)) pivots)
  (let ((pivoted (sel the-matrix))
        (cols (first (dimensions-of pivots))))
    (loop for i from 0 to (- cols 1)
          do (setf (ref-variable the-matrix (- (eref pivots i) 1))
                   (ref-variable pivoted i)))
    the-matrix))
