;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               matrix-sqrt.lisp                              
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '()))




;-------------------------------------------------------------------------------
; GENERIC FUNCTION: matrix-sqrt
;-------------------------------------------------------------------------------

(defgeneric matrix-sqrt (object)
  (:documentation
   
   "Returns the matrix-sqrt of object. Depending on the dimensions of ~
    the object this will either be the simple square root ~
    or a matrix S such that tp(S).*S will return the object. ~
    At present, S is the Cholesky factor of object."))

(defmethod matrix-sqrt ((object t))
  (sqrt object))

(defmethod matrix-sqrt ((object matrix))
  (cond
   ((mat1dp object) (sqrt object))
   ((mat2dp object) (upper-triangle-of (cholesky-of object)))))
