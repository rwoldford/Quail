;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                           ref-if.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Bob White
;;;
;;;
;;;----------------------------------------------------------------------------

(in-package :quail-kernel)


(eval-when (:compile-toplevel :load-toplevel :execute) (export '(ref-if)))

(defgeneric ref-if (object predicate)
  (:documentation 
   "Applies the predicate to each element of object and returns ~
    those elements which return T.  These are returned in an array.  ~
    If no elements satisfy the predicate then an empty array is returned."))

(defmethod ref-if (object predicate)
  (ref object (indices object predicate)))

