;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          diagonal.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1994 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1994.
;;;
;;;
;;;-----------------------------------------------------------------------------
;;;

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(diagonal diagonal-of)))

(defun diagonal (elements)
  "Returns a diagonal matrix having the specified ~
   elements along its diagonal.  The order is column major ~
   over the contents of elements.~~
   (:see-also (diagonal-of :function)~
   )
   (:required ~
   (:arg elements Any ref'able object -- a sequence, an array, etc.))"
  (let* ((n (number-of-elements elements))
         (d (array 0 :dimensions (list n n)))
         )
    (loop for i from 0 to (- n 1)
          do
          (setf (eref d i i)
                (column-major-eref elements i)))
    d))

(defun diagonal-of (matrix)
  "Returns an array of the (i,i) elements of the matrix.~
   (:see-also (diagonal :function)~
   )"
  (array
   (loop
     for i from 0
     below (min (nrows matrix) (ncols matrix))
     collect (eref matrix i i)))
  )
