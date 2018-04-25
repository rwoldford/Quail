;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               clip-mcl.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1988-1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     G. Desvignes 1988,1989
;;;     R.W. Oldford 1988-1992
;;;
;;;
;;;----------------------------------------------------------------------------------
;;;

(in-package :window-basics)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(clipping-region-of)))
          

(defun clipping-region-of (canvas)
  "Returns the content region of the canvas."
  (with-dereferenced-handles  ((rgn (rref (wptr canvas) windowRecord.contRgn)))
   (rlet ((rect :rect))
    (copy-record (rref rgn region.rgnBBox :storage :pointer)
                 :rect
                 rect)
    (make-region (canvas-x-offset canvas)
                 (canvas-y-offset canvas)
                 (- (rref rect rect.right)
                    (rref rect rect.left))
                 (- (rref rect rect.bottom)
                    (rref rect rect.top)))))
  )




