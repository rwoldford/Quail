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
    (make-region (canvas-x-offset canvas)
                 (canvas-y-offset canvas)
		 (xlib::drawable-width (host-window canvas))
		 (xlib::drawable-height (host-window canvas))))
