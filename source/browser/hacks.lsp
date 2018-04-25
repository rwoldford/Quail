;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               window-basics-package.lisp

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1991.
;;;
;;;
;;;----------------------------------------------------------------------------
(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(compute-window-height compute-window-width)))

  
(defun compute-window-height (height  title-height)
  (declare (ignore title-height))
  (+ height 30))
  
(defun compute-window-width (width title)
  (declare (ignore title))
  (- width 15))
