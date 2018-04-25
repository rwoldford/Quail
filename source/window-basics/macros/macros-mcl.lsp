;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                              macros-mcl.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is an addition to Window-basics for moving pointclouds
;;;  
;;;
;;;  Authors:
;;;     R.W. Oldford 1992
;;;     
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------

(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(with-focused-canvas host-to-canvas-y)))


(defmacro with-focused-canvas (canvas &body body)
  "Executes the body with all drawing methods focused on the ~
   canvas."
  `(case (display-mode-of ,canvas)
     (:active (ccl::with-focused-view (the-scroller ,canvas) ,@body))
     (:postscript ,@body)
     (:printer ,@body)
     (:mcl-printer ,@body)))

;;;------------------------------------------------------------------------
;;;
;;; Explicit fns to map between downwards-positive y-coords on mac and
;;; upwards-positive y-coords in canvas world.
;;;

(defmacro host-to-canvas-y (canvas y)
  `(- (canvas-height ,canvas) ,y))

(defmacro canvas-to-host-y (canvas y)
  `(- (canvas-height ,canvas) ,y))
