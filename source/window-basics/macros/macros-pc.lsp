;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       macros-pc.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is an addition to Window-basics for moving pointclouds
;;;  
;;;  Authors:
;;;     R.W. Oldford 1992
;;;     G.W. Bennett 1995
;;;     
;;;     This file is for the PC-port but is an unmodified
;;;      copy of the standard macros-mcl and -clx files. (gwb)
;;;----------------------------------------------------------------------------------
(in-package :wb)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(with-focused-canvas host-to-canvas-y)))

(defvar display-mode-of)

(defmacro with-focused-canvas (canvas &body body)
  "Executes the body with all drawing methods focused on the ~
   canvas."
  `(case (display-mode-of ,canvas)
     (:active ,@body)            ;; change when we know how to focus (ccl::with-focused-view (the-scroller ,canvas) ,@body))
     (:postscript ,@body)
     (:printer ,@body)
     (:mcl-printer ,@body)))
;;;------------------------------------------------------------------------
;;; Explicit fns to map between downwards-positive y-coords on mac and
;;; upwards-positive y-coords in canvas world.
(defmacro host-to-canvas-y (canvas y)
  `(- (canvas-height ,canvas) ,y))
(defmacro canvas-to-host-y (canvas y)
   `(- (canvas-height ,canvas) ,y))
