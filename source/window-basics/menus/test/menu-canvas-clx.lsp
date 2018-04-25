;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          menu-canvas-clx.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;
;;;  Authors:
;;;     H.A. Chipman 1991
;;;     C.B. Hurley 1989-1991
;;;     J.A. McDonald 1988-89
;;;     R.W. Oldford 1989-1991
;;;     J.O. Pedersen 1988-89
;;;     
;;;
;;;
;;;---------------------------------------------------------------------
;;; The story:
;;;
;;; Want various hierarchical menus to be associated with a canvas.
;;; These will pop-up in response to certain mouse-events while the
;;; mouse is over either the title-bar of the canvas, or over the
;;; body of the canvas.
;;;
;;; This file (and others specialized for each CL) contains those
;;; functions which attach menus to, and select menus from, a canvas.
;;; 
;;; -------------------------------------------------------------------
;;;
;;; Author(s):
;;;       rwoldford@stat.waterloo.edu
;;;
;;; The model is described in menu-canvas.  Menus are described in
;;; menu & menu-mcl.
;;;   

(in-package :wb)

(defun release-menu-space (menu)
  (destroy-menu menu))



(defmethod set-up-title-menus ((canvas menu-canvas))
  (install-title-menus canvas :menu-type :pop-up)
  (install-menu-bar canvas))
