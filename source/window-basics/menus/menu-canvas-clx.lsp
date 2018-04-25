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
;;;     N.G. Bennett 1993
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



(defmethod set-up-title-menus ((canvas menu-canvas)
                               &key
                               (title-left        "Information")
                               (title-middle      "Edit-display")
                               (title-right       "Canvas")
                               (title-ctrl-left   "")
                               (title-ctrl-middle "")
                               (title-ctrl-right  "")
                               (menu-type :pop-up)
                               (when-selected-fn #'default-when-selected-fn))
  (declare (ignore menu-type))
  
  (install-title-menus canvas
                       :menu-type :pop-up
                       :title-left title-left
                       :title-middle title-middle
                       :title-right title-right
                       :title-ctrl-left title-ctrl-left
                       :title-ctrl-middle title-ctrl-middle
                       :title-ctrl-right  title-ctrl-right
                       :when-selected-fn when-selected-fn)
  (install-menu-bar canvas)
  ;; THe following pops the window at doesn't really belong here
  ;; But the menus must be installed *before* the window is mapped.
  (xlib:map-window (host-window canvas)))
