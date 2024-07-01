;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                          menu-canvas-sblx.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;  Authors:
;;;     H.A. Chipman 1991
;;;     C.B. Hurley 1989-1991
;;;     J.A. McDonald 1988-89
;;;     R.W. Oldford 1989-1991
;;;     J.O. Pedersen 1988-89
;;;     G.W. Bennett 1996
;;;     
;;;---------------------------------------------------------------------
;;; The story:
;;; Want various hierarchical menus to be associated with a canvas.
;;; These will pop-up in response to certain mouse-events while the
;;; mouse is over either the title-bar of the canvas, or over the
;;; body of the canvas.
;;; This file (and others specialized for each CL) contains those
;;; functions which attach menus to, and select menus from, a canvas.
;;; 
;;; -------------------------------------------------------------------
;;; Author(s):
;;;       rwoldford@stat.waterloo.edu
;;; The model is described in menu-canvas.  Menus are described in
;;; menu & menu-mcl.
;;;

;;; The model for dealing with menus under mcclim is quite different from the mac/acl model
;;; Neither of these forms is liekly to be useful
;;; If we decide to put menus onto a single quail-menubar using Unmade as a place holder
;;; some version of set-up-title-menus might be needed
;;; GWBennett 07 SEP 2020
   
(in-package :wb)


#|
(defun release-menu-space (menu)
   (destroy-menu menu))

;;; In MCL the menu-type cannot be :pop-up
;;; ... the default
(defmethod set-up-title-menus ((canvas menu-canvas)
                                                   &key
                                                   (title-left        "Information")
                                                   (title-middle      "Edit-display")
                                                   (title-right       "Canvas")
                                                   (title-ctrl-left   "")
                                                   (title-ctrl-middle "")
                                                   (title-ctrl-right  "")
                                                   (menu-type :title)
                                                   (when-selected-fn #'default-when-selected-fn))
     (declare (ignore menu-type))
     (install-title-menus canvas
      :menu-type :pull-down
      :title-left title-left
      :title-middle title-middle
      :title-right title-right
      :title-ctrl-left title-ctrl-left
      :title-ctrl-middle title-ctrl-middle
      :title-ctrl-right  title-ctrl-right
      :when-selected-fn when-selected-fn)
     (put-title-menus-on-menubar canvas))
     |#
