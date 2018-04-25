;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               host-menu-canvas-clx.lisp
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
;;;     R.W. Oldford 1989-1991
;;;     
;;;
;;;
;;;-------------------------------------------------------------------
;;;


(in-package :wb)
(defclass host-menu-canvas ()
  ((menu-bar :initarg :menu-bar
	     :initform NIL
	     :accessor menu-bar-of)))

(defun install-menu-bar (canvas)
  (let ((left (canvas-get-title-menu canvas :key :left))
	(middle (canvas-get-title-menu canvas :key :middle))
	(right (canvas-get-title-menu canvas :key :right))
	(ctrl-left (canvas-get-title-menu canvas :key :ctrl-left))
	(ctrl-middle (canvas-get-title-menu canvas :key :ctrl-middle))
	(ctrl-right (canvas-get-title-menu canvas :key :ctrl-right))
	menus menu-bar)
    (when ctrl-right
      (push ctrl-right menus))
    (when ctrl-middle
      (push ctrl-middle menus))
    (when ctrl-left
      (push ctrl-left menus))
    (when right
      (push right menus))
    (when middle
      (push middle menus))
    (when left
      (push left menus))
    (when menus
      (setf menu-bar (make-instance 'wb-menu
                       :title "" ;;"Canvas Menus"
                       :type :title
                       :font *default-menu-font*
                       ))
      (set-menu-fns menu-bar
                    :selected (when-selected-fn-of (first menus))
                    :held (when-held-fn-of (first menus))
                    :unheld (when-unheld-fn-of (first menus))
                    :sub-item (sub-item-fn-of (first menus)))
      ;; Put menu-bar inside canvas
      ;;
      (xlib::reparent-window
       (window-of menu-bar)
       (host-window canvas)
       0 0)
                              
      (dolist (menu menus)
	(setf (super-menu-of menu) menu-bar)
	(xlib::reparent-window
         (item-window-of menu)
         (window-of menu-bar)
         0 0)
        (setf (sub-menus-alist-of menu-bar)
              (nconc (sub-menus-alist-of menu-bar)
                     (sub-menus-alist-of menu))))
      (push (list (item-window-of menu-bar)
                  menu-bar)
            (sub-menus-alist-of menu-bar))
      (setf (sub-menus-of menu-bar) menus)
      (menu-compute-geometry menu-bar)
      (xlib::unmap-window (item-window-of menu-bar))
      (setf (menu-bar-of canvas) menu-bar)
      (setf (xlib:window-plist (host-window canvas)) ;;(xwindow-of canvas))
	    (acons :wb-class-instance canvas NIL))
      menu-bar)))
