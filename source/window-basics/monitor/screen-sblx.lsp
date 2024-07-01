;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            screen-sblx.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;  Authors:
;;;     N.G. Bennett 1993
;;;     G.W. Bennett 1996
;;;     
;;;----------------------------------------------------------------------------------
(in-package :wb)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(screen-width screen-height screen-x screen-y)))

#|
(defun screen-height ()
  "Returns the height of the screen in pixels."
  (declare (special *default-display*))
  (xlib::screen-height
   (xlib::display-default-screen
    *default-display*)))

;;; The closest approximations seen to be as follows
;;; gwb feb 26, 1996 - check with rwo

(defun screen-height ()
   "Returns the height of the special variable *screen* in pixels. ~
    the optional t is required to get result in pixels. ~
    the functions used can be applied to any stream."
   ;(declare (special (cg::screen cg::*system*)))
   (let ((scrn (cg::screen cg::*system*)))
    (cg::with-device-context (hdc scrn)
      (cg::page-height (cg::screen cg::*system*) t))
     ))
|#

(defun screen-height ()
   "Returns the height of the special variable *screen* in pixels. ~
    the optional t is required to get result in pixels. ~
    the functions used can be applied to any stream."
   ;(declare (special (cg::screen cg::*system*)))
   (graft-height (find-graft))
   )

#|
(defun screen-width ()
  "Returns the width of the screen in pixels."
  (declare (special *default-display*))
  (xlib::screen-width
   (xlib::display-default-screen
    *default-display*)))

(defun screen-width ()
   "Returns the width of the special variable *screen* in pixels. ~
    The optional t is required to get the result in pixels. ~
    the functions used can be applied to any stream."
   ;(declare (special (cg::screen cg::*system*)))
   (let ((scrn (cg::screen cg::*system*)))
    (cg::with-device-context (hdc scrn)
      (cg::page-width (cg::screen cg::*system*) t))
   ))
|#

(defun screen-width ()
   "Returns the width of the special variable *screen* in pixels. ~
    The optional t is required to get the result in pixels. ~
    the functions used can be applied to any stream."
  ;(declare (special (cg::screen cg::*system*)))
   (graft-width (find-graft)))

(defun screen-to-host-y (y) (- (screen-height) y))

(defun host-to-screen-y (y) (- (screen-height) y))


 (defun exterior-frame-rect (canvas)
  "Returns the exterior rectangle of canvas ~
  in screen coordinates x L -> R, y T -> B."
       (transform-region (sheet-delta-transformation (frame-top-level-sheet canvas) (graft canvas))
                          (sheet-region (frame-top-level-sheet canvas))))

 (defun screen-x (canvas)
  "The x-coordinate of the bottom left of the EXTERIOR ~
  of canvas as a fixnum."
  (rectangle-min-x (exterior-frame-rect canvas)))

 (defun screen-y (canvas)
  "The y-coordinate of the bottom left of the EXTERIOR ~
  of the canvas as a fixnum."
 (host-to-screen-y (rectangle-max-y (exterior-frame-rect canvas))))


#| 
(defun screen-x (canvas)
   "The x-coordinate of the bottom left of the EXTERIOR ~
    of canvas as a fixnum."
   (cg::box-left (cg::exterior canvas))) 


;; Poach from -mcl
(defun screen-y (canvas)
   "The y-coordinate of the bottom left of the EXTERIOR ~
    of canvas as a fixnum."
  (host-to-screen-y (cg::box-bottom (cg::exterior canvas))))
|#
