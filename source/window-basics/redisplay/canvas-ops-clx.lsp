;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               canvas-ops-clx.lisp
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
;;;     N.G. Bennett 1993.
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(;;size-changed-p
           canvas-visible-p
           at-top-p close-canvas hide-canvas show-canvas
           canvas-width canvas-height
           canvas-title move-canvas
           shape-canvas
           canvas-to-top)))

#|
(defun size-changed-p (display)                           ; cbh
  ;; for a particular kind of display, redefine this
  (declare (ignore display)) T)
|#

(defun at-top-p (canvas)
  "Test whether the canvas is the top-most in the display."
  nil) 

(defun canvas-visible-p (canvas)
  "Test whether the canvas is visible in the display."
  (eq (xlib:window-map-state (host-window canvas)) :viewable)
  )

(defun close-canvas (canvas)
  "Close the canvas if it's open."
  (xlib:destroy-window (host-window canvas))
  )

(defun hide-canvas (canvas)
  "Hides the canvas from display."
  (xlib:unmap-window (host-window canvas))
  )

(defun show-canvas (canvas)
  "Displays the canvas."
  (xlib:map-window (host-window canvas))
  )


(defun canvas-width  (canvas)
  "Returns the display width of the canvas (in pixels)."
  (xlib:drawable-width (host-window canvas))
  )

(defun canvas-height (canvas)
  "Returns the display height of the canvas (in pixels)."
  (xlib:drawable-height (host-window canvas))
  )


;;;============================================================
;;; Canvas properties
;;;============================================================


(defun canvas-to-top (window)
  "Brings the canvas to the front of all other windows."
  (setf (xlib:window-priority window) :above)
  )

(defun canvas-title (canvas)
  (xlib:wm-name (host-window canvas))
  )

(defsetf canvas-title (canvas) (new-value)
  `(let ((my-new-value ,new-value))
     (setf (xlib:wm-name (host-window ,canvas)) my-new-value
	   (xlib:wm-icon-name (host-window ,canvas)) my-new-value)
     my-new-value)
  )

(defun move-canvas (canvas x &optional y)
  "Reposition the canvas to the position (x y) in screen coordinates.  ~
   If only one argument is given then it is assumed to be a position."
  (if y
    (setf (xlib:drawable-x (host-window canvas)) x
	  (xlib:drawable-y (host-window canvas)) y)
    (setf (xlib:drawable-x (host-window canvas)) (position-x x)
	  (xlib:drawable-y (host-window canvas)) (position-y x))))

(defun shape-canvas (canvas width &optional height)
  "Resize a canvas to have width and height as given.  If the optional ~
   argument height is not supplied, then it is assumed that the first argument  ~
   is either a position specifying the width and height as its x and y coordinates, ~
   or a region whose width and height specify the corresponding attributes of the canvas."
  
  (cond
   (height
    (setf (xlib:drawable-width (host-window canvas)) width
	  (xlib:drawable-height (host-window canvas)) height))
   ((position-p width)
    (setf (xlib:drawable-width (host-window canvas)) (position-x width) 
          (xlib:drawable-height (host-window canvas)) (position-y width)))
   ((region-p width)
    (setf (xlib:drawable-width (host-window canvas)) (region-width width)
          (xlib:drawable-height (host-window canvas)) (region-height width)))
   (T (set-view-size canvas width))))
