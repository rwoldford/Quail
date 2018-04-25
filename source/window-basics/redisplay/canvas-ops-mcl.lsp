;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               canvas-ops-mcl.lisp
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
;;;     R.W. Oldford 1989-1992
;;;     J.O. Pedersen 1988-89
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(;;size-changed-p
           canvas-visible-p
           at-top-p close-canvas hide-canvas show-canvas
           canvas-width canvas-height
           canvas-title  canvas-set-title which-canvas move-canvas
           shape-canvas
           canvas-to-top canvas-at-top canvases )))

#|
(defun size-changed-p (display)                           ; cbh
  ;; for a particular kind of display, redefine this
  (declare (ignore display)) T)
|#

(defun at-top-p (canvas)
  "Test whether the canvas is the top-most in the display."
  (eq canvas (first (ccl:windows))))

(defun canvas-visible-p (canvas)
  "Test whether the canvas is visible in the display."
  (ccl::wptr canvas))

(defun close-canvas (canvas)
  "Close the canvas if it's open."
  (ccl:window-close canvas))

(defun hide-canvas (canvas)
  "Hides the canvas from display."
  (ccl::window-hide canvas)
  )

(defun show-canvas (canvas)
  "Displays the canvas."
  (ccl:window-show canvas)
  )


(defun canvas-width  (canvas)
  "Returns the display width of the canvas (in pixels)."
  (- (h-draw:point-x (view-size canvas)) 15))

(defun canvas-height (canvas)
  "Returns the display height of the canvas (in pixels)."
  (- (h-draw:point-y (view-size canvas)) 15))


;;;============================================================
;;; Canvas properties
;;;============================================================


(defun canvas-to-top (window)
  "Brings the canvas to the front of all other windows."
  (ccl:window-select window))

(defun canvas-at-top ()
  "Returns the canvas at the front of all other canvases."
  (first (canvases)))

(defun canvases ()
  "Returns a list of all existing canvases."
  (ccl:windows :class 'canvas))

(defun which-canvas (screen-position)
  "Returns the canvas at the screen-position, if it exists."
  
  ;;loop over all windows, find the frontmost one containing position
  ;; return it if it is a canvas
  
  (find-if #'(lambda (w) 
               (inside-p (canvas-screen-region w) screen-position))
           (canvases)))

(defun canvas-title (canvas)
  (ccl:window-title canvas)
  )

(defsetf canvas-title (canvas) (new-value)
  `(let ((my-new-value ,new-value))
     (ccl:set-window-title ,canvas my-new-value)
     my-new-value)
  )

(defun canvas-set-title (c title)
  (setf (canvas-title c) title))

(defun move-canvas (canvas x &optional y)
  "Reposition the canvas to the position (x y) in screen coordinates.  ~
   If only one argument is given then it is assumed to be a position."
  (cond
   ((integerp y)
    (ccl::set-view-position canvas x (screen-to-host-y y)))
   (T
    (ccl::set-view-position canvas
                            (position-x x) (screen-to-host-y (position-y x))))))

(defun shape-canvas (canvas width &optional height)
  "Resize a canvas to have width and height as given.  If the optional ~
   argument height is not supplied, then it is assumed that the first argument  ~
   is either a position specifying the width and height as its x and y coordinates, ~
   or a region whose width and height specify the corresponding attributes of the canvas."
  
  (cond
   (height
    (ccl:set-view-size canvas width height))
   ((position-p width)
    (ccl:set-view-size canvas (position-x width) (position-y width)))
   ((region-p width)
    (ccl:set-view-size canvas
                       (region-width width)
                       (region-height width)))
   (T (ccl:set-view-size
       canvas
       (min width (window-max-width))
       (min width (window-max-height))))))
  ;; Following should be unnecessary.  Just a hack to make sure that the old region
  ;; gets redisplayed properly.
 ;; (redisplay canvas)
 ;; )

