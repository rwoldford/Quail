;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                               canvas-ops-pc.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;  Authors:
;;;     G.W. Bennett 1996
;;;     R.W. Oldford 1996
;;;     
;;;----------------------------------------------------------------------------------
;;; Changed defsetf on canvas-title to use 
;;; cg::set-stream-title
(in-package :wb)
(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(;;size-changed-p
           canvas-visible-p
           at-top-p close-canvas hide-canvas show-canvas
           canvas-width canvas-height
           canvas-title canvas-set-title which-canvas move-canvas
           shape-canvas
           canvas-to-top canvas-at-top canvases )))

#|
(defun size-changed-p (display)                           ; cbh
  ;; for a particular kind of display, redefine this
  (declare (ignore display)) T)
|#

(defun at-top-p (canvas)
  "Test whether the canvas is the top-most in the display."
   (declare (special *quail-menubar-window*))
   (let ((fw (first (canvases));(cg::front-window (cg::screen cg::*system*)) 08JUN2021
    ))
     (and (canvas-p canvas)
          (not (member 
                ;(cg::window-state canvas) 19oct05
                (cg::state canvas) ;19oct05
                 (list :shrunk :icon)))
          (or (eq canvas fw)
              (and (eq *quail-menubar-window* fw)
                   (eq canvas
                     (cg::next-window *quail-menubar-window*))))))
   )
              

(defun canvas-visible-p (canvas)   
  "Test whether the canvas is visible in the display."
  (eql (cg::state canvas) :normal))

(defun close-canvas (canvas)
  "Close the canvas if it's open."
  (cg::close canvas))


(defun hide-canvas (canvas)
  "Hides the canvas from display."
  (cg::shrink-window canvas T))
   
(defun show-canvas (canvas)
  "Displays the canvas."
  (cg::select-window canvas)
   )
   
(defun canvas-width  (canvas)
  "Returns the display width of the canvas (in pixels)."
  (- (cg::visible-box-width canvas) (cg::vertical-scrollbar-thickness))) ;;05may06

(defun canvas-height (canvas)
  "Returns the display height of the canvas (in pixels)."
  (- (cg::visible-box-height canvas) (cg::horizontal-scrollbar-thickness))) ;;05may06

;;;============================================================
;;; Canvas properties
;;;============================================================
(defun canvas-to-top (window)
  "Brings the canvas to the front of all other windows."
  (cg::select-window window))

(defun canvas-at-top ()
  "Returns the canvas at the front of all other canvases."
  (first (canvases)))

(defun canvases ()
  "Returns a list of all existing canvases."
  (loop for w in (cg::windows (cg::screen cg::*system*))
    when (wb::canvas-p w)
    ; when (typep w 'canvas) ;; Changed Nov 19+ 1997
     collect w))

(defun which-canvas (screen-position)
  "Returns the canvas at the screen-position, if it exists."
  ;;loop over all windows, find the frontmost one containing position
  ;; return it if it is a canvas
  (find-if #'(lambda (w) 
               (inside-p (canvas-screen-region w) screen-position))
           (canvases)))

(defun canvas-title (canvas)
  (cg::title canvas))
   
;;; Changed 18 Feb 1998 from (cg::set-stream-title to (setf (cg::stream-title
(defsetf canvas-title (canvas) (new-value)
  `(let ((my-new-value ,new-value))
      (setf (cg::title ,canvas) my-new-value)
     )
   )


(defun canvas-set-title (c title)
   "Sets the canvas title to be the value of the second argument ~
    title must be a string."
   (setf (cg::title  c) title))

(defun move-canvas (canvas x &optional y)
     "Reposition the canvas to the position (x y) in screen coordinates.  ~
   If only one argument is given then it is assumed to be a position."
     (cond
               ((integerp y)
                (cg::move-window canvas (cg::make-position x 
     (screen-to-host-y y))))
               (T
                  (cg::move-window canvas
                   (cg::make-position (position-x x) 
 (screen-to-host-y (position-y x)))))))

(defun shape-canvas (canvas width &optional height)
     "Resize a canvas to have width and height as given.  If the optional ~
argument height is not supplied, then it is assumed that the first argument  ~
is either a position specifying the width and height as its x and y coordinates, ~
or a region whose width and height specify the corresponding attributes of the canvas."
     (cond
               (height
                (cg::resize-window canvas (cg::make-position width height))
                )
               ((position-p width)
                (cg::resize-window canvas width))
               ((region-p width)
                (cg::resize-window  canvas
                 (cg::make-position (region-width width)
                  (region-height width))))
               (T
                (cg::resize-window canvas 
                 (cg::make-position (min width (window-max-width))
                  (min width (window-max-height)))))))