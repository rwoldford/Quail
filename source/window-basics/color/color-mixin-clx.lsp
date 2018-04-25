;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               color-mixin-clx.lisp
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
;;;     R.W. Oldford 1994
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(canvas-set-background-color
           canvas-background-color
           colored-canvas-p
           canvas-default-draw-color)))

(defclass color-mixin ()

  ((color?
    :initarg :color?
    :initform T
    :reader colored-canvas-p
    :documentation "Value is T when ~
                    the canvas is a color one, NIL otherwise.")
   )
   (:documentation
    "A mixin to allow a test for color."))

;;;
;;;  Setting the background and drawing colour/shading of a canvas.
;;;


(defun canvas-set-background-color (canvas color)
  (with-display-mode canvas (display-mode-of canvas) 
                     (canvas-set-background-color canvas color)
    (let* ((gcontext (gcontext canvas))
	   (display (xlib::gcontext-display gcontext))
	   (screen (xlib::display-default-screen display))
	   (colormap (xlib::screen-default-colormap screen))
	   (my-new-color
            (xlib::alloc-color colormap color)))
      (setf (xlib::gcontext-background gcontext) my-new-color)
      (setf (xlib::window-background (host-window canvas)) my-new-color)
      color
      )))

    ;;(when (and (colorp color) (colored-canvas-p canvas))
    ;;   (setf (xlib::window-background (host-window canvas)) color)
    ;;   (setf (xlib::gcontext-background (gcontext canvas)) color) 
    ;;  )))

(defun canvas-background-color (canvas)
  (let* ((gcontext (gcontext canvas))
         (display (xlib::gcontext-display gcontext))
         (screen (xlib::display-default-screen display))
         (colormap (xlib::screen-default-colormap screen))
         (my-pixel (xlib::gcontext-background gcontext)))
    (first (xlib::query-colors colormap (list my-pixel)))))
      
(defun canvas-default-draw-color (canvas)
  (declare (special *black-color* *white-color*))
  (or *default-canvas-pen-color*
        (if (eql (canvas-background-color canvas) *black-color*)
          *white-color*
          *black-color*)
        ))
