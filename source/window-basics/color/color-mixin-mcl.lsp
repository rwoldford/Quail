;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               color-mixin-mcl.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(canvas-set-background-color
           canvas-background-color
           colored-canvas-p
           canvas-default-draw-color)))

(defclass color-mixin ()

  ((color?
    :initarg :color?
    :initform NIL
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
    (when (and (colorp color) (colored-canvas-p canvas))
      ;;(set-back-color canvas color)
      (ccl:set-part-color canvas :content color)
      )))

(defun canvas-background-color (canvas)
 (declare (special *white-shade*))
 (if (colored-canvas-p canvas)
    (ccl:get-back-color canvas)
    *white-shade*))

(defun canvas-default-draw-color (canvas)
  (declare (special *black-color* *white-color*
                    *black-shade*))
  (if (colored-canvas-p canvas)
    (or *default-canvas-pen-color*
        (if (eql (canvas-background-color canvas) *black-color*)
          *white-color*
          *black-color*)
        )
    *black-shade*))
