;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               canvas.lisp
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
;;;     C.B. Hurley 1989-1991
;;;     J.A. McDonald 1988-89
;;;     R.W. Oldford 1989-1992
;;;     J.O. Pedersen 1988-89
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------
;;; This is ~/RESERVE/new-quail/window-basics/canvas/canvas.lsp
;;; As part of a new-build



(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(*default-canvas-region* set-up-default-canvas-region *current-canvas*
           canvas canvas-p canvas-font)))


(defclass canvas (color-mixin
                  pen-mixin
                  postscript-canvas
                  canvas-button
                  menu-canvas
                  canvas-redisplay-mixin
                  host-window
                  font-mixin
                  #+:sbcl-linux sheet
                  )

  ()
   (:documentation
    "Canvas is a window on which one can draw."))

(defun canvas-p (canvas)
  "Returns T if the argument is a canvas, NIL otherwise."
  (typep canvas 'canvas))

(defun current-canvas-p (canvas)
  (eq canvas *current-canvas*))
;;;
;;;  SETTING FONTS
;;;
(defmethod (setf canvas-font) (new-font (self canvas))
  (let ((result (call-next-method)))
    (with-display-mode self
                       (display-mode-of self)
                       ;; set-canvas-font is just a bogus function so that
                       ;; printer-set-canvas-font and ps-set-canvas-font
                       ;; are used in this macro
                       (set-canvas-font self new-font)
      )
    result))

(defun set-canvas-font (canvas new-font)
  (declare (ignore canvas new-font))
  )
