;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               pen-mixin.lisp
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
;;;     C.B. Hurley 1989-1992
;;;     J.A. McDonald 1988-89
;;;     R.W. Oldford 1989-1992
;;;     J.O. Pedersen 1988-89
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(pen-mixin
           pen-width-of pen-operation-of pen-texture-of pen-color-of 
           set-pen-operation set-pen-width
           set-pen-color canvas-set-pen
           pen-default-color
           )))



(defclass pen-mixin ()

  ((pen
    :initarg :pen
    :initform NIL
    :reader pen-of
    :documentation "Current pen used to draw in this canvas.")
   )
   (:documentation
    "Mixin to give a pen to an object."))

(defmethod initialize-instance :after ((self pen-mixin)
                                       &rest initargs
                                       &key pen-color pen-width pen-operation)
  (declare (ignore initargs))
  (unless (pen-of self)
    (setf (slot-value self 'pen)
          (make-instance 'pen)))
  (canvas-set-pen self :width pen-width :operation pen-operation :color pen-color)
  )

;;;--------------------------------------------------------------------------------------
;;;
;;;   setting pen attributes
;;;

(defun set-pen-operation (canvas operation)
  "Sets the operation of the pen."
  (unless (eq operation (pen-operation-of canvas))
    (let ((pen (pen-of canvas)))
      (setf (slot-value pen 'pen-operation) operation)
      (h-draw:set-pen-mode canvas (boole-to-op operation)))))


  
(defun set-pen-width (canvas width)
  "Set the width of canvas's pen to be width units."  
  (unless (and (pen-width-of canvas)
               (= width (pen-width-of canvas)))
    (with-display-mode canvas
      (display-mode-of canvas)
      (set-pen-width canvas width)
      (let ((pen (pen-of canvas)))
        (setf (slot-value pen 'pen-width) width)
        (setf (slot-value pen 'pen-point) (h-draw:make-point width width))
        (h-draw:set-pen-size canvas (pen-point-of pen))))))
  
(defun set-pen-color (canvas color)
  "Set the color of canvas's pen to be color." 
  (unless (eq-colors color (pen-color-of canvas))
    (with-display-mode canvas (display-mode-of canvas)
                       (set-pen-color canvas color)
      (let ((pen (pen-of canvas)))
        (setf (slot-value pen 'pen-color) color)
        (cond 
         ((colored-canvas-p canvas)
          (unless (colorp color)
            (setq color (shade-to-color color)))
          (h-draw:set-pen-color canvas color))
         (t
          (when (colorp color)
            (setq color (color-to-shade color)))
          (h-draw::set-pen-pattern canvas color)))))))

(defmethod (setf pen-of) ((new-pen pen) (self pen-mixin))
  (setf (slot-value self 'pen) new-pen)
  (canvas-set-pen self
                  :width (pen-width-of new-pen)
                  :operation (pen-operation-of new-pen)
                  :color (pen-color-of new-pen)))

(defun canvas-set-pen (canvas
                       &key
                       (width nil)
                       (operation nil)
                       (color nil))
  "Sets up the pen characteristics."

  (unless width (setf width 1))
  (unless operation (setf operation :default))
  (unless color (setf color (pen-default-color canvas)))

  (set-pen-width canvas width)
  (cond  ((colored-canvas-p canvas)
          (unless (colorp color)
            (setq color (shade-to-color color)))
          (set-pen-color canvas color))
         (t (when (colorp color)
              (setq color (color-to-shade color)))
            (with-focused-canvas canvas
              (h-draw:set-pen-pattern canvas color))))
  (set-pen-operation canvas operation)
  )
 

(defmethod pen-width-of ((c pen-mixin))
  (pen-width-of (pen-of c)))

(defmethod pen-operation-of ((c pen-mixin))
  (pen-operation-of (pen-of c)))

(defmethod pen-texture-of ((c pen-mixin))
  (pen-texture-of (pen-of c)))

(defmethod pen-color-of ((c pen-mixin))
  (pen-color-of (pen-of c)))

(defun pen-default-color (canvas)
  (canvas-default-draw-color canvas))
