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

;;;--------------------------------------------------------------------------------------
;;;
;;;   setting pen attributes
;;;

(defun set-pen-operation (canvas operation)
  "Sets the operation of the pen."
  (unless (eq operation (pen-operation-of canvas))
    (let ((pen (pen-of canvas)))
      (setf (slot-value pen 'pen-operation) operation)
      (h-draw:set-pen-mode canvas (boole-to-op operation))
 )))

;;; Here is fix-pen-size, a copy of h-draw:set-pen-size but in :wb
#+:sbcl-linux(defun fix-pen-size (canvas h &optional v)
  ;#-:sbcl(declare (inline h-draw::point-x h-draw::point-y))
  (let* ((its-pane (clim::get-frame-pane canvas 'host-pane)) ;,_ from 'wb::host-pane 09FE21
      (its-line-style (clim:medium-line-style its-pane))
    (its-unit (clim:line-style-unit its-line-style))
    (its-dashes (clim:line-style-dashes its-line-style))
    (its-joint-shape (clim:line-style-joint-shape its-line-style))
    (its-cap-shape (clim:line-style-cap-shape its-line-style))
    (new-thickness (if v (floor (+ h v) 2)
      (floor (+ (h-draw::point-x h) (h-draw::point-y h)) 2)))
    (new-style (clim:make-line-style :unit its-unit :thickness new-thickness :dashes its-dashes
      :joint-shape its-joint-shape :cap-shape its-cap-shape))
    )
  (setf (clim:medium-line-style its-pane) new-style)
  )
  )

;;;

  
#-:sbcl-linux(defun set-pen-width (canvas width)
  "Set the width of canvas's pen to be width units."
  (unless (and (pen-width-of canvas)
               (= width (pen-width-of canvas)))
    (with-display-mode canvas
      (display-mode-of canvas)
      (set-pen-width canvas width)
      (let ((pen (pen-of canvas)))
        (setf (slot-value pen 'pen-width) width)
        (setf (slot-value pen 'pen-point) (h-draw:make-point width width))
        (h-draw:set-pen-size canvas (pen-point-of pen))
        )))
  )

#+:sbcl-linux(defun set-pen-width (canvas width)
  "Set the width of canvas's pen to be width units."
  (unless (and (pen-width-of canvas)
               (= width (pen-width-of canvas)))
    (with-display-mode canvas
      (display-mode-of canvas)
      (set-pen-width canvas width)
      (let ((pen (pen-of canvas)))
        (setf (slot-value pen 'pen-width) width)
        (setf (slot-value pen 'pen-point) (h-draw:make-point width width))4
        (h-draw:set-pen-size canvas (pen-point-of pen))
        )))
  )

;;; Here is fix-pen-color, a copy of h-draw:set-pen-color but here in :wb
#+:sbcl-linux(defun fix-pen-color (canvas new-color)
  "Sets the drawing color of canvas to (Q)new-color"
  (let ((mp (clim::get-frame-pane canvas 'wb::host-pane)))
    (setf (clim:medium-ink mp)  new-color))) ;; 18DEC2024

;;;
  
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
          #-:sbcl-linux(h-draw:set-pen-color canvas color)
          #+:sbcl-linux(fix-pen-color canvas color) ;; copy of h-draw:set-pen-color but in :wb
          )
         (t
          (when (colorp color)
            (setq color (color-to-shade color)))
          (h-draw::set-pen-pattern canvas color)))
        )))
  )

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
