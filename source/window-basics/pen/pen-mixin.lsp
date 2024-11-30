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

#| START out 05MAR2021
 (defmethod initialize-instance :after ((self pen-mixin)
                                       &rest initargs
                                       &key pen-color pen-width pen-operation)
  (declare (ignore initargs))
 (sleep 10) ;; 05MAR2021
  (format t "~%i-i input pen-color is ~s " pen-color)
  (format t "~%i-i input pen-width is ~s " pen-width)
  (format t "~%i-i input pen-operation is ~s " pen-operation)
  (format t "~%i-i input self is ~s " self)
  (format t "~%i-i is self a frame ~s , a colored-canvas ~s " (clim:application-frame-p self) (colored-canvas-p self))
  (format t "~%i-i does it have a host-pane ~s " (clim:get-frame-pane self 'host-pane))
  (unless (pen-of self)
    (setf (slot-value self 'pen)
          (make-instance 'pen)))
  (canvas-set-pen self :width pen-width :operation pen-operation :color pen-color)
  )
|# ;END out 05MAR2021
;;;--------------------------------------------------------------------------------------
;;;
;;;   setting pen attributes
;;;

(defun set-pen-operation (canvas operation)
  "Sets the operation of the pen."
  ;(format t "~%Just inside set-pen-operation")
  ;(format t "~%input operation is ~s " operation)
  (unless (eq operation (pen-operation-of canvas))
    (let ((pen (pen-of canvas)))
      (setf (slot-value pen 'pen-operation) operation)
      (h-draw:set-pen-mode canvas (boole-to-op operation))
       ;(format t "~%out going operation is ~s " (slot-value pen 'pen-operation))))
 )))

;;; Here is fix-pen-size, a copy of h-draw:set-pen-size but in :wb
#+:sbcl-linux(defun fix-pen-size (canvas h &optional v)
  ;#-:sbcl(declare (inline h-draw::point-x h-draw::point-y))
  ;(format t "~%Just inside set-pen-size")
  ;(format t "~%f-p-s h is ~s " h)
  ;(format t "~%f-p-s v is ~s " v)
  ;(format t "~%f-p-s canvas is ~s " canvas)
  ;(format t "~%f-p-s is it a frame ? ~s " (clim:application-frame-p canvas))
  ;(format t "~%f-p-s its host-pane pane is ~s " (clim:get-frame-pane canvas 'host-pane))
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
    ;(format t "~%f-p-s In set-pen-size after defvar new-thickness check current values BEFORE make-line-style")
    ;(format t "~%f-p-s input canvas is ~s " canvas)
    ;(format t "~%f-p-s is canvas a frame ? ~s " (clim:application-frame-p canvas))
    ;(format t "~%f-p-s input h is ~s " h)
    ;(format t "~%f-p-s input v is ~s " v)
    ;(format t "~%f-p-s its-pane is ~s " its-pane)
    ;(format t "~%f-p-s its-line-style is ~s " its-line-style)
    ;(format t "~%f-p-s its-unit is ~s " its-unit)
    ;(format t "~%f-p-s its-dashes is ~s " its-dashes)
    ;(format t "~%f-p-s its-joint-shape is ~s " its-joint-shape)
    ;(format t "~%f-p-s its-cap-shape is ~s " its-cap-shape)
    ;(format t "~%f-p-s new-thickness is ~s " new-thickness)
    ;(format t "~%f-p-s new-style is ~s " new-style)
  (setf (clim:medium-line-style its-pane) new-style)
  )
  )

;;;

  
#-:sbcl-linux(defun set-pen-width (canvas width)
  "Set the width of canvas's pen to be width units."
  ;(format t "~%Just inside set-pen-width")
  ;(format t "~%s-p-w input width is ~s " width)
  ;(format t "~%s-p-w canvas is ~s " canvas)
  ;(format t "~%s-p-w is canvas a frame ? ~s " (clim:application-frame-p canvas))
  ;(format t "~%s-p-w does canvas have a host-pane ? ~s " (clim:get-frame-pane canvas 'host-pane))
  ;(format t "~%s-p-w pen-of canvas is ~s " (pen-of canvas))
  ;(format t "~%s-p-w pen-width-of pen-of canvas is ~s " (pen-width-of (pen-of canvas)))
  ;(format t "~%s-p-w pen-width-of canvas is ~s " (pen-width-of canvas))  
  (unless (and (pen-width-of canvas)
               (= width (pen-width-of canvas)))
    (with-display-mode canvas
      (display-mode-of canvas)
      (set-pen-width canvas width)
      (let ((pen (pen-of canvas)))
        ;(format t "~%s-p-w pen is ~s " pen)
        (setf (slot-value pen 'pen-width) width)
        ;(format t "~%s-p-w slot-value pen 'pen-width is ~s " (slot-value pen 'pen-width))
        (setf (slot-value pen 'pen-point) (h-draw:make-point width width))
        ;(format t "~%s-p-w slot-value pen 'pen-point is ~s " (slot-value pen 'pen-point))
        ;(fix-pen-size canvas (pen-point-of pen))
        (h-draw:set-pen-size canvas (pen-point-of pen))
        ;(format t "~%out going width is ~s " (slot-value pen 'pen-width))
        )))
  )

#+:sbcl-linux(defun set-pen-width (canvas width)
  "Set the width of canvas's pen to be width units."
  (format t "~%Just inside set-pen-width")
  (format t "~%s-p-w input width is ~s " width)
  (format t "~%s-p-w canvas is ~s " canvas)
  (format t "~%s-p-w is canvas a frame ? ~s " (clim:application-frame-p canvas))
  (format t "~%s-p-w does canvas have a host-pane ? ~s " (clim:get-frame-pane canvas 'host-pane))
  (format t "~%s-p-w pen-of canvas is ~s " (pen-of canvas))
  (format t "~%s-p-w pen-width-of pen-of canvas is ~s " (pen-width-of (pen-of canvas)))
  (format t "~%s-p-w pen-width-of canvas is ~s " (pen-width-of canvas))  
  (unless (and (pen-width-of canvas)
               (= width (pen-width-of canvas)))
    (with-display-mode canvas
      (display-mode-of canvas)
      (set-pen-width canvas width)
      (let ((pen (pen-of canvas)))
        (format t "~%s-p-w pen is ~s " pen)
        (setf (slot-value pen 'pen-width) width)
        (format t "~%s-p-w slot-value pen 'pen-width is ~s " (slot-value pen 'pen-width))
        (setf (slot-value pen 'pen-point) (h-draw:make-point width width))
        (format t "~%s-p-w slot-value pen 'pen-point is ~s " (slot-value pen 'pen-point))
        ;(fix-pen-size canvas (pen-point-of pen))  ;;2 lines status changed 18OCT2024
        (h-draw:set-pen-size canvas (pen-point-of pen))
        (format t "~%out going width is ~s " (slot-value pen 'pen-width))
        )))
  )

;;; Here is fix-pen-color, a copy of h-draw:set-pen-color but here in :wb
#+:sbcl-linux(defun fix-pen-color (canvas new-color)
  "Sets the drawing color of canvas to (Q)new-color"
  (let ((mp (clim::get-frame-pane canvas 'wb::host-pane)))
    ;(format t "~% Just inside h-draw:set-pen-color")
    ;(format t "~%s-p-c new-color is ~s " new-color)
    ;(format t "~%s-p-c canvas is ~s " canvas)
    ;(format t "~%s-p-c is canvas a frame ? ~s " (clim:application-frame-p canvas))
    ;(format t "~%s-p-c does canvas have a host-pane ~s " (clim:get-frame-pane canvas 'host-pane))
    (setf (clim:medium-foreground mp)  new-color)))

;;;
  
(defun set-pen-color (canvas color)
  "Set the color of canvas's pen to be color." 
  ;(format t "~%Just inside set-pen-color")
  ;(format t "~%s-p-c input color is ~s " color)
  (unless (eq-colors color (pen-color-of canvas))
    (with-display-mode canvas (display-mode-of canvas)
                       (set-pen-color canvas color)
      (let ((pen (pen-of canvas)))
        ;(format t "~%s-p-c just in let pen is ~s " pen)
        (setf (slot-value pen 'pen-color) color)
        (cond 
         ((colored-canvas-p canvas)
          (unless (colorp color)
            (setq color (shade-to-color color)))
          #-:sbcl-linux(h-draw:set-pen-color canvas color)
          #+:sbcl-linux(fix-pen-color canvas color) ;; copy of h-draw:set-pen-color but in :wb
          ;(format t "~%s-p-c just after fix-pen-color")
          )
         (t
          (when (colorp color)
            (setq color (color-to-shade color)))
          (h-draw::set-pen-pattern canvas color)))
        ;(format t "~%s-p-c just out of cond")
        ;(format t "~%out going color is ~s " (slot-value pen 'pen-color))
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
  (format t "~%Just inside canvas-set-pen")
  (format t "~%c-s-p input width is ~s " width)
  (format t "~%c-s-p input operation is ~s " operation)
  (format t "~%c-s-p input color is ~s " color)
;OK
  (unless width (setf width 1))
  (unless operation (setf operation :default))
  (unless color (setf color (pen-default-color canvas)))
  (format t "~%c-s-p after unlesses width is ~s " width)
  (format t "~%c-s-p after unlesses operation is ~s " operation)
  (format t "~%c-s-p after unlesses color is ~s " color)
;OK
  (set-pen-width canvas width)
  (format t "~%c-s-p After set-pen-width")
  (format t "~%c-s-p pen-width-of pen-of canvas is ~s " (pen-width-of (pen-of canvas))) 
  (format t "~%c-s-p pen-with-of canvas is ~s " (pen-width-of canvas))
  (cond  ((colored-canvas-p canvas)
          (unless (colorp color)
            (setq color (shade-to-color color)))
          (set-pen-color canvas color))
         (t (when (colorp color)
              (setq color (color-to-shade color)))
            (with-focused-canvas canvas
              (h-draw:set-pen-pattern canvas color))))
  ;(format t "~%c-s-p After cond for color")
  ;(format t "~%c-s-p pen-color-of pen-of canvas is ~s " (pen-color-of (pen-of canvas)))
  (set-pen-operation canvas operation)
  ;(format t "~%c-s-p Finally, for pen-operation")
  ;(format t "~%c-s-p pen-operation-of pen-of canvas is ~s " (pen-operation-of (pen-of canvas)))
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
