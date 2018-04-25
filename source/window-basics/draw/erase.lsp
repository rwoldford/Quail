;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               erase.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(canvas-erase-to canvas-erase-line
           canvas-erase-rectangle canvas-erase-filled-rectangle
           canvas-erase-inside-rectangle
           canvas-erase-circle canvas-erase-filled-circle
           canvas-erase-polygon canvas-erase-filled-polygon
           canvas-erase-string erase-rect&corners
           canvas-erase-arc canvas-erase-filled-arc)))

(defun canvas-erase-to (canvas x y &key width dashing operation)
  (with-focused-canvas canvas
    (let ((pen-color (pen-color-of canvas)))
      (set-pen-color canvas (canvas-background-color canvas))
      (canvas-draw-to canvas x y :width width :dashing dashing :operation operation)
      (set-pen-color canvas pen-color))))

(defun canvas-erase-line (canvas x1 y1 x2 y2
                             &key (width NIL) dashing operation)
  
  (with-focused-canvas canvas
    (canvas-move-to canvas x1 y1)
    (canvas-erase-to canvas x2 y2 :width width :dashing dashing :operation operation)))


(defun canvas-erase-rectangle (canvas x1 x2 y1 y2
                                  &key 
                                  (width NIL)
                                  (operation NIL)
                                  dashing)
  (with-focused-canvas canvas
    (let ((pen-color (pen-color-of canvas)))
      (set-pen-color canvas (canvas-background-color canvas))
      (canvas-draw-rectangle  canvas x1 x2 y1 y2
                              :width width :dashing dashing :operation operation)
      (set-pen-color canvas pen-color))
    ))



(defun canvas-erase-filled-rectangle (canvas left right bottom top
                                           &key 
                                           (operation (pen-operation-of canvas))
                                           ;;(pattern *black-shade*)
                                           )
  (with-focused-canvas canvas
    (let ((pen-color (pen-color-of canvas)))
      (set-pen-color canvas (canvas-background-color canvas))
      (canvas-draw-filled-rectangle
       canvas left right bottom top
       :operation operation)
      (set-pen-color canvas pen-color))))

(defun canvas-erase-inside-rectangle (canvas left right bottom top   
                                           &key 
                                           (width NIL)
                                           (operation NIL)
                                           dashing)
  (with-focused-canvas canvas
    (let ((pen-color (pen-color-of canvas)))
      (set-pen-color canvas (canvas-background-color canvas))
      (canvas-draw-inside-rectangle
       canvas left right bottom top
       :width width :dashing dashing :operation operation)
      (set-pen-color canvas pen-color))
    ))

(defun canvas-erase-circle (canvas x y radius
                              &key 
                              ;;(pattern *black-shade*)
                              (width NIL)
                              (operation NIL))
  (with-focused-canvas canvas
    (let ((pen-color (pen-color-of canvas)))
      (set-pen-color canvas (canvas-background-color canvas))
      (canvas-draw-circle canvas x y radius
                          :width width :operation operation)
      (set-pen-color canvas pen-color))))



(defun canvas-erase-filled-circle (canvas x y radius
                                     &key 
                                     (operation NIL)
                                     ;;(pattern *black-shade*)
                                     )
  (with-focused-canvas canvas
    (let ((pen-color (pen-color-of canvas)))
      (set-pen-color canvas (canvas-background-color canvas))
      (canvas-draw-filled-circle canvas x y radius :operation operation)
      (set-pen-color canvas pen-color))))




(defun canvas-erase-polygon (canvas list-of-points 
                                    &key
                                    (width NIL)
                                    (operation NIL)
                                    )
  
  (with-focused-canvas canvas
    (let ((pen-color (pen-color-of canvas)))
      (set-pen-color canvas (canvas-background-color canvas))
      (canvas-draw-polygon canvas list-of-points
                           :width width :operation operation)
      (set-pen-color canvas pen-color))))



(defun canvas-erase-filled-polygon 
       (canvas list-of-points
               &key
               (operation NIL)
               ;;(pattern *black-shade* )
               )
  
  
  (with-focused-canvas canvas
    (let ((pen-color (pen-color-of canvas)))
      (set-pen-color canvas (canvas-background-color canvas))
      (canvas-draw-filled-polygon canvas list-of-points :operation operation)
      (set-pen-color canvas pen-color))))

(defun canvas-erase-string
       (canvas string
               &key
               (font nil)
               region
               left bottom width height
               (orientation NIL)
               (justification NIL)
               (clip? NIL)
               color)
  "Erases the string in the canvas.  ~
   Erase the string in the given region (or that determined by left bottom ~
   width and height) of canvas.  If no region specs are given, the string is ~
   erased at the current pen position.~
   Justification can be :left or :right to justify horizontal position, ~
   :top or :bottom to justify vertical position, or a list of a vertical and/or ~
   horizontal justification to specify both directions.  Default justification is ~
   :left horizontally and :bottom vertically.  ~
   If clip? is non-NIL, the string is clipped to the specified region."
  (with-focused-canvas canvas
    (let ((pen-color (pen-color-of canvas)))
      (set-pen-color canvas (canvas-background-color canvas))
      (canvas-draw-string canvas string :font font :region region
                          :left left :bottom bottom
                          :width width :height height
                          :orientation orientation
                          :justification justification
                          :clip? clip?
                          :color color)
      (set-pen-color canvas pen-color))))

(defun erase-rect&corners (canvas rect &key (corner-width 10)
                                 (width NIL)
                                 (operation NIL))
  "Erase squares of size corner-width in the corners of rect."


  (let ((d corner-width)
        vl vb vr vt)
    (multiple-value-setq (vl vr vb vt) (region-bounds rect))
    (when (and (> (- vr vl) d)
               (> (- vt vb) d))
      (canvas-erase-rectangle canvas vl (+ vl d) vb (+ vb d)
                              :width width :operation operation)
      (canvas-erase-rectangle canvas vr (- vr d) vb (+ vb d)
                              :width width :operation operation)
      (canvas-erase-rectangle canvas vl (+ vl d) vt (- vt d)
                              :width width :operation operation)
      (canvas-erase-rectangle canvas vr (- vr d) vt (- vt d)
                              :width width :operation operation))
    (canvas-erase-rectangle canvas vl vr vb vt
                           :width width :operation operation )))



(defun canvas-erase-arc (canvas start-angle arc-angle
                                x-centre y-centre
                                x-radius y-radius
                                &key
                                (width NIL)
                                (operation NIL))
  (with-focused-canvas canvas
    (let ((pen-color (pen-color-of canvas)))
      (set-pen-color canvas (canvas-background-color canvas))
      (canvas-draw-arc canvas
                       start-angle arc-angle
                       x-centre y-centre
                       x-radius y-radius
                       :width width :operation operation)
      (set-pen-color canvas pen-color))))



(defun canvas-erase-filled-arc (canvas start-angle arc-angle
                                x-centre y-centre
                                x-radius y-radius
                                       &key
                                       (operation NIL)
                                       ;;(pattern *black-shade*)
                                       )
  (with-focused-canvas canvas
    (let ((pen-color (pen-color-of canvas)))
      (set-pen-color canvas (canvas-background-color canvas))
      (canvas-draw-filled-arc canvas  start-angle arc-angle
                                x-centre y-centre
                                x-radius y-radius
                                :operation operation)
      (set-pen-color canvas pen-color))))

