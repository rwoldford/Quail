;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               elliptical-button.lsp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is part of the Views system, a toolkit for interactive statistical graphics.
;;;  
;;;
;;;  Authors:
;;;     N. Wiebe 1998
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)
(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(elliptical-button)))



;;----------------------------------------------------------------------


(defvar *elliptical-button-text-color* *black-color*)
(defvar *elliptical-button-color* *gray-color*)


(defclass elliptical-button (control-mixin label) 
  ((style-keys :initform '(:button-color :font :color) 
               :allocation :class)
   (button-height :initform 2 :initarg :button-height
                  :accessor button-height
                  :documentation "Perceived 3d height of the button above ~
                                  the screen in pixels.")
   (diameter-x :initform :viewport :initarg :diameter-x
               :accessor diameter-x
               :documentation "Maximum diameter in the horizontal direction")
   (diameter-y :initform :viewport :initarg :diameter-y
              :accessor diameter-y
              :documentation "Maximum diameter in the vertical direction"))
  
  (:default-initargs :font wb::*small-graphics-font*
    :color *elliptical-button-text-color* :button-color *elliptical-button-color* 
    ))

(defmethod selected-p ((self elliptical-button) viewport location)
  ;; is view selected with mouse at location?
  ;; default looks to see if location is in viewport
  (let* ((width (width-of viewport))
         (height (height-of viewport))
         (diamx (diameter-x self))
         (diamx (if (eq diamx :viewport) width diamx))
         (diamy (diameter-y self))
         (diamy (if (eq diamy :viewport) height diamy))
         (p (min (/ width diamx) (/ height diamy) 1))
         (x-centre (round (+ (left-of viewport) (/ width 2))))
         (y-centre (round (+ (bottom-of viewport) (/ height 2))))
         (diameter-x (* p diamx))
         (x-radius (round (/ diameter-x 2)))
         (diameter-y (* p diamy))
         (y-radius (round (/ diameter-y 2)))
         (left (- x-centre x-radius))
         (right (+ x-centre x-radius))
         (x (elt location 0))
         (y (elt location 1)))
    
    (and (active-viewport-p viewport)
         (>= x left)
         (<= x right)
         (or (and (>= y y-centre)
                  (<= y (+ y-centre (sqrt (* (** y-radius 2)
                                             (- 1 (/ (** (- x x-centre) 2)
                                                     (** x-radius 2))))))))
             (and (< y y-centre)
                  (>= y (+ y-centre (* -1 (sqrt (* (** y-radius 2)
                                             (- 1 (/ (** (- x x-centre) 2)
                                                     (** x-radius 2)))))))))))))

           

(defmethod draw-view ((self elliptical-button) &key viewport)
  (let ((just (vw::justification-of self))
        (dir (orientation-of self))
        (font (draw-style self :font))
        (col (draw-style self :color))
        (string (get-text self))
        (button-color (draw-style self :button-color)))

    (unless (wb::canvas-font-p font)
      (setq font
            (let ((vp (car (viewports-of self))))
              (wb::canvas-make-font 
               :size
               (max 7
                    (min 16 
                         (truncate (* 0.5 (min (width-of vp) (height-of vp)))))))))

      (set-draw-style self :font font))
    (if (null dir)
      (let ((vp (car (viewports-of self))))
        (setf (orientation-of self)
              (setq dir (if (> (height-of vp) (width-of vp))
                          :vertical
                          :horizontal)))))
    
    (with-exposed-viewports self viewport vp
      (let*
        ((canvas (window-of vp))
         (width (width-of vp))
         (height (height-of vp))
         (diamx (diameter-x self))
         (diamx (if (eq diamx :viewport) width diamx))
         (diamy (diameter-y self))
         (diamy (if (eq diamy :viewport) height diamy))
         (p (min (/ width diamx) (/ height diamy) 1))
         (x-centre (round (+ (left-of vp) (/ width 2))))
         (y-centre (round (+ (bottom-of vp) (/ height 2))))
         (diameter-x (* p diamx))
         (x-radius (round (/ diameter-x 2)))
         (diameter-y (* p diamy))
         (y-radius (round (/ diameter-y 2)))
         colour1 colour2 colour3
         (button-height (truncate (abs (button-height self))))
         (half-button-height (truncate button-height 2))
         )
        (cond
         ((vw::control-state self)
          (setf colour1 (wb::darken button-color))
          (setf colour2 (wb::lighten button-color :factor .1))
          (setf colour3 (wb::lighten button-color))
          )
         (T
          (setf colour1 (wb::lighten button-color))
          (setf colour2 (wb::lighten button-color :factor .1))
          (setf colour3 (wb::darken button-color))
          )
         )


        (wb::canvas-draw-filled-arc canvas 65 360 
                            x-centre y-centre x-radius y-radius
                             :color button-color)
        (wb::canvas-draw-arc canvas 15 60 
                            x-centre y-centre x-radius y-radius
                             :width button-height
                             :color colour2)
        (wb::canvas-draw-arc canvas 75 120 
                            x-centre y-centre x-radius y-radius
                             :width button-height
                             :color colour1)
        (wb::canvas-draw-arc canvas 195 60 
                            x-centre y-centre x-radius y-radius
                            :width button-height
                            :color colour2)
        (wb::canvas-draw-arc canvas 255 120 
                            x-centre y-centre x-radius y-radius
                            :width button-height
                            :color colour3)
        (wb::canvas-draw-arc canvas 65 360 
                            x-centre y-centre (+ x-radius half-button-height) (+ y-radius half-button-height)
                            :width 1
                             :color wb::*black-colour*)
        (wb::canvas-draw-string canvas string
                               :left (+ (- x-centre x-radius) button-height)
                               :width (round (- diameter-x button-height button-height))
                               :bottom (+ (- y-centre y-radius) button-height)
                               :height (round (- diameter-y button-height button-height))
                               :font font
                               :orientation dir
                               :clip? t
                               :justification just
                               :color col)
        )
      
      )))
