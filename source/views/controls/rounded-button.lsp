;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               rounded-button.lsp
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
(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(rounded-button)))



;;----------------------------------------------------------------------


(defvar *rounded-button-text-color* *black-color*)
(defvar *rounded-button-color* *gray-color*)


(defclass rounded-button (control-mixin label) 
  ((style-keys :initform '(:button-color :font :color) 
               :allocation :class)
   (button-height :initform 2 :initarg :button-height
                  :accessor button-height
                  :documentation "Perceived 3d height of the button above ~
                                  the screen in pixels.")
   (max-height :initform 30 :initarg :max-height
               :accessor max-height
               :documentation "Maximum height of the button")
   (max-width :initform 80 :initarg :max-width
              :accessor max-width
              :documentation "Maximum width of the button"))

  
  (:default-initargs :font wb:*small-graphics-font*
    :color *rounded-button-text-color* :button-color *rounded-button-color* 
    ))

(defmethod selected-p ((self rounded-button) viewport location)
  ;; is view selected with mouse at location?
  ;; default looks to see if location is in viewport
  (let* ((width (width-of viewport))
         (height (height-of viewport))
         (mwidth (max-width self))
         (mwidth (if (eq mwidth :viewport) width mwidth))
         (mheight (max-height self))
         (mheight (if (eq mheight :viewport) height mheight))
         (p (min (/ width mwidth) (/ height mheight) 1))
         (x-centre (+ (left-of viewport) (/ width 2)))
         (y-centre (+ (bottom-of viewport) (/ height 2)))
         (width (* p mwidth))
         (half-width (/ width 2))
         (height (* p mheight))
         (half-height (/ height 2))
         (left (round (- x-centre half-width)))
         (right (round (+ x-centre half-width)))
         (top (round (+ y-centre half-height)))
         (bottom (round (- y-centre half-height)))
         (button-height (truncate (abs (button-height self))))
         (curve (* 2 button-height))
         (x (elt location 0))
         (y (elt location 1)))

    (and (active-viewport-p viewport)
         (or (and (>= x (+ left curve))
                  (<= x (- right curve))
                  (>= y bottom)
                  (<= y top))
             (and (>= x left)
                  (< x (+ left curve))
                  (>= y (+ (* -1 (- x left)) bottom curve))
                  (<= y (- (+ (- x left) top) curve)))
             (and (<= x right)
                  (> x (- right curve))
                  (>= y (+ (- x right) bottom curve))
                  (<= y (- (+ (* -1 (- x right)) top) curve)))))))
         


(defmethod draw-view ((self rounded-button) &key viewport)
  (let ((just (vw::justification-of self))
        (dir (orientation-of self))
        (font (draw-style self :font))
        (col (draw-style self :color))
        (string (get-text self))
        (button-color (draw-style self :button-color)))
    (unless (wb:canvas-font-p font)
      (setq font
            (let ((vp (car (viewports-of self))))
              (wb:canvas-make-font 
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
         (mwidth (max-width self))
         (mwidth (if (eq mwidth :viewport) width mwidth))
         (mheight (max-height self))
         (mheight (if (eq mheight :viewport) height mheight))
         (p (min (/ width mwidth) (/ height mheight) 1))
         (x-centre (+ (left-of vp) (/ width 2)))
         (y-centre (+ (bottom-of vp) (/ height 2)))
         (width (* p mwidth))
         (half-width (/ width 2))
         (height (* p mheight))
         (half-height (/ height 2))
         (left (round (- x-centre half-width)))
         (right (round (+ x-centre half-width)))
         (top (round (+ y-centre half-height)))
         (bottom (round (- y-centre half-height)))
         (button-height (truncate (abs (button-height self))))
         (half-button-height (truncate button-height 2))
         (curve (* 2 button-height))
         (list-of-points (list (cons left (+ bottom curve))
                           (cons left (- top curve))
                           (cons (+ left curve) top)
                           (cons (- right curve) top)
                           (cons right (- top curve))
                           (cons right (+ bottom curve))
                           (cons (- right curve) bottom)
                           (cons (+ left curve) bottom)
                           (cons left (+ bottom curve))
                           ))
         left-top-colour bottom-right-colour
         )
        (cond
         ((vw::control-state self)
          (setf left-top-colour (wb::darken button-color))
          (setf bottom-right-colour (wb::lighten button-color))
          )
         (T
          (setf left-top-colour (wb::lighten button-color))
          (setf bottom-right-colour (wb::darken button-color))
          )
         )
        (wb:canvas-draw-filled-polygon canvas list-of-points :color button-color)
        (wb:canvas-draw-line canvas left (+ bottom curve) left (- top curve)
                             :width button-height
                             :color left-top-colour)
        (wb:canvas-draw-line canvas left (- top curve) (+ left curve) top 
                             :width button-height
                             :color left-top-colour)
        (wb:canvas-draw-line canvas (+ left curve) top (- right curve) top
                             :width button-height
                             :color left-top-colour)
        (wb:canvas-draw-line canvas (- right curve) top right (- top curve)
                             :width button-height
                             :color (wb::lighten button-color :factor .1))
        (wb:canvas-draw-line canvas right (- top curve) right (+ bottom curve)
                             :width button-height
                             :color bottom-right-colour)
        (wb:canvas-draw-line canvas right (+ bottom curve) (- right curve) bottom
                             :width button-height
                             :color bottom-right-colour)
        (wb:canvas-draw-line canvas (- right curve) bottom (+ left curve) bottom
                             :width button-height
                             :color bottom-right-colour)
        (wb:canvas-draw-line canvas (+ left curve) bottom left (+ bottom curve)
                             :width button-height
                             :color (wb::lighten button-color :factor .1))
;;outline
        (wb:canvas-draw-line canvas (- left half-button-height) (- (+ bottom curve) button-height) (- left half-button-height) (+ (- top curve) half-button-height)
                             :width 1
                             :color wb::*black-colour*)
        (wb:canvas-draw-line canvas (- left half-button-height) (+ (- top curve) half-button-height) (- (+ left curve) button-height) (+ half-button-height top) 
                             :width 1
                             :color wb::*black-colour*)
        (wb:canvas-draw-line canvas (- (+ left curve) button-height) (+ half-button-height top) (+ button-height (- right curve)) (+ half-button-height top)
                             :width 1
                             :color wb::*black-colour*)
        (wb:canvas-draw-line canvas (+ button-height (- right curve)) (+ half-button-height top) (+ button-height right) (+ (- top curve) half-button-height)
                             :width 1
                             :color wb::*black-colour*)
        (wb:canvas-draw-line canvas (+ button-height right) (+ (- top curve) half-button-height) (+ button-height right) (- (+ bottom curve) button-height)
                             :width 1
                             :color wb::*black-colour*)
        (wb:canvas-draw-line canvas (+ button-height right) (- (+ bottom curve) button-height) (+ button-height (- right curve)) (- bottom button-height)
                             :width 1
                             :color wb::*black-colour*)
        (wb:canvas-draw-line canvas (+ button-height (- right curve)) (- bottom button-height) (- (+ left curve) half-button-height) (- bottom button-height)
                             :width 1
                             :color wb::*black-colour*)
        (wb:canvas-draw-line canvas (- (+ left curve) half-button-height) (- bottom button-height) (- left half-button-height) (- (+ bottom curve) button-height)
                             :width 1
                             :color wb::*black-colour*)
        (wb:canvas-draw-string canvas string
                               :left (+ left button-height)
                               :width (- width button-height button-height)
                               :bottom (+ bottom button-height)
                               :height (- height button-height button-height)
                               :font font
                               :orientation dir
                               :clip? t
                               :justification just
                               :color col)
        )
      
      )))

