;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               control-button.lsp
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
;;;     C.B. Hurley 1988-1992 George Washington University
;;;     R.W. Oldford 1988-1991
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)
(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(control-button)))



;;----------------------------------------------------------------------


(defvar *control-button-text-color* *black-color*)

;originally defvar-ed in views/views-general/special-vars.lsp .. therefore  ;; 18SEP 2023
(setf *control-button-text-color* *gray-color*) ;(defvar *control-button-color* *gray-color*) 


(defclass control-button (control-mixin label) 
  ((style-keys :initform '(:button-color :font :color) 
               :allocation :class)
   (button-height :initform 2 :initarg :button-height
                  :accessor button-height
                  :documentation "Perceived 3d height of the button above ~
                                  the screen in pixels.")
   (button-color-menu? :initarg :button-color-menu?
                      :initform T
                      :accessor button-color-menu-p)
   (button-size-menu? :initarg :button-size-menu?
                      :initform T
                      :accessor button-size-menu-p)
   (max-height :initform :viewport :initarg :max-height
               :accessor max-height
               :documentation "Maximum height of the button")
   (max-width :initform :viewport :initarg :max-width
              :accessor max-width
              :documentation "Maximum width of the button")
   )
  
  (:default-initargs :font wb::*small-graphics-font*
    :color *control-button-text-color* :button-color *control-button-color* 
    ))

(defmethod selected-p ((self control-button) viewport location)
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
         (x 0) ;; 14MAR2022  gwb
         (y 0) ;; 14MAR2022  gwb
         )

    (if (not (numberp (elt location 0)))
      (setf x (elt location 1)
            y (elt location 2)) ;; sbcl requires x and y to be initialised before being used 14MAR2022  gwb
      (setf x (elt location 0)
            y (elt location 1))
      )
    
  (and (active-viewport-p viewport)
       (if (2d-position-p location)
         (let ((x (2d-position-x location))
               (y (2d-position-y location)))
           (and (>= x left)
                (<= x right)
                (>= y bottom)
                (<= y top)))
     (if (region-p location)
      (multiple-value-bind (l r b tp) (bounds-of location)
        (and (>= l left)
                (<= r right)
                (>= b bottom)
                (<= tp top))))))))
        



(defmethod draw-view ((self control-button) &key viewport)
  (let ((just (justification-of self))
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
         left-top-colour bottom-right-colour
         (button-height (truncate (abs (button-height self))))
         (half-button-height (truncate button-height 2))
         )
        (cond
         ((control-state self)
          (setf left-top-colour (wb::darken button-color))
          (setf bottom-right-colour (wb::lighten button-color))
          )
         (T
          (setf left-top-colour (wb::lighten button-color))
          (setf bottom-right-colour (wb::darken button-color))
          )
         )
        (wb::canvas-draw-filled-rectangle canvas left right bottom top
                                         :color button-color)
        (wb::canvas-draw-line canvas right bottom right top
                             :width button-height
                             :color bottom-right-colour)
        (wb::canvas-draw-line canvas left bottom right bottom
                             :width button-height
                             :color bottom-right-colour)
        (wb::canvas-draw-line canvas left bottom left top
                             :width button-height
                             :color left-top-colour)
        (wb::canvas-draw-line canvas left top right top
                             :width button-height
                             :color left-top-colour)
;        (wb::canvas-draw-rectangle canvas (- left button-height) (+ right button-height 1)
;                                   (- bottom button-height 1) (+ top button-height)
;                                   :width 1 :color wb::*black-colour*)
        (wb::canvas-draw-line canvas (+ button-height right) (- bottom button-height) (+ button-height right) (+ half-button-height top)
                             :width 1
                             :color wb::*black-colour*)
        (wb::canvas-draw-line canvas (- left half-button-height) (- bottom button-height) (+ button-height right) (- bottom button-height)
                             :width 1
                             :color wb::*black-colour*)
        (wb::canvas-draw-line canvas (- left half-button-height) (- bottom button-height) (- left half-button-height) (+ half-button-height top)
                             :width 1
                             :color wb::*black-colour*)
        (wb::canvas-draw-line canvas (- left half-button-height) (+ half-button-height top) (+ button-height right) (+ half-button-height top)
                             :width 1
                             :color wb::*black-colour*)
        (wb::canvas-draw-string canvas string
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
