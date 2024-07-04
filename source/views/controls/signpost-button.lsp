;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               signpost-button.lsp
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
(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(signpost-button)))



;;----------------------------------------------------------------------


(defvar *signpost-button-text-color* wb::*black-color*)
(defvar *signpost-button-color* wb::*gray-color*)


(defclass signpost-button (control-mixin label) 
  ((style-keys :initform '(:button-color :font :color) 
               :allocation :class)
   (button-height :initform 2 :initarg :button-height
                  :accessor button-height
                  :documentation "Perceived 3d height of the button above ~
                                  the screen in pixels.")
   (max-height :initform 25 :initarg :max-height
               :accessor max-height
               :documentation "Maximum height of the button")
   (max-width :initform 100 :initarg :max-width
              :accessor max-width
              :documentation "Maximum width of the button"))
  (:default-initargs :font wb:*small-graphics-font*
    :color *signpost-button-text-color* :button-color *signpost-button-color*
    :justification :left
    ))

(defun signpost-button (&rest keyword-args &key
                             (text "Unnamed signpost")
                             (text-color *signpost-button-text-color*)
                             (font wb:*small-graphics-font*)
                             (button-color *signpost-button-color*)
                             (button-fn NIL)
                             (draw? NIL)
                             )
  (declare (ignore draw? font button-color)) ; 29JUL2023
  (apply #'make-instance 'signpost-button  :text text :color text-color :left-fn button-fn
         keyword-args))

(defmethod selected-p ((self signpost-button) viewport location)
  ;; is view selected with mouse at location?
  ;; default looks to see if location is in viewport
  (let* ((width (width-of viewport))
         (height (height-of viewport))
         (mwidth (max-width self))
         (mheight (max-height self))
         (x-centre (+ (left-of viewport) (/ width 2)))
         (y-centre (+ (bottom-of viewport) (/ height 2)))
         p left right top bottom mid-vert mid-hor slope
         y-intercept1 y-intercept2 x y
         half-width half-height)
    
    (setf mwidth (if (eq mwidth :viewport)
                   width
                   mwidth))
    (setf mheight (if (eq mheight :viewport)
                    height 
                    mheight))
    
    (setf p (min (/ width mwidth) (/ height mheight) 1))
    (setf width (* p mwidth))
    (setf half-width (/ width 2))
    (setf height (* p mheight))
    (setf half-height (/ height 2))
    
    (setf left (round (- x-centre half-width)))
    (setf right (round (+ x-centre half-width)))
    (setf top (round (+ y-centre half-height)))
    (setf bottom (round (- y-centre half-height)))
    (setf mid-vert (round (/ (+ top bottom) 2)))
    (setf mid-hor (round (+ (/ (* width 4) 5) left)))
    (setf slope (/ (- mid-vert bottom) (- right mid-hor)))
    (setf y-intercept1 (- mid-vert (* slope right)))
    (setf y-intercept2 (- mid-vert (*  -1 slope right)))
    (setf x (elt location 0))
    (setf y (elt location 1))
    
    (and (active-viewport-p viewport)
         (or (and (>= x left) 
                  (<= x mid-hor)
                  (>= y bottom)
                  (<= y top))
             (and (> x mid-hor)
                  (<= x right)
                  (>= y (+ (* slope x) y-intercept1))
                  (<= y (+ (* -1 slope x) y-intercept2)))))))

 

(defmethod draw-view ((self signpost-button) &key viewport)
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
         (mwidth (if (eq (max-width self) :viewport)
                   width (max-width self)))
         (mheight (if (eq (max-height self) :viewport)
                    height
                    (max-height self)))
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
         (mid-vert (round (/ (+ top bottom) 2)))
         (mid-hor (round (+ (/ (* width 4) 5) left)))
         left-top-colour bottom-right-colour
         (button-height (truncate (abs (button-height self))))
         (half-button-height (truncate button-height 2))
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

        (wb::canvas-draw-filled-polygon canvas (list (cons left bottom) (cons left top)
                                                    (cons mid-hor top) (cons right mid-vert)
                                                    (cons mid-hor bottom)) :color button-color)
        (wb::canvas-draw-line canvas left top mid-hor top
                             :width button-height
                             :color left-top-colour)
        (wb::canvas-draw-line canvas mid-hor top right mid-vert
                             :width button-height
                             :color (wb::lighten button-color :factor .1))
        (wb::canvas-draw-line canvas right mid-vert mid-hor bottom
                             :width button-height
                             :color bottom-right-colour)
        (wb::canvas-draw-line canvas mid-hor bottom left bottom
                             :width button-height
                             :color bottom-right-colour)
        (wb::canvas-draw-line canvas left bottom left top
                             :width button-height
                             :color left-top-colour)

        (wb::canvas-draw-line canvas (- left half-button-height) (+ half-button-height top) (+ button-height mid-hor) (+ half-button-height top)
                             :width 1
                             :color wb::*black-colour*)
        (wb::canvas-draw-line canvas (+ button-height mid-hor) (+ half-button-height top) (+ 1 button-height right) mid-vert
                             :width 1
                             :color wb::*black-colour*)
        (wb::canvas-draw-line canvas (+ 1 button-height right) mid-vert (+ button-height mid-hor) (- bottom button-height)
                             :width 1
                             :color wb::*black-colour*)
        (wb::canvas-draw-line canvas (+ button-height mid-hor) (- bottom button-height) (- left half-button-height) (- bottom button-height)
                             :width 1
                             :color wb::*black-colour*)
        (wb::canvas-draw-line canvas (- left half-button-height) (- bottom button-height) (- left half-button-height) (+ half-button-height top)
                             :width 1
                             :color wb::*black-colour*)
        (wb::canvas-draw-string canvas string
                               :left (+ left (* 3 button-height))
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

