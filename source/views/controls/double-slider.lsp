;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               double-slider.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is part of the Views system, a toolkit for interactive statistical
;;;  graphics.
;;;  
;;;
;;;  Authors:
;;;     C.B. Hurley 1994
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(double-slider slider-low-level-of slider-high-level-of 
           double-bar-slider double-needle-slider)))

;;;----------------------------------------------------------------------------------



(defclass double-slider (slider)
  ((low-level :accessor slider-low-level-of :initarg :low-level :initform nil )
   (high-level :accessor slider-high-level-of :initarg :high-level :initform nil )))
   

(defclass double-needle-slider (double-slider) 
  ((style-keys :initform '(:color :needle-size :box-color) 
               :allocation :class))
  (:default-initargs :needle-size 10 ))


(defclass double-bar-slider (double-slider) 
  ((low-color :accessor slider-low-color-of :initarg :low-color :initform nil )
   (high-color :accessor slider-high-color-of :initarg :high-color :initform nil ))
  (:default-initargs :pretty? nil))
                   
(defmethod initialize-instance :after ((self double-slider) &key)
  (if (null (slider-high-level-of self))
    (setf (slider-high-level-of self)
            (+ (* 0.25 (min-level self)) (* 0.75 (max-level self)))))
  (if (null (slider-low-level-of self))
    (setf (slider-low-level-of self)
            (+ (* 0.75 (min-level self)) (* 0.25 (max-level self))))))



(defmethod get-map-portion ((self double-needle-slider) viewport)
  (let ((adjust-vp (copy-region viewport))
        (ns (truncate (draw-style self :needle-size) 2)))
    (ecase (orientation-of self) 
      (:horizontal
       (incf (left-of adjust-vp) ns)
       (decf (right-of adjust-vp) ns))
      (:vertical 
       (incf (bottom-of adjust-vp) ns)
       (decf (top-of adjust-vp) ns)))
    adjust-vp))




(defmethod draw-view ((self double-needle-slider) &key viewport)
  (with-exposed-viewports self viewport vp
    (multiple-value-bind (l r b tp) (bounds-of vp)
      (let ((map (select-map-to-viewport self vp) )
            (lo (slider-low-level-of self))
            (hi (slider-high-level-of self))
            (ns (truncate (draw-style self :needle-size) 2)))
        (loop for level in (list lo hi) do
              (if (eql (orientation-of self) :horizontal)
                (setq r (round (+  ns (x-shift map) (* level (x-scale map))))
                      l (round (+ (- ns) (x-shift map) (* level (x-scale map)))))
                (setq tp (round (+ ns (y-shift map) (* level (y-scale map)))) 
                      b (round (+  (- ns) (y-shift map) (* level (y-scale map))))))
              
              (wb:canvas-draw-filled-rectangle 
               (window-of vp) l r b tp
               :color (draw-style self :color)))))))


(defmethod erase-slider ((self double-needle-slider) &key viewport)
  (with-exposed-viewports self viewport vp
    (multiple-value-bind (l r b tp) (bounds-of vp)
      (let ((map (select-map-to-viewport self vp) )
            (lo (slider-low-level-of self))
            (hi (slider-high-level-of self))
            (ns (truncate (draw-style self :needle-size) 2)))
        (loop for level in (list lo hi) do
              (if (eql (orientation-of self) :horizontal)
                (setq r (round (+  ns (x-shift map) (* level (x-scale map))))
                      l (round (+ (- ns) (x-shift map) (* level (x-scale map)))))
                (setq tp (round (+ ns (y-shift map) (* level (y-scale map)))) 
                      b (round (+  (- ns) (y-shift map) (* level (y-scale map))))))
              
              (wb:canvas-clear 
               (window-of vp) :canvas-left l :canvas-bottom b
               :width (+ 1 (- r l)) :height (+ 1 (- tp b))))))))
  


(defmethod set-slider-level ((self double-slider) 
                             &key low-level high-level viewport position)
  (if (and (null low-level) (null high-level) position)
    (let* ((pos-local (apply-transform 
                        (invert-transform 
                         (select-map-to-viewport self viewport) ) 
                        position))
           (new (if (eql (orientation-of self) :horizontal)
                  (2d-position-x pos-local)
                  (2d-position-y pos-local))))
      (if (< (abs (- new (slider-low-level-of self)))
             (abs (- new (slider-high-level-of self))))
        (setq low-level new high-level (slider-high-level-of self) )
        (setq high-level new low-level (slider-low-level-of self) ))))

  (if (slider-step-of self)
    (setf low-level (* (slider-step-of self) (round low-level (slider-step-of self)))
          high-level (* (slider-step-of self) (round high-level (slider-step-of self)))))
  (setf low-level (max (min-level self) (min low-level (max-level self)))
        high-level (max (min-level self) (min high-level (max-level self))))
  (unless (and (= low-level (slider-low-level-of self))
            (= high-level (slider-high-level-of self)))
    (erase-slider self :low-level low-level :high-level high-level)
    (setf (slider-low-level-of self) low-level)
    (setf (slider-high-level-of self) high-level)
    (draw-view self)))


(defmethod draw-view ((self double-bar-slider) &key viewport)
  (with-exposed-viewports self viewport vp
    (multiple-value-bind (l r b tp) (bounds-of vp)
      (let ((map (select-map-to-viewport self vp) )
            (low (slider-low-level-of self))
            (l1 l) (r1 r) (b1 b) (tp1 tp)
            (hi (slider-high-level-of self)))
        (if (eql (orientation-of self) :horizontal)
            (setq l1 (round (+  (x-shift map) (* low (x-scale map))))
                  r1 (round (+  (x-shift map) (* hi (x-scale map)))) )
            (setq b1 (round (+  (y-shift map) (* low (y-scale map))) )
                  tp1 (round (+  (y-shift map) (* hi (y-scale map))))) )
        (wb:canvas-draw-filled-rectangle 
         (window-of vp) l l1 b tp
          :color (slider-low-color-of self))
        (wb:canvas-draw-filled-rectangle 
         (window-of vp) r1 r b tp
          :color (slider-high-color-of self))
        (wb:canvas-draw-filled-rectangle 
         (window-of vp) l1 r1 b1 tp1
          :color (draw-style self :color))))))



(defmethod erase-slider ((self double-bar-slider) &key viewport )
  (erase-view self :viewport viewport))


