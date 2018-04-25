;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               slider.lsp
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
;;;     C.B. Hurley 1988-1991 George Washington University
;;;     R.W. Oldford 1988-1991
;;;     N. Wiebe 1998-1999
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(slider slider-mixin slider-level-of min-level max-level bar-slider needle-slider)))

;;;----------------------------------------------------------------------------------
(setq *default-slider-color* *default-point-color*)

(defclass slider-mixin (control-mixin boxed-view-mixin orientation-mixin 
                                      tic-mixin  )
  ((style-keys :initform '(:color :box-color) :allocation :class )
   (step :accessor slider-step-of :initarg :step :initform nil )
   (middle-menu :allocation :class :initform nil))
  (:default-initargs :min 0.0 :max 1.0 :color  *default-slider-color*
    ))


(defclass slider (slider-mixin simple-view )
  ((level :accessor slider-level-of :initarg :level :initform nil )
   (continuous-action-fn :accessor continuous-action-fn-of 
	:initarg :continuous-action-fn :initform nil
    :documentation "A function of no arguments to be fired as the slider level ~
                                         changes value.")))

  
(defclass bar-slider (slider) ())
(defclass needle-slider (slider) 
  ((style-keys :initform '(:color :needle-size :box-color) :allocation :class))
  (:default-initargs :needle-size 20 ))
                     
(defmethod initialize-instance :after ((self slider) &key)
  (if (null (slider-level-of self))
    (setf (slider-level-of self)
            (/ (+ (min-level self) (max-level self)) 2.0))))



(defmethod min-level ((self slider-mixin))
  (ecase (orientation-of self) 
           (:horizontal (left-of (bounding-region-of self)))
           (:vertical (bottom-of (bounding-region-of self)))))

(defmethod max-level ((self slider-mixin))
  (ecase (orientation-of self) 
           (:horizontal (right-of (bounding-region-of self)))
           (:vertical (top-of (bounding-region-of self)))))


(defmethod get-map-portion ((self needle-slider) viewport)
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
#|
(defmethod get-draw-portion ((self needle-slider) viewport)
  viewport)
|#
  


(defmethod draw-view ((self bar-slider) &key viewport)
  (with-exposed-viewports self viewport vp
    (multiple-value-bind (l r b tp) (bounds-of vp)
      (let ((map (select-map-to-viewport self vp) )
            (level (or (slider-level-of self) 0))
            (x-wid (- r l)) (y-wid (- tp b)))
        (if (eql (orientation-of self) :horizontal)
            (setq x-wid (round (* level (x-scale map))))
            (setq y-wid (round (* level (y-scale map)))))
        (wb:canvas-draw-filled-rectangle 
         (window-of vp) l (+ l x-wid) b (+ b y-wid)
         :color (draw-style self :color))))))


(defmethod erase-slider ((self slider-mixin) &key viewport &allow-other-keys )
  (erase-view self :viewport viewport))

(defmethod erase-slider ((self bar-slider) &key viewport level)
  (let ((old-level (or (slider-level-of self) 0))  )
    (when (< level old-level)
      (with-exposed-viewports self viewport vp
        (multiple-value-bind (l r b tp) (bounds-of vp)
          (let ((map (select-map-to-viewport self vp) ))
            (if (eql (orientation-of self) :horizontal)
              (setq l (round (+  (x-shift map) (* level (x-scale map))))
                    r (round (+  (x-shift map) (* old-level (x-scale map)))))
              (setq b (round (+  (y-shift map) (* level (y-scale map))))
                    tp (round (+  (y-shift map) (* old-level (y-scale map))))))
            
            (wb:canvas-clear  (window-of vp)
                              :canvas-left l :canvas-bottom b
                              :width (+ 1 (- r l))  :height (+ 1 (- tp b)))))))))


(defmethod draw-view ((self needle-slider) &key viewport)
  (with-exposed-viewports self viewport vp
    (multiple-value-bind (l r b tp) (bounds-of vp)
      (let ((map (select-map-to-viewport self vp) )
            (level (or (slider-level-of self) 0))
            (ns (truncate (draw-style self :needle-size) 2)))
        (if (eql (orientation-of self) :horizontal)
          (setq r (round (+  ns (x-shift map) (* level (x-scale map))))
                l (round (+ (- ns) (x-shift map) (* level (x-scale map)))))
          (setq tp (round (+ ns (y-shift map) (* level (y-scale map))))
                b (round (+  (- ns) (y-shift map) (* level (y-scale map))))))

        (let ((colour1 wb::*light-gray-colour*)
              (colour2 wb::*dark-gray-colour*)
              (colour3 wb::*default-canvas-pen-color*)
              (colour4 wb::*gray-colour*)
              (width 3)  ;;width between lines
              (lw 1)     ;; line width
              (num (min 3 (truncate ns 4))))    
;; max 3 lines above/below centre line

          (wb:canvas-draw-filled-rectangle (window-of vp) 
	                       l r b tp :color colour4)
          (wb:canvas-draw-rectangle (window-of vp) l r b tp :color colour3)
;   or  (wb:canvas-draw-inside-rectangle (window-of vp) l r b tp :color colour3)
;   (wb:canvas-draw-rectangle (window-of vp) (- l 1) r b (+ tp 1) :colorcolour3)

          (if (eql (orientation-of self) :horizontal)
            (let* ((indent-fraction 0.1)
                   (indent (truncate (* indent-fraction (height-of vp))))
                   (middle (truncate  (+ l r 1) 2)))
              (wb:canvas-draw-line (window-of vp) middle 
                                   (+ b (truncate indent 2))
                                   middle (- tp (truncate indent 2))
                                   :color colour2)
              (wb:canvas-draw-line (window-of vp) (+ lw middle) 
                                   (+ b (truncate indent 2))
                                   (+ lw middle) (- tp (truncate indent 2))
                                   :color colour1)
              (do ((index 0 (+ index 1))
                   (vert (+ middle width) (+ vert width)))
                  ((= index num))
                (wb:canvas-draw-line (window-of vp) vert (+ b indent) vert 
                                 (- tp indent)
                                     :color colour2)
                (wb:canvas-draw-line (window-of vp) (+ lw vert) (+ b indent) 
                                     (+ lw vert) (- tp indent)
                                     :color colour1))
              (do ((index 0 (+ index 1))
                   (vert (- middle width) (- vert width)))
                  ((= index num))
                (wb:canvas-draw-line (window-of vp) vert (+ b indent) vert 
                                     (- tp indent)
                                     :color colour2)
                (wb:canvas-draw-line (window-of vp) (+ lw vert) (+ b indent) 
                                     (+ lw vert) (- tp indent)
                                     :color colour1)))
            (let* ((indent-fraction 0.1)
                   (indent (truncate (* indent-fraction (width-of vp))))
                   (middle (truncate (+ b tp) 2)))
              (wb:canvas-draw-line (window-of vp) (+ l (truncate indent 2))
                                   middle (- r (truncate indent 2)) middle
                                   :color colour2)
              (wb:canvas-draw-line (window-of vp) (+ l (round indent 2))
                                   (+ lw middle) (- r (round indent 2)) 
                                   (+ lw middle)
                                   :color colour1)
              (do ((index 0 (+ index 1))
                   (hor (+ middle width) (+ hor width)))
                  ((= index num))
                (wb:canvas-draw-line (window-of vp) (+ l indent) hor 
                                     (- r indent) hor
                                     :color colour2)
                (wb:canvas-draw-line (window-of vp) (+ l indent) (+ lw hor) 
                                     (- r indent) (+ lw hor)
                                     :color colour1))
              (do ((index 0 (+ index 1))
                   (hor (- middle width) (- hor width)))
                  ((= index num))
                (wb:canvas-draw-line (window-of vp) (+ l indent) hor 
                                     (- r indent) hor
                                     :color colour2)
                (wb:canvas-draw-line (window-of vp) (+ l indent) (+ lw hor) 
                                     (- r indent) (+ lw hor)
                                     :color colour1))
              )))
        ))))

(defmethod erase-slider ((self needle-slider) &key viewport)
  (with-exposed-viewports self viewport vp
    (multiple-value-bind (l r b tp) (bounds-of vp)
      (let ((map (select-map-to-viewport self vp) )
            (level (or (slider-level-of self) 0))
             (ns (truncate (draw-style self :needle-size) 2)))

        (if (eql (orientation-of self) :horizontal)
          (setq r (round (+  ns (x-shift map) (* level (x-scale map))))
                l (round (+ (- ns) (x-shift map) (* level (x-scale map)))))
          (setq tp (round (+ ns (y-shift map) (* level (y-scale map))))
                b (round (+  (- ns) (y-shift map) (* level (y-scale map))))))

        (if (eql (orientation-of self) :horizontal)
          (wb:canvas-clear
           (window-of vp) :canvas-left l :canvas-bottom b
           :width (+ 1 (- r l)) :height (- tp b 1))
          (wb:canvas-clear
           (window-of vp) :canvas-left (+ 1 l) :canvas-bottom (- b 1)
           :width (- r l 1) :height (+ 2 (- tp b)))
;          (wb:canvas-clear
;           (window-of vp) :canvas-left (- l 1) :canvas-bottom b
;           :width (+ 2 (- r l)) :height (- tp b))
;          (wb:canvas-clear
;           (window-of vp) :canvas-left l :canvas-bottom (- b 1)
;           :width (- r l) :height (+ 2 (- tp b)))
          )))))

  
(defmethod get-menu-items :around
           ((self slider-mixin) (slot-name (eql 'middle-menu))))


(defmethod set-slider-level ((self slider) &key level viewport position)
  (setq level (or level
                  (let ((pos-local (apply-transform 
                                    (invert-transform 
                                     (select-map-to-viewport self viewport) ) 
                                    position)))
                    (if (eql (orientation-of self) :horizontal)
                      (2d-position-x pos-local)
                      (2d-position-y pos-local)))))
  (if (slider-step-of self)
    (setf level (* (slider-step-of self) (round level (slider-step-of self)))))
  (setf level (max (min-level self) (min level (max-level self))))
  (when (not (= level (slider-level-of self)))
    (erase-slider self :level level)
    (setf (slider-level-of self) level)
    (draw-view self)
    (let ((af (continuous-action-fn-of self)))
      (when (functionp af)
        (funcall af)))))


(defmethod left-button-fn :before ((self slider-mixin) &key viewport position)
  
  (declare (ignore position))
  (loop while (wb:mouse-down-p) do
        ;;(sleep 0.1)
         (set-slider-level self :viewport  viewport 
                           :position 
                           (view-position 
                            (wb:mouse-position (window-of viewport))))))

(defmethod use-x-axis-p ((self slider-mixin))
  (eql (orientation-of self) :horizontal))

(defmethod use-y-axis-p ((self slider-mixin))
  (eql (orientation-of self) :vertical))
