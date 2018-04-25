;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               range-slider.lsp
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
;;;  Authors: N. Wiebe 1999
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------

(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(range-slider-mixin hue-range-slider lightness-range-slider)))
;;;----------------------------------------------------------------------------------
(defclass range-slider-mixin (double-bar-slider)
  ((saturation :accessor slider-saturation-of 
               :initform 1
               :initarg :saturation
               :documentation "Value should be between 0 and 1.")
   (color-model :accessor color-model-of
                :initform :triangle
                :initarg :color-model
                :documentation "Model which transforms lhs to rgb.")
   (flip-range? :accessor flip-range? :initform nil :initarg :flip-range?
                :documentation "Flips the range of values.")
   )
  (:documentation "Superclass to sliders that show hue and lightness ranges.")
  )

(defclass hue-range-slider (range-slider-mixin)
  ((lightness :accessor slider-lightness-of
              :initform 0.5
              :initarg :lightness
              :documentation "Value should be between 0 and 1.")
   )
  (:documentation "A slider showing a range of hue.~
                   Hue values are always 'mod'-ed 360 so min- and ~
                   max-levels should be something appropriate.")
  (:default-initargs :min 0 :max 720))

(defclass lightness-range-slider (range-slider-mixin)
  ((hue :accessor slider-hue-of
        :initform nil
        :initarg :hue
        :documentation "Will be 'mod'-ed by 360.")
   )
  (:documentation "A slider showing a range of lightness.~
                   Lightness values actually range between 0 and 1 ~
                   but for picture density, choose a large max-level.")
  (:default-initargs :min 0 :max 720))

(defmethod draw-view ((self hue-range-slider) &key viewport)
  (with-exposed-viewports self viewport vp
    (multiple-value-bind (l r b tp) (bounds-of vp)
      (let ((map (select-map-to-viewport self vp) )
            (low (slider-low-level-of self))
            (hi (slider-high-level-of self))
            (flip (flip-range? self)))
        (case
          (orientation-of self)
          (:horizontal
           (let*
             ((begin low)
              (end hi)
              x-pos)             
             (loop for x from begin to end
                   do
                   (setf x-pos (truncate (+  (x-shift map) (* x (x-scale map)))))
                   (wb:canvas-draw-line
                    (window-of vp)
                    x-pos
                    b
                    x-pos
                    tp
                    :color
                    (apply #'wb::make-color
                           (wb:lhs_to_rgb (slider-lightness-of self)
                                          (if flip (- 359 (mod x 360)) (mod x 360))
                                          (slider-saturation-of self)
                                          :model (color-model-of self))
                           ))
                   )
             )
           )
          (:vertical
           (let*
             ((begin low)
              (end hi)
              y-pos)             
             (loop for y from begin to end
                   do
                   (setf y-pos (truncate (+  (y-shift map) (* y (y-scale map)))))
                   (wb:canvas-draw-line
                    (window-of vp)
                    l
                    y-pos
                    r
                    y-pos
                    :color
                    (apply #'wb::make-color
                           (wb:lhs_to_rgb (slider-lightness-of self)
                                          (if flip (- 359 (mod y 360)) (mod y 360))
                                          (slider-saturation-of self)
                                          :model (color-model-of self))
                           ))
                   ))))
        
        ))))


(defmethod draw-view ((self lightness-range-slider) &key viewport)
  (with-exposed-viewports self viewport vp
    (multiple-value-bind (l r b tp) (bounds-of vp)
      (let ((map (select-map-to-viewport self vp) )
            (low (slider-low-level-of self))
            (hi (slider-high-level-of self))
            (hue (if (slider-hue-of self) (mod (slider-hue-of self) 360)))
            (base-one (- (max-level self) (min-level self)))
            (flip (flip-range? self)))
        (case
          (orientation-of self)
          (:horizontal
           (let*
             ((begin low)
              (end hi)
              x-pos)             
             (loop for x from begin to end
                   do
                   (setf x-pos (truncate (+  (x-shift map) (* x (x-scale map)))))
                   (wb:canvas-draw-line
                    (window-of vp)
                    x-pos
                    b
                    x-pos
                    tp
                    :color
                    (apply #'wb::make-color
                           (wb:lhs_to_rgb (if flip (- 1 (/ x base-one)) (/ x base-one)) 
                                          hue
                                          (slider-saturation-of self)
                                          :model (color-model-of self))
                           )))
             )
           )
          (:vertical
           (let*
             ((begin low)
              (end hi)
              y-pos)
             (loop for y from begin to end
                   do
                   (setf y-pos (truncate (+  (y-shift map) (* y (y-scale map)))))
                   (wb:canvas-draw-line
                    (window-of vp)
                    l
                    y-pos
                    r
                    y-pos
                    :color
                    (apply #'wb::make-color
                           (wb:lhs_to_rgb (if flip (- 1 (/ y base-one)) (/ y base-one))
                                          hue
                                          (slider-saturation-of self)
                                          :model (color-model-of self))
                           ))
                   ))))
        
        ))))

(defmethod left-button-fn  ((self range-slider-mixin) 
                            &key viewport ) 
  
  (let* ((lbf (left-fn-of self))
         (args (mapcar #'eval  (target-of self)))
         (vlist (member :viewport args)))
    (if  vlist  (push viewport (cdr  vlist)))
    ;    (control-start self)
    (unwind-protect 
      (if lbf (apply lbf args))
      ;      (if (toggle-control self)
      ;        (control-done self))
      )
    )
  )

(defmethod erase-slider ((self range-slider-mixin) &key viewport )
  ;erases conservatively, only portions are erased
  (with-exposed-viewports self viewport vp
    (let* ((w (window-of vp))
           (map (select-map-to-viewport self vp))
           (low (slider-low-level-of self))
           (hi (slider-high-level-of self))
           rl rb rw rh temp1 temp2)
      (when vp
        (setq rl   (left-of vp))
        (setq rb  (bottom-of vp))
        (setq rw (width-of vp))
        (setq rh (height-of vp))
        (case
          (orientation-of self)
          (:horizontal
           (setf temp1 (truncate (+  (x-shift map) (* low (x-scale map)))))
           (setf temp2 (truncate (+  (x-shift map) (* hi (x-scale map)))))
           (wb::canvas-clear w 
                             :canvas-left rl :canvas-bottom (+ rb -1)
                             :width (+ rl temp1) :height (+ 1 rh) )
           (wb::canvas-clear w
                             :canvas-left (+ temp2 1) :canvas-bottom (+ rb -1)
                             :width (+ rl (- rw temp2)) :height (+ 1 rh)) )
          (:vertical
           (setf temp1 (truncate (+  (y-shift map) (* low (y-scale map)))))
           (setf temp2 (truncate (+  (y-shift map) (* hi (y-scale map)))))
           (wb::canvas-clear w 
                             :canvas-left rl :canvas-bottom (+ rb (- temp2 rb) -1)
                             :width (+ 1 rw) :height (- rh (- temp2 rb)) )
           (wb::canvas-clear w
                             :canvas-left rl :canvas-bottom (+ rb -1)
                             :width (+ 1 rw) :height (- temp1 rb) ) )
          )
        (when (box-p self)
          (draw-viewport vp :color (draw-style self :box-color) 
                         :margin (box-margin-of self)))
        ))))
;maybe draw a few repeat lines to get rid of sometimes occuring missing lines
(defmethod set-slider-level ((self lightness-range-slider) 
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
    (setf low-level (* (slider-step-of self) (truncate low-level (slider-step-of self)))
          high-level (* (slider-step-of self) (truncate high-level (slider-step-of self)))
          ))
  (setf low-level (max (min-level self) (min low-level (max-level self)))
        high-level (max (min-level self) (min high-level (max-level self))))
  (unless (and (= low-level (slider-low-level-of self))
               (= high-level (slider-high-level-of self)))
    (cond 
     ((< (slider-low-level-of self) low-level (slider-high-level-of self))
      ;(erase-slider self :low-level low-level :high-level (slider-high-level-of self))
      (with-exposed-viewports self viewport vp
        (multiple-value-bind (l r b tp) (bounds-of vp)
          (let ((map (select-map-to-viewport self vp) )
                (old (slider-low-level-of self))
                (new low-level)
                )
            (case
              (orientation-of self)
              (:horizontal
               (let ((begin (truncate (+ (x-shift map) (* old (x-scale map)))))
                     (end (truncate (+ (x-shift map) (* new (x-scale map)))))
                     )
                 (wb::canvas-clear (window-of vp)
                                   :canvas-left begin :canvas-bottom b
                                   :width (- end begin) :height (- tp b))))
              (:vertical
               (let ((begin (truncate (+ (y-shift map) (* old (y-scale map)))))
                     (end (truncate (+ (y-shift map) (* new (y-scale map)))))
                     )
                 (wb::canvas-clear (window-of vp)
                                   :canvas-left l :canvas-bottom begin
                                   :width (- r l) :height (- end begin))))
              )
            )))
      
      (setf (slider-low-level-of self) low-level)
      ) 
     ((< (slider-low-level-of self) high-level (slider-high-level-of self))
      ;(erase-slider self :low-level (slider-low-level-of self) :high-level high-level)
      (with-exposed-viewports self viewport vp
        (multiple-value-bind (l r b tp) (bounds-of vp)
          (let ((map (select-map-to-viewport self vp) )
                (old (slider-high-level-of self))
                (new high-level)
                ;(new (+ 1 high-level))
                )
            (case
              (orientation-of self)
              (:horizontal
               (let ((begin (truncate (+ (x-shift map) (* new (x-scale map)))))
                     (end (truncate (+ (x-shift map) (* old (x-scale map)))))
                     )
                 (wb::canvas-clear (window-of vp)
                                   :canvas-left begin :canvas-bottom b
                                   :width (- end begin) :height (- tp b))))
              (:vertical
               (let ((begin (truncate (+ (y-shift map) (* new (y-scale map)))))
                     (end (truncate (+ (y-shift map) (* old (y-scale map)))))
                     )
                 (wb::canvas-clear (window-of vp)
                                   :canvas-left l :canvas-bottom begin
                                   :width (- r l) :height (- end begin))))
              )
            )))      
      (setf (slider-high-level-of self) high-level)
      )
     ((< (slider-high-level-of self) high-level)
      ;           (draw-view self)
      
      (with-exposed-viewports self viewport vp
        (multiple-value-bind (l r b tp) (bounds-of vp)
          (let ((map (select-map-to-viewport self vp) )
                (hue (if (slider-hue-of self) (mod (slider-hue-of self) 360)))
                (base-one (- (max-level self) (min-level self)))
                (flip (flip-range? self)))
            (case
              (orientation-of self)
              (:horizontal
               (let*
                 ((begin (slider-high-level-of self) )
                  (end high-level)
                  x-pos)
                 (loop for x from begin to end
                       do
                       (setf x-pos (truncate (+  (x-shift map) (* x (x-scale map)))))
                       (wb:canvas-draw-line
                        (window-of vp)
                        x-pos
                        b
                        x-pos
                        tp
                        :color
                        (apply #'wb::make-color
                               (wb:lhs_to_rgb (if flip (- 1 (/ x base-one)) (/ x base-one))
                                              hue
                                              (slider-saturation-of self)
                                              :model (color-model-of self))
                               ))
                       )
                 ))
              (:vertical
               (let*
                 ((begin (slider-high-level-of self) )
                  (end high-level)
                  y-pos)
                 (loop for y from (+ begin 1) to end
                       do
                       (setf y-pos (truncate (+  (y-shift map) (* y (y-scale map)))))
                       (wb:canvas-draw-line
                        (window-of vp)
                        l
                        y-pos
                        r
                        y-pos
                        :color
                        (apply #'wb::make-color
                               (wb:lhs_to_rgb (if flip (- 1 (/ y base-one)) (/ y base-one))
                                              hue
                                              (slider-saturation-of self)
                                              :model (color-model-of self))
                               ))
                       )))))))
      
      (setf (slider-high-level-of self) high-level)
      )
     ((> (slider-low-level-of self) low-level)
      ;  (draw-view self)
      (with-exposed-viewports self viewport vp
        (multiple-value-bind (l r b tp) (bounds-of vp)
          (let ((map (select-map-to-viewport self vp) )
                (hue (if (slider-hue-of self) (mod (slider-hue-of self) 360)))
                (base-one (- (max-level self) (min-level self)))
                (flip (flip-range? self)))
            (case
              (orientation-of self)
              (:horizontal
               (let*
                 ((begin low-level)
                  (end (slider-low-level-of self))
                  x-pos)
                 (loop for x from begin to end
                       do
                       (setf x-pos (truncate (+  (x-shift map) (* x (x-scale map)))))
                       (wb:canvas-draw-line
                        (window-of vp)
                        x-pos
                        b
                        x-pos
                        tp
                        :color
                        (apply #'wb::make-color
                               (wb:lhs_to_rgb (if flip (- 1 (/ x base-one)) (/ x base-one))
                                              hue
                                              (slider-saturation-of self)
                                              :model (color-model-of self))
                               ))
                       )
                 ))
              (:vertical
               (let*
                 ((begin low-level)
                  (end (slider-low-level-of self))
                  y-pos)
                 (loop for y from (+ begin 1) to end
                       do
                       (setf y-pos (truncate (+  (y-shift map) (* y (y-scale map)))))
                       (wb:canvas-draw-line
                        (window-of vp)
                        l
                        y-pos
                        r
                        y-pos
                        :color
                        (apply #'wb::make-color
                               (wb:lhs_to_rgb (if flip (- 1 (/ y base-one)) (/ y base-one))
                                              hue
                                              (slider-saturation-of self)
                                              :model (color-model-of self))
                               ))
                       )))))))
      (setf (slider-low-level-of self) low-level)
      )
     )
    (when (box-p self)
      (draw-viewport viewport :color (draw-style self :box-color) 
                     :margin (box-margin-of self)))
    
    ))

(defmethod set-slider-level ((self hue-range-slider) 
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
    (setf low-level (* (slider-step-of self) (truncate low-level (slider-step-of self)))
          high-level (* (slider-step-of self) (truncate high-level (slider-step-of self)))))
  (setf low-level (max (min-level self) (min low-level (max-level self)))
        high-level (max (min-level self) (min high-level (max-level self))))
  (unless (and (= low-level (slider-low-level-of self))
               (= high-level (slider-high-level-of self)))
    (cond 
     ((< (slider-low-level-of self) low-level (slider-high-level-of self))
      ;(erase-slider self :low-level low-level :high-level (slider-high-level-of self))
      (with-exposed-viewports self viewport vp
        (multiple-value-bind (l r b tp) (bounds-of vp)
          (let ((map (select-map-to-viewport self vp) )
                (old (slider-low-level-of self))
                (new low-level)
                )
            (case
              (orientation-of self)
              (:horizontal
               (let ((begin (truncate (+ (x-shift map) (* old (x-scale map)))))
                     (end (truncate (+ (x-shift map) (* new (x-scale map)))))
                     )
                 (wb::canvas-clear (window-of vp)
                                   :canvas-left begin :canvas-bottom b
                                   :width (- end begin) :height (- tp b))))
              (:vertical
               (let ((begin (truncate (+ (y-shift map) (* old (y-scale map)))))
                     (end (truncate (+ (y-shift map) (* new (y-scale map)))))
                     )
                 (wb::canvas-clear (window-of vp)
                                   :canvas-left l :canvas-bottom begin
                                   :width (- r l) :height (- end begin))))
              )
            )))
      (setf (slider-low-level-of self) low-level)
      ) 
     ((< (slider-low-level-of self) high-level (slider-high-level-of self))
      ;(erase-slider self :low-level (slider-low-level-of self) :high-level high-level)
      (with-exposed-viewports self viewport vp
        (multiple-value-bind (l r b tp) (bounds-of vp)
          (let ((map (select-map-to-viewport self vp) )
                (old (slider-high-level-of self))
                (new high-level)
                ;(new (+ 1 high-level))
                )
            (case
              (orientation-of self)
              (:horizontal
               (let ((begin (truncate (+ (x-shift map) (* new (x-scale map)))))
                     (end (truncate (+ (x-shift map) (* old (x-scale map)))))
                     )
                 (wb::canvas-clear (window-of vp)
                                   :canvas-left begin :canvas-bottom b
                                   :width (- end begin) :height (- tp b))))
              (:vertical
               (let ((begin (truncate (+ (y-shift map) (* new (y-scale map)))))
                     (end (truncate (+ (y-shift map) (* old (y-scale map)))))
                     )
                 (wb::canvas-clear (window-of vp)
                                   :canvas-left l :canvas-bottom begin
                                   :width (- r l) :height (- end begin))))
              )
            )))
      (setf (slider-high-level-of self) high-level)
      )
     ((< (slider-high-level-of self) high-level)
      ;           (draw-view self)
      
      (with-exposed-viewports self viewport vp
        (multiple-value-bind (l r b tp) (bounds-of vp)
          (let ((map (select-map-to-viewport self vp) )
                (flip (flip-range? self)))
            
            
            (case
              (orientation-of self)
              (:horizontal
               (let*
                 ((begin (slider-high-level-of self))
                  (end high-level)
                  x-pos)
                 (loop for x from begin to end
                       do
                       (setf x-pos (truncate (+  (x-shift map) (* x (x-scale map)))))
                       (wb:canvas-draw-line
                        (window-of vp)
                        x-pos
                        b
                        x-pos
                        tp
                        :color
                        (apply #'wb::make-color
                               (wb:lhs_to_rgb (slider-lightness-of self)
                                              (if flip (- 359 (mod x 360)) (mod x 360))
                                              (slider-saturation-of self)
                                              :model (color-model-of self))
                               ))
                       )
                 ))
              (:vertical
               (let*
                 ((begin (slider-high-level-of self))
                  (end high-level)
                  y-pos)
                 (loop for y from (+ begin 1) to end
                       do
                       (setf y-pos (truncate (+  (y-shift map) (* y (y-scale map)))))
                       (wb:canvas-draw-line
                        (window-of vp)
                        l
                        y-pos
                        r
                        y-pos
                        :color
                        (apply #'wb::make-color
                               (wb:lhs_to_rgb (slider-lightness-of self)
                                              (if flip (- 359 (mod y 360)) (mod y 360))
                                              (slider-saturation-of self)
                                              :model (color-model-of self))
                               ))
                       )))))))
      
      (setf (slider-high-level-of self) high-level)
      )
     ((> (slider-low-level-of self) low-level)
      ;  (draw-view self)
      (with-exposed-viewports self viewport vp
        (multiple-value-bind (l r b tp) (bounds-of vp)
          (let ((map (select-map-to-viewport self vp) )
                (flip (flip-range? self)))
            (case
              (orientation-of self)
              (:horizontal
               (let*
                 ((begin low-level)
                  (end (slider-low-level-of self))
                  x-pos)
                 (loop for x from begin to end
                       do
                       (setf x-pos (truncate (+  (x-shift map) (* x (x-scale map)))))
                       (wb:canvas-draw-line
                        (window-of vp)
                        x-pos
                        b
                        x-pos
                        tp
                        :color
                        (apply #'wb::make-color
                               (wb:lhs_to_rgb (slider-lightness-of self)
                                              (if flip (- 359 (mod x 360)) (mod x 360))
                                              (slider-saturation-of self)
                                              :model (color-model-of self))
                               ))
                       )
                 ))
              (:vertical
               (let*
                 ((begin low-level)
                  (end (slider-low-level-of self))
                  y-pos)
                 (loop for y from begin to end
                       do
                       (setf y-pos (truncate (+  (y-shift map) (* y (y-scale map)))))
                       (wb:canvas-draw-line
                        (window-of vp)
                        l
                        y-pos
                        r
                        y-pos
                        :color
                        (apply #'wb::make-color
                               (wb:lhs_to_rgb (slider-lightness-of self)
                                              (if flip (- 359 (mod y 360)) (mod y 360))
                                              (slider-saturation-of self)
                                              :model (color-model-of self))
                               ))
                       )))))))
      (setf (slider-low-level-of self) low-level)
      )
     )
    (when (box-p self)
      (draw-viewport viewport :color (draw-style self :box-color) 
                     :margin (box-margin-of self)))
    
    ))
