;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               hue-light-view.lsp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(user-pick-hue user-pick-hue-light-range)))
;;;----------------------------------------------------------------------------------

(defun user-pick-hue ()
  ; a user-pick-hue wheel
  (let* ((size 300)
         (x-centre (truncate size 2))
         (y-centre (- x-centre (truncate size 10)))
         (radius (truncate size 3))
         (arc-angle 1)
         (lightness 0.5)
         (saturation 1)
         (indent 25)
         (color-model :double-hexcone)
         (text (text-view :text "Click on hue:" :draw? nil))
         vw hue-angle
         text-vp)
    (setf vw (make-view-window :left indent :right (+ indent size) 
                               :bottom indent :top (+ indent size) 
                               :title "Hue Wheel"))
    (setf text-vp (make-viewport vw 10 100 (- size 50) (- size 30)))
    (draw-view text :viewport text-vp :draw? t)
    (loop for i from 0 to 359 by 1 
          do
          (wb::canvas-draw-filled-arc vw i arc-angle 
                                      x-centre y-centre radius radius 
                                      :color (apply #'wb::make-color 
                                                    (wb::lhs_to_rgb lightness i saturation 
                                                                    :model color-model))))
    (loop until (wb::mouse-down-p))
    (when (wb::mouse-down-p)
      (let* ((mouse-pos (wb::mouse-position vw))
             (x (- (wb::position-x mouse-pos) x-centre))
             (y (- (wb::position-y mouse-pos) y-centre))
             )
        (cond
         ((> (+ (q::sqr x) (q::sqr y)) (q::sqr radius))
          (setf hue-angle nil))
         ((= 0 x)
          (setf hue-angle (if (>= y 0) 90 270)))
         ((and (> x 0) (>= y 0)) 
          (setf hue-angle (* (/ (atan (/ y x)) pi) 180)))
         ((and (< x 0) (>= y 0)) 
          (setf hue-angle (+ 180 (* (/ (atan (/ y x)) pi) 180))))
         ((and (< x 0) (< y 0)) 
          (setf hue-angle (+ 180 (* (/ (atan (/ y x)) pi) 180))))
         ((and (> x 0) (< y 0)) 
          (setf hue-angle (+ 360 (* (/ (atan (/ y x)) pi) 180))))
         )))
    (sleep 0.5)
    (wb::close-canvas vw)
    hue-angle))

(defun user-pick-hue-light-range (self slot &key 
                                       (prompt-user-string 
                                        "Pick Hue and/or Lightness Range"))
  ;slot is a 3x3 matrix in self [see map-with-lhs-matrix for details]
  (let* ((width (min 450 (floor (wb::screen-width) 2)))
         (height (min 350 (floor (wb::screen-height) 2)))
         (left (if (<= (+ width (wb::screen-mouse-x) 20)
                       (wb::screen-width))
                 (wb::screen-mouse-x)
                 width))
         (bottom (if (<= (+ height (wb::screen-mouse-y) 20) (wb::screen-height))
                   (wb::screen-mouse-y)
                   height))
         (lhs-matrix (eval `(,slot ,self)))
         (light (or (q::eref lhs-matrix 0 0) 0.5))
         (light-min (or (q::eref lhs-matrix 0 1) 0.05))
         (light-max (or (q::eref lhs-matrix 0 2) 0.95))
         (hue (or (q::eref lhs-matrix 1 0) nil))
         (hue-min (or (q::eref lhs-matrix 1 1) 0))
         (hue-max (or (q::eref lhs-matrix 1 2) 720))
         
         (empty1 (text-view :text "" :draw? NIL))
         (empty2 (text-view :text "" :draw? NIL))
         (keyword-args nil)
         (title (text-view :text prompt-user-string :draw? nil))
         (hue-toggle (make-instance 'radio-button :draw? nil))
         (lightness-toggle (make-instance 'radio-button :draw? nil))
         (hue-lightness-toggle (make-instance 'radio-button :draw? nil))
         (hue-text (text-view :text "By Hue?" :draw? nil))
         (lightness-text (text-view :text "By Lightness?" :draw? nil))
         (hue-lightness-text (text-view :text "By Hue and Lightness?" :draw? nil))
         (hue-slider-text (text-view :text (format NIL "Hue Range:") :draw? nil))
         (lightness-slider-text (text-view :text (format NIL "Lightness Range:") :draw? nil))
         ;(color-model-button (make-instance 'control-button :text "triangle" :draw? nil))
         (color-model :triangle)
         (hue-slider (make-instance 'hue-range-slider
                       :orientation :horizontal
                       :min 0 :max 720 :low-level hue-min 
                       :high-level hue-max :draw? nil
                       :box? t :lightness light :color-model color-model
                       ))
         (lightness-slider (make-instance 'lightness-range-slider 
                             :orientation :horizontal 
                             :min 0 :max 720 :low-level (* 720 light-min) 
                             :high-level (* 720 light-max) :draw? nil
                             :box? t :hue (if hue (mod hue 360)) :color-model color-model
                             ))
         
         (lightness-level-text (text-view :text (format NIL "Lightness Level:") :draw? nil))
         (lightness-level-slider (make-instance 'needle-slider :orientation :vertical
                                                :min 0 :max 1 :needle-size 16
                                                :level light :box? t))
         
         (help-button (control-button :text "Help" :draw? nil))
         (apply-button (control-button :text "Apply" :draw? nil))
         (done-button (control-button :text "Done" :draw? nil))
         
         (red-chip (make-instance 'logical-widget :color wb::*red-color*))
         (yellow-chip (make-instance 'logical-widget :color wb::*yellow-color*))
         (green-chip (make-instance 'logical-widget :color wb::*green-color*))
         (cyan-chip (make-instance 'logical-widget :color wb::*cyan-color*))
         (blue-chip (make-instance 'logical-widget :color wb::*blue-color*))
         (magenta-chip (make-instance 'logical-widget :color wb::*magenta-color*))
         (purple-chip (make-instance 'logical-widget :color wb::*purple-color*))
         (orange-chip (make-instance 'logical-widget :color wb::*orange-color*))
         (gray-chip (make-instance 'logical-widget :color wb::*gray-color*))
         (other-chip (control-button :text "Other" :button-color wb::*white-color*))
         
         (colour-chip-list (list gray-chip  magenta-chip red-chip orange-chip yellow-chip 
                                 green-chip cyan-chip blue-chip purple-chip))
         
         (hue-choice-toggles (apply #'grid-layout :ncols 5
                                    :subviews 
                                    (concatenate 'list colour-chip-list (list other-chip))
                                    :box-views? nil :gap-x .05 :gap-y .05
                                    :viewed-object self
                                    :draw? NIL
                                    keyword-args))
         
         (hue-choice-text (text-view :text (format NIL "Hue Choice:") :draw? nil))
         
         (text1-region (make-region 05 28 63 70))
         (object1-region (make-region 05 80 40 63))
         (text2-region (make-region 05 28 28 35))
         (object2a-region (make-region 05 80 05 28))
         (object2b-region (make-region 29 39 05 35))
         text1 object1 text2 object2
         layout highlight-by vw vp)
    
    
    
    (setf layout
          (apply #'view-layout
                 :viewed-object nil
                 :draw? nil
                 :subviews
                 (list empty1
                       title
                       lightness-toggle lightness-text 
                       hue-toggle hue-text
                       hue-lightness-toggle hue-lightness-text
                       ;color-model-button
                       help-button apply-button done-button
                       empty2)
                 :positions
                 '((00 00 00 00) ;;empty1
                   (03 97 87 97) ;;title
                   
                   (03 13 77 87) ;;lightness-toggle
                   (15 30 77 87) ;;lightness-text
                   
                   (32 42 77 87) ;;hue-toggle
                   (44 54 77 87) ;;hue-text 
                   
                   (56 66 77 87) ;;hue-lightness-toggle
                   (68 97 77 87) ;;hue-lightness-text
                   
                   ;(75 97 68 78) ;;color-model-button
                   
                   (85 97 50 60) ;;help-button
                   (85 97 35 45) ;;apply-button
                   (85 97 05 15) ;;done-button
                   (100 100 100 100)) ;;empty2
                 keyword-args))
    
    (setf (left-fn-of red-chip)
          #'(lambda (self)
              (declare (ignore self))
              (if (not (logical-value-of red-chip))
                (progn
                  (loop for chip in colour-chip-list do
                        (setf (logical-value-of chip) nil)
                        (draw-view chip :viewport (viewport-of chip) :erase? T))
                  (setf (logical-value-of red-chip) t)
                  (draw-view red-chip :viewport (viewport-of red-chip) :erase? T)
                  (setf (slider-hue-of lightness-slider) (wb::hue-of wb::*red-color*))
                  (draw-view lightness-slider :viewport (viewport-of lightness-slider) 
                             :erase? t)
                  ))))
    (setf (left-fn-of yellow-chip)
          #'(lambda (self)
              (declare (ignore self))
              (if (not (logical-value-of yellow-chip))
                (progn
                  (loop for chip in colour-chip-list do
                        (setf (logical-value-of chip) nil)
                        (draw-view chip :viewport (viewport-of chip) :erase? T))
                  (setf (logical-value-of yellow-chip) t)
                  (draw-view yellow-chip :viewport (viewport-of yellow-chip) :erase? T)
                  (setf (slider-hue-of lightness-slider) (wb::hue-of wb::*yellow-color*))
                  (draw-view lightness-slider :viewport (viewport-of lightness-slider) 
                             :erase? t)
                  ))))
    (setf (left-fn-of green-chip)
          #'(lambda (self)
              (declare (ignore self))
              (if (not (logical-value-of green-chip))
                (progn
                  (loop for chip in colour-chip-list do
                        (setf (logical-value-of chip) nil)
                        (draw-view chip :viewport (viewport-of chip) :erase? T))
                  (setf (logical-value-of green-chip) t)
                  (draw-view green-chip :viewport (viewport-of green-chip) :erase? T)
                  (setf (slider-hue-of lightness-slider) (wb::hue-of wb::*green-color*))
                  (draw-view lightness-slider :viewport (viewport-of lightness-slider) 
                             :erase? t)
                  ))))
    (setf (left-fn-of cyan-chip)
          #'(lambda (self)
              (declare (ignore self))
              (if (not (logical-value-of cyan-chip))
                (progn
                  (loop for chip in colour-chip-list do
                        (setf (logical-value-of chip) nil)
                        (draw-view chip :viewport (viewport-of chip) :erase? T))
                  (setf (logical-value-of cyan-chip) t)
                  (draw-view cyan-chip :viewport (viewport-of cyan-chip) :erase? T)
                  (setf (slider-hue-of lightness-slider) (wb::hue-of wb::*cyan-color*))
                  (draw-view lightness-slider :viewport (viewport-of lightness-slider) 
                             :erase? t)
                  ))))
    (setf (left-fn-of blue-chip)
          #'(lambda (self)
              (declare (ignore self))
              (if (not (logical-value-of blue-chip))
                (progn
                  (loop for chip in colour-chip-list do
                        (setf (logical-value-of chip) nil)
                        (draw-view chip :viewport (viewport-of chip) :erase? T))
                  (setf (logical-value-of blue-chip) t)
                  (draw-view blue-chip :viewport (viewport-of blue-chip) :erase? T)
                  (setf (slider-hue-of lightness-slider) (wb::hue-of wb::*blue-color*))
                  (draw-view lightness-slider :viewport (viewport-of lightness-slider) 
                             :erase? t)
                  ))))
    (setf (left-fn-of magenta-chip)
          #'(lambda (self)
              (declare (ignore self))
              (if (not (logical-value-of magenta-chip))
                (progn
                  (loop for chip in colour-chip-list do
                        (setf (logical-value-of chip) nil)
                        (draw-view chip :viewport (viewport-of chip) :erase? T))
                  (setf (logical-value-of magenta-chip) t)
                  (draw-view magenta-chip :viewport (viewport-of magenta-chip) :erase? T)
                  (setf (slider-hue-of lightness-slider) (wb::hue-of wb::*magenta-color*))
                  (draw-view lightness-slider :viewport (viewport-of lightness-slider) 
                             :erase? t)
                  ))))
    (setf (left-fn-of purple-chip)
          #'(lambda (self)
              (declare (ignore self))
              (if (not (logical-value-of purple-chip))
                (progn
                  (loop for chip in colour-chip-list do
                        (setf (logical-value-of chip) nil)
                        (draw-view chip :viewport (viewport-of chip) :erase? T))
                  (setf (logical-value-of purple-chip) t)
                  (draw-view purple-chip :viewport (viewport-of purple-chip) :erase? T)
                  (setf (slider-hue-of lightness-slider) (wb::hue-of wb::*purple-color*))
                  (draw-view lightness-slider :viewport (viewport-of lightness-slider) 
                             :erase? t)
                  ))))
    (setf (left-fn-of orange-chip)
          #'(lambda (self)
              (declare (ignore self))
              (if (not (logical-value-of orange-chip))
                (progn
                  (loop for chip in colour-chip-list do
                        (setf (logical-value-of chip) nil)
                        (draw-view chip :viewport (viewport-of chip) :erase? T))
                  (setf (logical-value-of orange-chip) t)
                  (draw-view orange-chip :viewport (viewport-of orange-chip) :erase? T)
                  (setf (slider-hue-of lightness-slider) (wb::hue-of wb::*orange-color*))
                  (draw-view lightness-slider :viewport (viewport-of lightness-slider) 
                             :erase? t)
                  ))))
    (setf (left-fn-of gray-chip)
          #'(lambda (self)
              (declare (ignore self))
              (if (not (logical-value-of gray-chip))
                (progn
                  (loop for chip in colour-chip-list do
                        (setf (logical-value-of chip) nil)
                        (draw-view chip :viewport (viewport-of chip) :erase? T))
                  (setf (logical-value-of gray-chip) t)
                  (draw-view gray-chip :viewport (viewport-of gray-chip) :erase? T)
                  (setf (slider-hue-of lightness-slider) (wb::hue-of wb::*gray-color*))
                  (draw-view lightness-slider :viewport (viewport-of lightness-slider) 
                             :erase? t)
                  ))))
    (setf (left-fn-of other-chip)
          #'(lambda ()
              (let ((hue (user-pick-hue))
                    ;(colour (wb::user-pick-color)) ;temporary
                    )
                (loop for chip in colour-chip-list do
                      (setf (logical-value-of chip) nil)
                      (draw-view chip :viewport (viewport-of chip) :erase? T))
                ;(setf (slider-hue-of lightness-slider) (wb::hue-of colour))
                (setf (slider-hue-of lightness-slider) hue)
                (draw-view lightness-slider :viewport (viewport-of lightness-slider) 
                           :erase? t)
                )))
    
    (setf (continuous-action-fn-of lightness-level-slider)
          #'(lambda()
              (setf (slider-lightness-of hue-slider) 
                    (slider-level-of lightness-level-slider))
              (draw-view hue-slider :viewport (viewport-of hue-slider) :erase? t)))
    
    (setf (left-fn-of lightness-toggle)
          #'(lambda (self)
              (declare (ignore self))
              (if (not (logical-value-of lightness-toggle))
                (progn
                  (setf (viewed-object-of lightness-toggle) t)
                  (if (logical-value-of hue-toggle)
                    (progn (setf (viewed-object-of hue-toggle) nil)
                           (draw-view hue-toggle :viewport 
                                      (viewport-of hue-toggle) :erase? T)))
                  (if (logical-value-of hue-lightness-toggle)
                    (progn (setf (viewed-object-of hue-lightness-toggle) nil)
                           (draw-view hue-lightness-toggle :viewport 
                                      (viewport-of hue-lightness-toggle) :erase? T)))                             
                  (draw-view lightness-toggle :viewport (viewport-of lightness-toggle) 
                             :erase? T)
                  (sleep 1)
                  (erase-view layout :viewport vp)
                  
                  (if text1 (delete-subview layout text1))
                  (if object1 (delete-subview layout object1))
                  (if text2 (delete-subview layout text2))
                  (if object2 (delete-subview layout object2))
                  (setf text1 lightness-slider-text)
                  (setf object1 lightness-slider)
                  (setf text2 hue-choice-text)
                  (setf object2 hue-choice-toggles)
                  (add-subview layout text1 text1-region)
                  (add-subview layout object1 object1-region)
                  (add-subview layout text2 text2-region)
                  (add-subview layout object2 object2a-region)
                  
                  (setf highlight-by :lightness)
                  (draw-view layout :viewport vp)
                  ))))
    (setf (left-fn-of hue-toggle)
          #'(lambda (self)
              (declare (ignore self))
              (if (not (logical-value-of hue-toggle))
                (progn
                  (setf (viewed-object-of hue-toggle) t)
                  (if (logical-value-of lightness-toggle)
                    (progn (setf (viewed-object-of lightness-toggle) nil)
                           (draw-view lightness-toggle :viewport 
                                      (viewport-of lightness-toggle) :erase? T)))
                  (if (logical-value-of hue-lightness-toggle)
                    (progn (setf (viewed-object-of hue-lightness-toggle) nil)
                           (draw-view hue-lightness-toggle :viewport 
                                      (viewport-of hue-lightness-toggle) :erase? T)))                                 
                  (draw-view hue-toggle :viewport (viewport-of hue-toggle) :erase? T)
                  (sleep 1)
                  (erase-view layout :viewport vp)
                  
                  (if text1 (delete-subview layout text1))
                  (if object1 (delete-subview layout object1))
                  (if text2 (delete-subview layout text2))
                  (if object2 (delete-subview layout object2))
                  (setf text1 hue-slider-text)
                  (setf object1 hue-slider)
                  (setf text2 lightness-level-text)
                  (setf object2 lightness-level-slider)
                  (add-subview layout text1 text1-region)
                  (add-subview layout object1 object1-region)
                  (add-subview layout text2 text2-region)
                  (add-subview layout object2 object2b-region)
                  
                  (setf highlight-by :hue)
                  (draw-view layout :viewport vp)
                  ))))
    
    (setf (left-fn-of hue-lightness-toggle)
          #'(lambda (self)
              (declare (ignore self))
              (if (not (logical-value-of hue-lightness-toggle))
                (progn
                  (setf (viewed-object-of hue-lightness-toggle) t)
                  (if (logical-value-of hue-lightness-toggle)
                    (progn (if (logical-value-of hue-toggle)
                             (progn (setf (viewed-object-of hue-toggle) nil)
                                    (draw-view hue-toggle :viewport 
                                               (viewport-of hue-toggle) :erase? T)))
                           (if (logical-value-of lightness-toggle)
                             (progn (setf (viewed-object-of lightness-toggle) nil)
                                    (draw-view lightness-toggle :viewport 
                                               (viewport-of lightness-toggle) 
                                               :erase? T)))))                                  
                  (draw-view hue-lightness-toggle :viewport 
                             (viewport-of hue-lightness-toggle) :erase? T)
                  (sleep 1)
                  (erase-view layout :viewport vp)
                  
                  (if text1 (delete-subview layout text1))
                  (if object1 (delete-subview layout object1))
                  (if text2 (delete-subview layout text2))
                  (if object2 (delete-subview layout object2))
                  (setf text1 hue-slider-text)
                  (setf object1 hue-slider)
                  (setf text2 lightness-slider-text)
                  (setf object2 lightness-slider)
                  (add-subview layout text1 text1-region)
                  (add-subview layout object1 object1-region)
                  (add-subview layout text2 text2-region)
                  (add-subview layout object2 object2a-region)
                  
                  (setf highlight-by :both)
                  
                  (setf (slider-lightness-of hue-slider) 0.5)
                  (setf (slider-hue-of lightness-slider) nil)  
                  (draw-view layout :viewport vp)
                  (setf (slider-lightness-of hue-slider) light)
                  (setf (slider-hue-of lightness-slider) hue)
                  ))))
#|
    (setf (left-fn-of color-model-button)
      #'(lambda ()          
          (cond 
            ((equal "triangle" (text-of color-model-button))
             (setf (text-of color-model-button) "hexcone" color-model :hexcone))
            ((equal "hexcone" (text-of color-model-button)) 
             (setf (text-of color-model-button) "double-hexcone" 
                    color-model :double-hexcone))
            ((equal "double-hexcone" (text-of color-model-button))
             (setf (text-of color-model-button) "triangle" color-model :triangle)))
          (erase-view color-model-button :viewport (viewport-of color-model-button))
          (draw-view color-model-button 
                     :viewport (viewport-of color-model-button) :draw? t)
          (setf (color-model-of hue-slider) color-model 
                (color-model-of lightness-slider) color-model)
          (draw-view object1 :viewport (viewport-of object1) :erase? t)
          (draw-view object2 :viewport (viewport-of object2) :erase? t)
          ))
|#
    (setf (left-fn-of help-button)
          #'(lambda ()
              (let ((lightness-help (format NIL "To increase or decrease the range of ~
                                                 lightness, click and drag the ends of the ~
                                                 lightness slider in the left or right ~
                                                 directions. ~%To change the singular hue ~
                                                 value of the lightness range, click on one ~
                                                 of the color chips. ~%The other chip ~
                                                 gives the choice of any hue from the hue ~
                                                 wheel. ~%A hue selection will immediately ~
                                                 modify the colours in the lightness ~
                                                 slider."))
                    (hue-help (format NIL "To increase or decrease the range of hue, click ~
                                           and drag the ends of the slider in the left or ~
                                           right directions. ~%To change the singular ~
                                           lightness level of the hue range, move the ~
                                           lightness slider up and down. ~%A change in ~
                                           lightness will immediately modify the colours ~
                                           in the hue slider."))                   
                    (hue-lightness-help (format NIL "To increase or decrease the range of ~
                                                     hue, click and drag the ends of the ~
                                                     hue slider in the left or right ~
                                                     directions. ~%To increase or decrease ~
                                                     the range of lightness, click and drag ~
                                                     the ends of the lightness slider in ~
                                                     the left or right directions.")))
                (inform-user (case highlight-by
                               (:hue hue-help)
                               (:lightness lightness-help)
                               (:both hue-lightness-help)
                               ))
                )))
    
    (setf (left-fn-of apply-button)
          #'(lambda ()
              (let ((mat (qk::sel (eval `(,slot ,self)))))
                (case highlight-by
                  (:hue
                   (let ((low-hue (slider-low-level-of hue-slider))
                         (hi-hue (slider-high-level-of hue-slider))
                         (lightness (slider-lightness-of hue-slider))
                         )
                     (setf (q::eref mat 1 1) low-hue)
                     (setf (q::eref mat 1 2) hi-hue)
                     (setf (q::eref mat 0 0) lightness)
                     (setf (q::eref mat 2 2) :hue)
                     ))
                  (:lightness
                   (let* ((base-one (- (max-level lightness-slider) 
                                       (min-level lightness-slider)))
                          (low-light (/ (slider-low-level-of lightness-slider) base-one))
                          (hi-light (/ (slider-high-level-of lightness-slider) base-one))
                          (hue (slider-hue-of lightness-slider))
                          )
                     (setf (q::eref mat 0 1) low-light)
                     (setf (q::eref mat 0 2) hi-light)
                     (setf (q::eref mat 1 0) hue)
                     (setf (q::eref mat 2 2) :lightness)
                     ))
                  (:both
                   (let* ((low-hue (slider-low-level-of hue-slider))
                          (hi-hue (slider-high-level-of hue-slider))
                          (base-one (- (max-level lightness-slider) 
                                       (min-level lightness-slider)))
                          (low-light (/ (slider-low-level-of lightness-slider) base-one))
                          (hi-light (/ (slider-high-level-of lightness-slider) base-one))
                          )
                     (setf (q::eref mat 1 1) low-hue)
                     (setf (q::eref mat 1 2) hi-hue)
                     (setf (q::eref mat 0 1) low-light)
                     (setf (q::eref mat 0 2) hi-light)
                     (setf (q::eref mat 2 2) :both)
                     )))
                (eval `(setf (,slot ,self) ,mat))
                
                )))
    
    (setf (left-fn-of done-button)  ;window closes
          #'(lambda ()
              (sleep 0.5)
              (wb::close-canvas vw) 
              ))
    
    (setf vw (make-view-window :left left :right (+ left width) 
                               :bottom bottom :top (+ bottom height)
                               :title prompt-user-string))
    (setf vp (make-viewport vw 0 1))
    (draw-view layout :viewport vp :draw? T)
    (funcall (left-fn-of lightness-toggle) lightness-toggle)
    ))
