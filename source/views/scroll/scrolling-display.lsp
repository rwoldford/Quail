
;;;
;;;                               scrolling-display.lisp
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
;;;     C.B. Hurley 1992 George Washington University
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(scrolling-display scroller-of scroller-displays-of )))




(defclass scrolling-display (compound-view position-key-mixin) 
  ((right-scroller :initform nil :accessor right-scroller-of)
   (bottom-scroller :initform nil :accessor bottom-scroller-of)
   (displays :initform nil :accessor scroller-displays-of)
   (position-keys :initform '(:gap) :allocation :class))
  (:default-initargs :right-scroller t :bottom-scroller t :gap 0))




(defmethod construct-sub-views  ((self scrolling-display) 
                                 &key displays display 
                                 right-scroller bottom-scroller)
  (if display (setq displays (list display)))
    
  (when displays
    (let* ((rs (if right-scroller
                 (scroll-bar
                  :min 0 
                  :max  100
                  :step 1
                  :orientation :vertical
                  :needle-size 12
                  :scrolling-views displays
                  :scroll-fn #'move-view-start)))
           (bs (if bottom-scroller
                 (scroll-bar
                  :min 0 
                  :max  100
                  :step 1
                  :orientation :horizontal
                  :needle-size 0.03
                  :scrolling-views nil
                  :scroll-fn nil)))
           scrollers)
      (if rs (push rs scrollers))
      (if bs (push bs scrollers))
      (setf (subviews-of self) (append displays scrollers))
      (setf (scroller-displays-of self) displays)
      (setf (right-scroller-of self) rs)
      (setf (bottom-scroller-of self) bs))))


#|
(defmethod compute-sub-viewports ((self scrolling-display)
                                  &optional viewport subviews)
  (declare (ignore subviews))
  (loop with s-vp
        with gap = (getf (default-positions-of self) :gap)
        for vp in (if viewport (list viewport) (viewports-of self))
        do
        (multiple-value-bind (l r b tp) (bounds-of vp)
          (let* ((rs (right-scroller-of self))
                (bs (bottom-scroller-of self))
                (d (scroller-displays-of self))
                (scr-left (if rs (- r 15) r))
                (scr-bot (if bs (+ b 15) b)))
             (when rs 
             (setq s-vp (or (select-viewport rs vp) (make-viewport (window-of vp))))
              (setf (bounds-of s-vp) (list scr-left r scr-bot tp))
              (add-viewport rs s-vp vp))
            (when bs 
              (setq s-vp (or (select-viewport bs vp) (make-viewport (window-of vp))))
              (setf (bounds-of s-vp) (list l scr-left b scr-bot))
              (add-viewport bs s-vp vp))
                (incf l gap) (decf tp gap) (decf scr-left gap) (incf scr-bot gap) 
       
            (loop with reg-l = l
                  for di in d
                  for sv-vp = (or (select-viewport di vp) (make-viewport (window-of vp)))
                  for reg-w = (draw-region-width di)
                  for reg = (make-region reg-l (min scr-left (+ reg-l reg-w)) scr-bot tp )
                                            
                  do (setf (bounds-of sv-vp) reg)
                  (add-viewport di sv-vp vp)
                  (setq reg-l (min scr-left (+ reg-l reg-w ))))) )) )                    
       
 (defmethod reshape-sub-viewports ((self scrolling-display) viewport  
                                  &key new-location transform )
  (declare (ignore new-location transform))
  (map-subviews-to-viewport self viewport))


(defmethod draw-region-width ((self scrolling-display))
  (+ (if (right-scroller-of self) 15 0)  (* 2 (getf (default-positions-of self) :gap))
     (reduce #'+ (mapcar  #'draw-region-width (scroller-displays-of self)))))

(defmethod draw-region-height ((self scrolling-display))
  (+ (if (bottom-scroller-of self) 15 0)
     (* 2 (getf (default-positions-of self) :gap))
     (loop for d in (scroller-displays-of self)
           maximize
           (draw-region-height d))))



    
(defmethod make-draw-viewport ((self scrolling-display) &optional title)
 (let* ((w (draw-region-width self))
         (h (draw-region-height self))
         (topi 40) (righti 5) (scroll 15)
         (win (make-view-window 
             :title title
             :left (max 0
                        (- (wb:screen-width) righti scroll w))
             :bottom (max 0 
                          (- (wb:screen-height) topi scroll h))
             :right (- (wb:screen-width) righti)
             :top (- (wb:screen-height) topi))))
    (make-viewport win)))

|# 

  

