;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               angled-brush.lisp
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
;;;     C.B. Hurley 1994 George Washington University
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)
(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(angled-brush set-brush-angle rotate-brush reangle-brush)))
;;----------------------------------------------------------------------------------

(defclass angled-brush (brush)
  ((angle :allocation :class :accessor brush-angle
          :initform 0)
   (brush-cache-pars :allocation :class :accessor brush-cache-pars
          :initform (make-list 6))
   ))
(defgeneric set-brush-angle (brush angle)
  (:documentation "Sets angle of brush to angle."))


(defgeneric reangle-brush (brushable-view-mixin 
                           &key viewport angle)
  (:documentation "Reangles brush on viewport.~
                   If angle is nil the user specifies an angle~
                   with the mouse"))

(defgeneric rotate-brush (brushable-view-mixin &key viewport angle)
  (:documentation "Rotates brush by angle."))


(defmethod brush-angle ((self brush)) 0)

(defmethod initialize-instance :after ((self angled-brush) &key )
  (if (zerop (brush-angle self))
    (change-class self 'brush)
    (set-brush-cache-pars self)))

(defmethod set-brush-cache-pars ((self angled-brush))
  (let* ((a (/ (brush-angle self) 180.0) )
        (c (cos a)) (s (sin a))
        (w (brush-width self))
        (h (brush-height self))
        (pars (brush-cache-pars self)))
    (setf (car pars) (round (* w c)))
    (setf (second pars) (round (* w s)))
    (setf (third pars) (round (* h c)))
    (setf (fourth pars) (round (* h s)))
    (setf (fifth pars) c) (setf (sixth pars) s)))
    


 


(defmethod contains-p ((self angled-brush) region)
  (if (zerop (brush-angle self))
    (call-next-method)
    (destructuring-bind (wc ws hc hs c s)
                        (brush-cache-pars self)
      (declare (ignore wc ws hc hs))
      (multiple-value-bind (l r b tp) (bounds-of region)
        (let* ((x (brush-x self)) (y (brush-y self))
               (w (brush-width self)) (h (brush-height self))
               db dr)
          (labels ((dbf(l b)
                     (abs (setq db (+ (* s (- x l)) (* c (- b y))))))
                   (dtf()  (abs (- db h)))
                   (drf(l b)
                     (abs (setq dr (+ (* c (- x l)) (* s (- y b))))))
                   (dlf() (abs (- dr w)))
                   (inside(l b) (and (<= (dbf l b) h) (<= (dtf) h)
                                     (<= (drf l b) w) (<= (dlf) w))))
            
            (when (and (inside l b) (inside r tp) (inside l tp) (inside r b))
              region)))))))

(defmethod intersects-p ((self angled-brush) region)
  (if (zerop (brush-angle self))
    (call-next-method)
    (or (contains-p self region)
    (destructuring-bind (wc ws hc hs c s)
                        (brush-cache-pars self)
      (declare (ignore c s))
    (let* ((x (brush-x self)) (y (brush-y self))
           (x3  (- x wc)) (y3  (- y ws))
           (x1 (- x hs)) (y1 (+ y hc))
           (x2 (- x1 wc))  (y2 (- y1 ws))
           )
    (multiple-value-bind (l r b tp) (bounds-of region)
      (or (line-segment-intersectp l b r b x y x1 y1)
          (line-segment-intersectp l b r b x2 y2 x1 y1)
          (line-segment-intersectp l b r b x2 y2 x3 y3)
          (line-segment-intersectp l b r b x y x3 y3)
          
          (line-segment-intersectp r b r tp x y x1 y1)
          (line-segment-intersectp r b r tp x2 y2 x1 y1)
          (line-segment-intersectp r b r tp x2 y2 x3 y3)
          (line-segment-intersectp r b r tp x y x3 y3)
          
          ;(line-segment-intersectp t tp l tp x y x1 y1)
          ;(line-segment-intersectp t tp l tp x2 y2 x1 y1) ;;; 08MAR2022 gwb
          ;(line-segment-intersectp t tp l tp x2 y2 x3 y3)
          ;(line-segment-intersectp t tp l tp x y x3 y3)

          (line-segment-intersectp r tp l tp x y x1 y1)
          (line-segment-intersectp r tp l tp x2 y2 x1 y1)
          (line-segment-intersectp r tp l tp x2 y2 x3 y3)
          (line-segment-intersectp r tp l tp x y x3 y3)
        
          (line-segment-intersectp l tp l b x y x1 y1)
          (line-segment-intersectp l tp l b x2 y2 x1 y1)
          (line-segment-intersectp l tp l b x2 y2 x3 y3)
          (line-segment-intersectp l tp l b x y x3 y3))))))))

(defmethod shape-brush ((self angled-brush) width height)
  (setf (brush-width self) width)
  (setf (brush-height self) height)
  (set-brush-cache-pars self)
  self)

(defmethod set-brush-angle ((self brush) angle)
  (unless (zerop angle)
    (change-class self 'angled-brush)
    (setf (brush-angle self) angle)
    (set-brush-cache-pars self)))


(defmethod set-brush-angle ((self angled-brush) angle)
    (setf (brush-angle self) angle)
    (set-brush-cache-pars self))


(defmethod locate-brush((self angled-brush) region)
  (multiple-value-bind (l r b tp) (bounds-of region)
    (setf (brush-x self) r)
    (setf (brush-width self) (- r l))
    (setf (brush-y self) b)
    (setf (brush-height self) (- tp b)))
  (set-brush-cache-pars self))

(defmethod default-brush-shape  ((Self angled-brush))
  (shape-brush self *default-brush-width* *default-brush-height*)
  (set-brush-angle self 0))


(defmethod xor-draw-brush-at-xy ((self angled-brush) viewport x y)
  (destructuring-bind (wc ws hc hs c s)
                      (brush-cache-pars self)
    (declare (ignore c s))
    (let* ((bw (window-of viewport)) 
           (x3  (- x wc)) (y3  (- y ws))
           (x1 (- x hs)) (y1 (+ y hc))
           (x2 (- x1 wc))  (y2 (- y1 ws))
           )
      (wb:canvas-move-to bw x y)
      (wb:canvas-draw-to bw x1 y1 :operation :boole-xor)
      (wb:canvas-draw-to bw x2 y2 :operation :boole-xor)
      (wb:canvas-draw-to bw x3 y3 :operation :boole-xor)
      (wb:canvas-draw-to bw x y :operation :boole-xor))))

 
(defmethod draw-brush ((self angled-brush) viewport 
                       &optional (x (brush-x self))
                       (y (brush-y self)))
  (xor-draw-brush-at-xy self viewport x y)
  (setf (brush-x self) x)
  (setf (brush-y self) y))
 

(defmethod move-brush ((self angled-brush ) viewport &optional x y )
  (let* ((w (window-of viewport))
          (old-x (brush-x self))
           (old-y (brush-y self)))
    (setq x (or x (wb:mouse-x w)))
    (setq y (or y (wb:mouse-y w)))
    (unless (and (= x old-x) (= y old-y ))
      (xor-draw-brush-at-xy self viewport old-x old-y)
      (xor-draw-brush-at-xy self viewport x y)
      (setf (brush-x self) x)
      (setf (brush-y self) y))))




(defmethod rotate-brush ((self brushable-view-mixin) &key viewport angle)
  (let ((brush (brush-of self)) )
    (change-class brush 'angled-brush)
    (if angle 
      (set-brush-angle brush angle)
       (let* ((w (window-of viewport)))
         (set-brush-angle brush 0)
     
        (draw-brush brush viewport (wb:mouse-x w) (wb:mouse-y w)) 
        
        (loop with start-time = (get-universal-time)
              with wait-time = 1/1200
              until (or (>= (- (get-universal-time) start-time) wait-time)
                        (wb:mouse-down-p)))
        (loop  while (wb:mouse-down-p)
               do
               (draw-brush brush viewport)
               (if (> (wb:mouse-y w) (brush-y brush))
                 (set-brush-angle brush  (+ (brush-angle brush) 2))
                 (set-brush-angle brush (- (brush-angle brush) 2)))
               (draw-brush brush viewport (wb:mouse-x w) (wb:mouse-y w))
               )
        (draw-brush brush viewport)
        
        ))))

;; Already defined at line 143 in identical code - chose the earlier one GWB 07SEP2023
;(defmethod set-brush-angle ((self angled-brush) angle)
;     (setf (brush-angle self) angle)
;    (set-brush-cache-pars self))


(defmethod reshape-brush ((self angled-brush) ;brushable-view-mixin) 07SEP2023
                          &key viewport width height)
  
  (let ((brush (brush-of self)) )
    (unless (and width height ) 
      (set-brush-angle brush 0)
      (let (ignore) 
        (declare (ignorable ignore))
      (multiple-value-setq (ignore ignore width height)
        (wb:select-rectangle :canvas (window-of viewport)
                             :width (brush-width brush)
                             :height (brush-height brush)))
        ))
    
    (shape-brush brush width height)
    ))


  
(defmethod reangle-brush ((self brushable-view-mixin ) 
                          &key viewport  angle)
  (let ((brush (brush-of self)))
    (if angle
      (set-brush-angle brush angle)
      (rotate-brush self :viewport viewport))
    ))


(defmethod get-brush-menu-items ((self  brushable-view-mixin))
  
  '(("Brush" nil "" :sub-items
    (
    
    ("Shape Brush"  (reshape-brush :viewport) )
    ("Angle Brush"  (reangle-brush :viewport) )
    
    )) ("-" nil)))


;; this version allows brush to be angled
