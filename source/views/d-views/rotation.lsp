;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               rotation.lisp
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
;;;     C.B. Hurley 1988-1992 George Washington University
;;;     R.W. Oldford 1988-1992
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)


(eval-when (:compile-toplevel :load-toplevel :execute) (export '(rotating-cloud  recenter-scaled-coords
          rotate-points set-rotation  scaled-axis-coords-of 
           compute-scaled-axis-coords 
           viewport-axis-coords-of draw-tripod erase-tripod
          set-draw-axis-p set-draw-label-p set-increment)))



(defclass rotating-cloud (viewport-coords-cache-mixin  square-view-mixin moving-cloud-mixin)
  ( (middle-menu :allocation :class :initform nil)
    (style-keys :initform '(:font :size :fill? :color :axis-color) :allocation :class)
    (rotation 
    :initarg :rotation 
    :initform nil
    :accessor rotation-of)
    (scaled-axis-coords-cache 
    :initform nil 
    :accessor scaled-axis-coords-cache-of)
    (draw-axis? 
     :initform t 
     :initarg :draw-axis?
     :accessor draw-axis-p)
    (draw-label? 
     :initform t 
     :initarg :draw-label?
     :accessor draw-label-p)
    (increment 
     :initform  (/ pi 30)
     :initarg  :increment 
     :accessor increment-of)
     (x-text :initform nil :initarg :x-text :accessor x-text-of)
    (y-text :initform nil :initarg :y-text :accessor y-text-of)
    (z-text :initform nil :initarg :z-text :accessor z-text-of)
   )
  (:default-initargs :font wb:*very-small-graphics-font* 
    :axis-color *default-axis-color*
    :viewport-compute-method #'compute-viewport-axis-coords)
  )


(defgeneric recenter-saled-coords (rotating-cloud &optional center)
  (:documentation "Shifts the scaled coords so that center appears at the ~
                   viewports center. ~
                   If center is :selected the center of highlit subviews is used. ~
                   If center is :original the original (default) center is used. "))
  
(defgeneric rotate-points (cloud &key viewport)
  (:documentation "rotates the cloud in viewport"))
                          
(defgeneric set-rotation (cloud &key rotation)
  (:documentation "sets rotation of cloud. Default is identity"))

(defgeneric compute-scaled-axis-coords (cloud)
  (:documentation "computes and returns standardized origin and endpoints of axes"))
                         
(defgeneric scaled-axis-coords-of (cloud)
  (:documentation "returns standardized origin and endpoints of axes"))
  
  




(defgeneric viewport-axis-coords-of (cloud viewport)
  (:documentation "returns the origin and endpoints of axes in viewport coords"))
  
(defgeneric draw-tripod (cloud &key viewport labels? )
  (:documentation "draws a tripod for axes"))

(defgeneric erase-tripod (cloud &key viewport labels? )
  (:documentation "erases  tripod for axes"))

(defgeneric set-draw-axis-p (cloud val )
  (:documentation "sets the value of accessor draw-axis-p and redraws"))

(defgeneric set-draw-label-p (cloud val )
  (:documentation "sets the value of accessor draw-label-p and redraws"))

(defgeneric set-increment (cloud val )
  (:documentation "sets the stepsize for rotation"))

;;--------------------------------------------------------------------------------

(defmethod description-string ((self rotating-cloud) )
  
  (format nil "~A ~% X: ~A  Y: ~A Z: ~A" 
          (call-next-method)
          (first (rotation-of self))
          (second (rotation-of self))
          (third (rotation-of self))))
  



(defun identity-3d-rotation ()
  "a list of lists for an identity rotation in 3d"
 (list (list 1 0 0) ( list 0 1 0) ( list 0 0 1)))



#|
;; same as below which works in mcl 2.0.1 but not in mcl 2.0

(defmethod rotate-coords ((self rotating-cloud) coords &key (integer? t))
  (setf (rotation-of self) (or (rotation-of self) (identity-3d-rotation)))
  (loop with fn = (if integer? #'round #'identity)
         with rot = (rotation-of self)
        with x1 = (elt (elt rot 0) 0)
         with x2 = (elt (elt rot 1) 0)
         with x3 = (elt (elt rot 2) 0)
         with y1 = (elt (elt rot 0) 1)
         with y2 = (elt (elt rot 1) 1)
         with y3 = (elt (elt rot 2) 1)
         with z1 = (elt (elt rot 0) 2)
         with z2 = (elt (elt rot 1) 2)
         with z3 = (elt (elt rot 2) 2)
        
        for (a b c) in coords
        collect
        (if (and (numberp a) (numberp b) (numberp c))
          (list (funcall fn (+ (* x1 a) (* x2 b) (* x3 c)))
                (funcall fn  (+ (* y1 a) (* y2 b) (* y3 c)))
                (funcall fn   (+ (* z1 a) (* z2 b) (* z3 c)))))))
|#

(defmethod rotate-coords ((self rotating-cloud) coords &key (integer? t))
  (setf (rotation-of self) (or (rotation-of self) (identity-3d-rotation)))
  (loop with fn = (if integer? #'round #'identity)
        with ((x1 y1 z1) 
              (x2 y2 z2)
              (x3 y3 z3)) float  = (rotation-of self)
        
        for (a b c) in coords
        collect
        (if (and (numberp a) (numberp b) (numberp c))
          (list (funcall fn (+ (* x1 a) (* x2 b) (* x3 c)))
                (funcall fn  (+ (* y1 a) (* y2 b) (* y3 c)))
                (funcall fn   (+ (* z1 a) (* z2 b) (* z3 c)))))))





(defmethod compute-plot-coords ((self rotating-cloud))
  (rotate-coords self (scaled-coords-of self)))
  

(defmethod centered-viewport-axis-coords-of ((self rotating-cloud) 
                                             viewport)
  (scale-data-for-viewport self
                           (rotate-coords self (scaled-axis-coords-of self ))
                           viewport))




(defmethod set-bounding-region :after ((self rotating-cloud) &key  &allow-other-keys)
  (setf (scaled-axis-coords-cache-of self) nil))


(defmethod smallest-bounding-region ((self rotating-cloud) )
  (let ((br (make-region)))
    (if (cases-of self)
      (loop   for (x y z) in  (scaled-coords-of self )
              for s in (case-status-of self)
              when (active-status-p s)
              maximize (+ (* x x) (* y y) (* z z)) into radius
             finally 
             (setq radius (ceiling  (sqrt radius)))
             (if (zerop radius) (incf radius 1))
             (setf (bounds-of br) 
                   (list (- radius) radius 
                         (- radius) radius))))
    br))




(defmethod recenter-scaled-coords ((self rotating-cloud) &optional center)
  (let (cx cy cz )
    (cond ((listp center)
           (setq cx (first center) cy (second center) cz (third center)))
          ((eq center :selected)
           (loop  for  sub in (subviews-of self)
                  for s in (case-status-of self)
                  for (x y z)  in (scaled-coords-of self)
                  when (and (draw-style sub :highlight?) (active-status-p s))
                  sum x into sumx and
                  sum y into sumy and sum z into sumz and
                  count 1 into n
                  finally (unless (zerop n)
                            (setq cx (round (/ sumx n))  
                                  cy (round (/ sumy n))  
                                  cz (round (/ sumz n))))))
          ((eq center :original)
           (setf (scaled-coords-cache-of self) nil)
           (setf (plot-coords-cache-of self) nil))
          (t nil))
    
    (when (and (numberp cx) (numberp cy) (numberp cz))
      (loop for s in (case-status-of self)
            for xyz in (scaled-coords-of self)
            unless (invalid-status-p s)
            do (decf (car xyz) cx)
            (decf (second xyz) cy)
            (decf (third xyz) cz))
      (let* ((r (rotation-of self))
             (sx (round (+ (* (elt (elt r 0) 0) cx) 
                           (* (elt (elt r 1) 0) cy) 
                           (* (elt (elt r 2) 0) cz))))
             (sy (round (+ (* (elt (elt r 0) 1) cx) 
                           (* (elt (elt r 1) 1) cy) 
                           (* (elt (elt r 2) 1) cz))))
             (sz (round  (+ (* (elt (elt r 0) 2) cx) 
                            (* (elt (elt r 1) 2) cy) 
                            (* (elt (elt r 2) 2) cz)))))
        (loop for s in (case-status-of self)
              for xyz in (plot-coords-of self)
              unless (invalid-status-p s)
              do (decf (car xyz) sx)
              (decf (second xyz) sy)
              (decf (third xyz) sz))))))

(defmethod bounds-of-selected ((self rotating-cloud))
  
  (loop  for sub in (subviews-of self)
                for status in (case-status-of self)
                for (x y z)  in (scaled-coords-of self)
                when (and (draw-style sub :highlight?) (active-status-p status))
                maximize (+ (* x x) (* y y) (* z z)) into radius
                finally 
                (when  radius
                (setq radius (ceiling (sqrt radius)))
                (if (zerop radius) (incf radius 1))
                (return (make-region (- radius) radius (- radius) radius)))))





(defmethod new-bounds ((self rotating-cloud) &key (region :compute) pretty?)
  (unless (region-p region)
    (setq region 
          (case region
            (:prompt
             (apply #'make-region
                    (wb:prompt-user :type 'list :read-type :read
                                    :prompt-string "(left right bottom top)")))
            (:original (recenter-scaled-coords self  :original)
                       (adjust-subviews self )
                       (original-bounds self :draw? t)
                       nil)
             (t (recenter-scaled-coords self :selected)
                (adjust-subviews self )
                (setq region (bounds-of-selected self))
                
               region))))
  (when (region-p region) (change-bounding-region self region :pretty? pretty?))
  )
                      


(defmethod stop-rotate ((self rotating-cloud))
  (if (wb:mouse-down-p)
    #'(lambda() (not (wb:mouse-down-p)))
    #'(lambda() (wb:mouse-down-p))))

(defmethod rotate-points ((self rotating-cloud) &key 
                          viewport (direction :y) (steps 1000) 
                          (increment (increment-of self)))
  
  (unless viewport
    (setq viewport
          (if (> (length (viewports-of self)) 1)
            (which-viewport self)
            (car (viewports-of self)))))
  
  (if (and viewport (not (moving-p self))
           (active-viewport-p viewport))
    (let* ((temp-coords (centered-viewport-coords-of self  viewport))
           (fill? (subview-styles self :fill?))
           (axis-color (draw-style self :axis-color))
           (colors (subview-styles self :color))
           (symbols (subview-styles self :symbol :box))
           (sizes (subview-styles self :size 4))
           (invisible? (subview-styles self :invisible?))
            (draw-region (get-draw-portion self viewport))
           (viewport-coords (viewport-locns-of self  viewport))
           (ignore-points (loop for i in invisible?
                  for s in (case-status-of self)
                  for vis in (visible-subs-p self)
                  collect (or i (not vis)  (not (active-status-p s))))) 
           (draw-rate (draw-rate-of self))
           )
      
      (unwind-protect
        (progn
          (setf (moving-p self) t)
         (set-draw-style self :highlight? nil)
          (loop for s in (subviews-of self) 
                when (draw-style s :highlight?) do
                (set-drawing-style s :highlight? nil))
                
           (if (draw-axis-p self)
            (erase-tripod self :viewport viewport))
          (if (draw-label-p self)
              (erase-tripod-labels self :viewport viewport))
             
          (multiple-value-bind 
            (rot end-coords end-axes)
            (wb:rotate-point-cloud (window-of viewport) 
                                   temp-coords
                                   :axes (if (draw-axis-p self) 
                                           (centered-viewport-axis-coords-of self  viewport))
                                   :axis-color axis-color
                                   :size sizes :symbol symbols :fill? fill?
                                   :color colors  :invisible? ignore-points
                                   :erase-points viewport-coords
                                   :erase-axes (if (draw-axis-p self) 
                                                 (viewport-axis-coords-of self  viewport))
                                   :stop-fn (stop-rotate self)
                                   :plot-rgn (wb-region draw-region)
                                   :direction direction :steps steps :increment increment
                                   :viewport-coords? nil
                                   :draw-rate draw-rate)
            
            (wb:mapply-rotation! rot (rotation-of self) :integer? nil)
            
            (setf (plot-coords-cache-of self) nil)
            
            (adjust-subviews self )
            (set-viewport-locns self :viewport viewport :coords end-coords)
            (set-viewport-coords self :viewport viewport :coords end-axes)
            (if (draw-label-p self)
              (draw-tripod-labels self :viewport viewport))
            
            (loop for vp in (viewports-of self)
               unless (eq vp viewport) 
                  do
                  (remove-viewport-coords self :viewport vp)
                  (set-viewport-locns self :viewport vp)
                  (draw-view self :erase? nil :viewport vp))))
        (setf (moving-p self) nil)
        (draw-view self :erase? t :viewport viewport)
        ))))



(defmethod set-rotation ((self rotating-cloud) 
                         &key (rotation (identity-3d-rotation)))
  (clear-view-viewport self)
  (setf (rotation-of self) rotation)
  (setf (plot-coords-cache-of self) nil)
  (remove-viewport-coords self) 
  (adjust-subviews self )
  (set-viewport-locns self )
  (draw-view self :erase? nil))
  

(defmethod erase-view ((self rotating-cloud) 
                       &key viewport)
  
  (clear-view-viewport self :viewport viewport))




(defmethod draw-view :after ((self rotating-cloud)
                      &key viewport)
  (when (draw-axis-p self)
    (draw-tripod self :viewport viewport)))

(defmethod set-drawing-style :around ((self rotating-cloud)
                      &key)
  (when (draw-axis-p self)
    (erase-tripod self ))
  (call-next-method)
  (when (draw-axis-p self)
    (draw-tripod self )))








(defmethod initialize-instance :after ((self rotating-cloud) 
                                       &key x-text y-text z-text )
  (unless x-text
  (setf (x-text-of self)  (coord-string-x self)))
   (unless y-text (setf (y-text-of self) (coord-string-y self)))
   (unless z-text (setf (z-text-of self) (coord-string-z self))))


;;; axes







(defmethod compute-scaled-axis-coords ((self rotating-cloud))
  (let* ((r (radius-of (bounding-region-of self)))
         (m (round (* 0.8 r))))
 
        (list (list 0 0 0)
                      (list m 0 0)
                      (list 0 m 0)
                      (list 0 0 m))))


(defmethod scaled-axis-coords-of ((self rotating-cloud) )
  (or (scaled-axis-coords-cache-of self)
        (setf (scaled-axis-coords-cache-of self) 
              (compute-scaled-axis-coords self ))))



(defmethod compute-viewport-axis-coords ((self rotating-cloud) 
                                    viewport)
  ;; returns the coords of self scaled to the viewport for rotation
  
  (setq viewport (or viewport (car (viewports-of self))))
  (let* ((temp (centered-viewport-axis-coords-of self  viewport))
          (shift (wb:make-shift-transform (wb-region viewport))))
    (wb:mapply-transform shift temp)))

(defmethod viewport-axis-coords-of ((self rotating-cloud) 
                                    viewport)
  (viewport-coords-of self :viewport viewport))
   

(defmethod draw-tripod ((self rotating-cloud) 
                        &key viewport (operation :default) (labels? (draw-label-p self)))
  (loop for vp in (if viewport (list viewport) (viewports-of self))
        when (active-viewport-p vp) do
        
        (loop with points = (viewport-axis-coords-of self  vp)
              with bw = (window-of vp)
              with o = (first points)
              with font = (draw-style self :font)
              with color = (draw-style self :axis-color)
              for p in (cdr points)
              for c in (list (x-text-of self) (y-text-of self) (z-text-of self)) do
              (wb:canvas-draw-line bw (car o) (cadr o) 
                                   (car p ) (cadr p) 
                                   :operation operation 
                                   :color color
                                   :width 1)
              (if labels?
                (wb:canvas-draw-string bw c :font font
                                       :color color))
              )))

(defmethod erase-tripod ((self rotating-cloud) &key viewport (labels? (draw-label-p self)))
  (loop for vp in (if viewport (list viewport) (viewports-of self))
        when (active-viewport-p vp) do
        (loop with points = (viewport-axis-coords-of self  vp)
              with bw = (window-of vp)
              with font = (draw-style self :font)
              with o = (first points) 
              for p in (cdr points)
              for c in (list (x-text-of self) (y-text-of self) (z-text-of self)) do
              (wb:canvas-erase-line  bw (car o) (cadr o) 
                                    (car p ) (cadr p) :operation :boole-andc1
                                    :width 1 )
              (when labels?
                (wb:canvas-erase-string bw c  :font font ))
              ) ) )

(defmethod draw-tripod-labels ((self rotating-cloud) 
                        &key viewport  )
  (loop for vp in (if viewport (list viewport) (viewports-of self))
        when (active-viewport-p vp) do
        
        (loop with points = (viewport-axis-coords-of self vp)
              with bw = (window-of vp)
              with font = (draw-style self :font)
              
              with color = (draw-style self :axis-color)
              for p in (cdr points)
              for c in (list (x-text-of self) (y-text-of self) (z-text-of self)) do
              (wb:canvas-move-to bw (car p ) (cadr p) )
              (wb:canvas-draw-string bw c :font font :color color))))

(defmethod erase-tripod-labels ((self rotating-cloud) 
                        &key viewport  )
  (loop for vp in (if viewport (list viewport) (viewports-of self))
        when (active-viewport-p vp) do
        
        (loop with points = (viewport-axis-coords-of self  vp)
              with bw = (window-of vp)
              with font = (draw-style self :font)
              for p in (cdr points)
              for c in (list (x-text-of self) (y-text-of self) (z-text-of self)) do
              (wb:canvas-move-to bw (car p ) (cadr p) )
              (wb:canvas-erase-string bw c :font font ))))



#|
(defmethod change-variable :around ((self rotating-cloud) &rest args
                            &key (draw? t))
  (apply #'new-variable self :draw? draw? args))
|#

(defmethod styles-to-subs ((self rotating-cloud) ) 
  (list :highlight? :fill? :size :color ))

(defmethod get-rotation-menu-items ((self  rotating-cloud) )
  
  `(("Start" (set-rotation ))
    ("Axes" (set-draw-axis-p :toggle))
    ("Labels" (set-draw-label-p :toggle))
    ("Font" nil "" :sub-items ,(font-menu-items))
    ("-" nil)
    ))



(defmethod get-rotation-menu-items ((self  rotating-cloud) )
  
  `(("Start"  nil "" :sub-items 
     (("XY" (set-rotation ))
      ("XZ" (set-rotation :rotation ,(list (list 1 0 0) (list 0 0 1) (list 0 1 0))))
      ("YX" (set-rotation :rotation ,(list (list 0 1 0) (list 1 0 0) (list 0 0 1))))
      ("YZ" (set-rotation :rotation ,(list (list 0 1 0) (list 0 0 1) (list 1 0 0))))
      ("ZX" (set-rotation :rotation ,(list (list 0 0 1) (list 1 0 0) (list 0 1 0))))
      ("ZY" (set-rotation :rotation ,(list (list 0 0 1) (list 0 1 0) (list 1 0 0))))))
    ("Axes" (set-draw-axis-p :toggle))
    ("Labels" (set-draw-label-p :toggle))
    ("Font" nil "" :sub-items ,(font-menu-items))
    ("-" nil)
    ))


(defmethod update-menu-items :before ((self rotating-cloud) 
                                      (slot-name (eql 'middle-menu)))
  (let* ((m (slot-value self slot-name)))
    (wb:check-menu-item m "Axes" (draw-axis-p self ))
    (wb:check-menu-item m "Labels" (draw-label-p self ))))
    



(defmethod remake-menu :after ((self  rotating-cloud ) 
                               (slot-name (eql 'middle-menu))
                               &optional middle)  
  (setf (slot-value self 'middle-menu) 
        (or middle (append
                    (get-rotation-menu-items self)
                    (slot-value self 'middle-menu)
                    ))))



(defmethod set-draw-axis-p ((self rotating-cloud) val)
  (if (eq val :toggle)
    (setq val (not (draw-axis-p self))))
  (setf (draw-axis-p self) val)
  (if (null val) 
    (setf (draw-axis-p self) nil))
  (if val
    (draw-tripod self) (erase-tripod self :labels? t)))

(defmethod set-draw-label-p ((self rotating-cloud) val)
  (if (eq val :toggle)
    (setq val (not (draw-label-p self))))
  (if val
    (if (draw-axis-p self)
      (setf (draw-label-p self) t))
    (setf (draw-label-p self) nil))
  (if (draw-label-p self)
    (draw-tripod-labels self) (erase-tripod-labels self)))

(defmethod set-increment ((self rotating-cloud) val)
  (case val
    (:faster (setq val (* (increment-of self) 1.5)))
    (:slower (setq val (/ (increment-of self) 1.5)))
    (t nil))
  (setf (increment-of self) val))
  


(defmethod reposition-view :after ((self rotating-cloud)
                            &key default-positions draw?)
  (declare (ignore default-positions))
  (when (and draw? (draw-axis-p self))
    (draw-tripod self )))


(defmethod new-variable  :before ((self rotating-cloud) &key draw?)
 
    (if (and draw? (draw-axis-p self)) (erase-tripod self ))
   
    (setf (scaled-axis-coords-cache-of self) nil)
    (setf (plot-coords-cache-of self) nil)
    (setf (viewport-coords-cache-of self) nil)
   )

(defmethod new-variable  :after ((self rotating-cloud) &key draw?)
  
    (setf (x-text-of self) (coord-string-x self))
    (setf (y-text-of self) (coord-string-y self))
    (setf (z-text-of self) (coord-string-z self))
    (if (and draw? (draw-axis-p self)) (draw-tripod self ))
    
    )

(defmethod new-case-status :before  ((self rotating-cloud) 
                                     cases status &key rescale?)
  (declare (ignore cases status)) 
  (when rescale? 
    (setf (scaled-axis-coords-cache-of self) nil)
    (setf (plot-coords-cache-of self) nil)
    (setf (viewport-coords-cache-of self) nil)
    ))


(defmethod new-case-status :after  ((self rotating-cloud) 
                                     cases status &key rescale?)
  (declare (ignore cases status)) 
  (when rescale? 
     (adjust-subviews self)))
    




(defmethod move-points ((self rotating-cloud) &rest args )
  (apply #'rotate-points self args))


(defmethod reset-bounding-region ((self rotating-cloud) &key (draw? nil ) )
  (if draw? (erase-view self))
   (set-bounding-region self)
  (remap-to-viewports self :erase? nil :draw? draw?)
  )
  
 



(defmethod visible-subviews-p ((self rotating-cloud))
  
  (loop with br = (bounding-region-of self)
        with sr = (expt (radius-of br) 2)
        for (x y z) in  (scaled-coords-of self )
        for s in (case-status-of self)
        collect (and (active-status-p s)
                       (<=  (+ (* x x) (* y y) (* z z)) sr))))




(defmethod margin-string-bottom ((self rotating-cloud))
  (coord-strings  self))

(defmethod margin-string-top ((self rotating-cloud))
  (coord-strings  self))

(defmethod margin-string-left ((self rotating-cloud))
  )

(defmethod margin-string-right ((self rotating-cloud))
  )



(defmethod use-y-axis-p ((self rotating-cloud))
  nil)


(defmethod use-x-axis-p ((self rotating-cloud))
  nil)
