;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               brush.lisp
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
;;;     C.B. Hurley 1988-1991 George Washington University
;;;     R.W. Oldford 1988-1991
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(brush  shape-brush default-brush-shape
           draw-brush move-brush
           brushable-view-mixin  brush-of get-brush-menu-item
           toggle-brushing 
           reshape-brush drag-brush)))
;;;----------------------------------------------------------------------------------

;; (eval-when ...) is necessary since the brush slot of brushable-view-mixin
;; is class-allocated, and hence evaluated at compile-time
#|
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass brush ()
    ((height :allocation :class :accessor brush-height
             :initform *default-brush-height*)
     (width :allocation :class :accessor brush-width
            :initform *default-brush-width*)
     
     (brush-x :accessor brush-x :initform nil )
     (brush-y :accessor brush-y :initform nil)
     (brush-test :accessor brush-test-of :initform #'intersects-p)
     (brush-mode :accessor brushing-mode-of :initform :select)
     (drag-brushing? :accessor drag-brushing-p :initform nil)))
  
  (defun make-brush ()
    (make-instance 'brush ))
  )
|#


 (defclass brush ()
    ((height :allocation :class :accessor brush-height
             :initform *default-brush-height*)
     (width :allocation :class :accessor brush-width
            :initform *default-brush-width*)
     
     (brush-x :accessor brush-x :initform nil )
     (brush-y :accessor brush-y :initform nil)
     (brush-test :accessor brush-test-of :initform #'is-brushed-p :initarg :brush-test)
     (brush-mode :accessor brushing-mode-of :initform :select)
     (drag-brushing? :accessor drag-brushing-p :initform nil)))


(defun make-brush (&rest args)
    (apply #'make-instance 'brush  args))


(defclass  brushable-view-mixin ()
  ((brush ;;:allocation :class
          :accessor brush-of
          :initarg :brush
          :initform nil)))

(defclass  draggable-brush-view-mixin (brushable-view-mixin)
  ())




(defgeneric shape-brush (brush width height)
  (:documentation "Shape brush using width and height"))


(defgeneric locate-brush (brush region)
  (:documentation "Set brush location to region"))

(defgeneric default-brush-shape  (brush)
  (:documentation "Shape brush using default width and height"))


(defgeneric draw-brush (brush viewport 
                              &optional x y)              
  (:documentation "Draw brush at x,y in viewport"))

(defgeneric move-brush ( brush viewport &optional x y )
  (:documentation "Move brush to x,y in viewport" ))

(defgeneric toggle-brushing (brushable-view-mixin &key)
  (:documentation "Toggle brushing on-off"))






(defgeneric reshape-brush (brushable-view-mixin 
                           &key viewport width height)
  (:documentation "Reshape brush on viewport.~
                   If width or height is nil the user shapes a rectangle~
                   with the mouse"))


(defgeneric drag-brush  (brushable-view-mixin  &key viewport width height)   
  (:documentation "Drag brush on viewport"))

;;;----------------------------------------

(defmethod locate-brush((self brush) region)
  (multiple-value-bind (l r b tp) (bounds-of region)
    (setf (brush-x self) r)
    (setf (brush-width self) (- r l))
    (setf (brush-y self) b)
    (setf (brush-height self) (- tp b))))

(defmethod contains-p ((self brush) region)
  
  (multiple-value-bind (l r b tp) (bounds-of region)
    (let* ((xmax (brush-x self))
           (xmin (- xmax (brush-width self)))
           (ymin (brush-y self))
           (ymax (+ ymin (brush-height self))))
      (when (and (>= l xmin ) (<= r xmax)
                 (>= b ymin ) (<= tp ymax))
        region))))

(defmethod intersects-p ((self brush) region)
  
  (multiple-value-bind (la ra ba ta) (bounds-of region)
    (let* ((rb (brush-x self))
           (lb (- rb (brush-width self)))
           (bb (brush-y self))
           (tb (+ bb (brush-height self))))
       (when (and (<= (max la lb) (min ra rb))
                 (<= (max ba bb) (min ta tb)))
        region))))
        


(defmethod shape-brush ((self brush) width height)
  (setf (brush-width self) width)
  (setf (brush-height self) height)
  self)



(defmethod default-brush-shape  ((Self brush))
  (shape-brush self *default-brush-width* *default-brush-height*))

#|
(defmethod draw-brush ((self brush) viewport 
                       &optional  (x (brush-x self))
                       (y (brush-y self)))
  (let ((bw (window-of viewport)))
    (setf (brush-x self) x)
    (setf (brush-y self) y) 
    (wb:canvas-draw-rectangle bw (- x (brush-width self)) x y (+ y (brush-height self))
                              :operation :boole-xor)))
|#



(defmethod draw-brush ((self brush) viewport 
                       &optional x y)
  
  (let ((bw (window-of viewport)))
    (if (null x)
      (progn
        (setq x (brush-x self))
        (if (null x)
          (setq x (setf (brush-x self) (wb:mouse-x bw)))))
      (setf (brush-x self) x))
    
    (if (null y)
      (progn
        (setq y (brush-y self))
      (if (null y)
        (setq y (setf (brush-y self) (wb:mouse-y bw)))))
      (setf (brush-y self) y))
    
    (wb:canvas-draw-rectangle bw (- x (brush-width self)) x y (+ y (brush-height self))
                              :operation :boole-xor)))
 

(defmethod move-brush ((self brush ) viewport &optional x y )
  (let* (
         ;;(b (brush-bitmap self))
         (w (window-of viewport))
         (wid (brush-width self))
         (hgt (brush-height self))
         (old-x (brush-x self))
         (old-y (brush-y self)))
    (setq x (or x (wb:mouse-x w)))
    (setq y (or y (wb:mouse-y w)))
    (unless (and (= x old-x) (= y old-y ))
      (wb:canvas-draw-rectangle w (- old-x wid) old-x old-y (+ old-y hgt)
                              :operation  :boole-xor)
      (setf (brush-x self) x)
      (setf (brush-y self) y)
      (wb:canvas-draw-rectangle w (- x wid) x y (+ y hgt)
                              :operation :boole-xor)
      )))








(defmethod remake-menu :after ((self  brushable-view-mixin ) 
                               (slot-name (eql 'middle-menu))
                               &optional middle)  
  (setf (slot-value self 'middle-menu) 
        (or middle (append
                    (get-brush-menu-items self)
                    (slot-value self 'middle-menu)
                    ))))



(defmethod default-brush-test ((self  brushable-view-mixin))
  #'is-brushed-p)
    

(defmethod brush-of ((self  brushable-view-mixin))
  (or (slot-value self 'brush)
      (setf (slot-value self 'brush)
            (make-brush :brush-test (default-brush-test self)))))

(defgeneric is-brushed-p(view brush viewport)
  (:documentation "Is the image of view in viewport brushed ?"))

(defmethod is-brushed-p((self view) (brush brush) viewport)
  (contains-p brush viewport))
  


(defmethod get-brush-menu-items ((self brushable-view-mixin))
  
  '(("Shape Brush"  (reshape-brush :viewport) ) 
    ("-" nil)))

(defmethod update-menu-items :after ((self brushable-view-mixin) 
                                     (slot-name (eql 'middle-menu)))
  (update-brush-menu-items self))


(defmethod update-brush-menu-items  ((self brushable-view-mixin) )
 nil)


(defmethod get-brush-menu-items ((self draggable-brush-view-mixin))
  
  '(("Brush" nil "" :sub-items
    (("BrushMode?" (toggle-brushing))
    ("Shape Brush"  (reshape-brush :viewport) )
    ("Angle Brush"  (reangle-brush :viewport) )
    
    )) ("-" nil)))



(defmethod update-brush-menu-items ((self draggable-brush-view-mixin))
  (let* ((m (wb:get-menu-item (slot-value self 'middle-menu) "Brush"))
         (brush (brush-of self)))
    (wb:check-menu-item m "BrushMode?" (drag-brushing-p brush))
   
    #|
    
    (if (drag-brushing-p brush)
      (progn  (wb:enable-menu-item  m "BrushHigh?")
              (wb:enable-menu-item  m "Shape Brush"))
      (progn  (wb:disable-menu-item  m "BrushHigh?")
              (wb:disable-menu-item  m "Shape Brush")))
|#


))


(defmethod toggle-brushing ((self draggable-brush-view-mixin) &key)
  (let ((brush (brush-of self)))
    (setf (drag-brushing-p brush)
          (not (drag-brushing-p  brush )))))



(defmethod brush-test-of ((self brushable-view-mixin ) )
  (brush-test-of (brush-of self)))


(defmethod brushing-mode-of ((self brushable-view-mixin ) )
  (brushing-mode-of (brush-of self)))









(defmethod reshape-brush ((self brushable-view-mixin) 
                          &key viewport width height)
  
  (let ((brush (brush-of self)) 
        ignore)
    (declare (ignorable ignore))
     (unless (and width height ) 
      (multiple-value-setq (ignore ignore width height)
        
        (wb:select-rectangle :canvas (window-of viewport)
                             :width (brush-width brush)
                             :height (brush-height brush))))
    (shape-brush brush width height)
    ))





(defmethod collect-views-in-region ((self brushable-view-mixin ) &key viewport region)
  (let ((brush (brush-of self))
        (test (brush-test-of self)))
  (if region
    (progn
      (locate-brush brush region)
      (loop  for sv in (subviews-of self)
             for vis in (visible-subviews-p self)
             for vp = (select-viewport sv viewport)
             when (and vis  (funcall test sv brush vp))
             collect sv))
  (let* ((w (window-of viewport))
         (old-ang (brush-angle brush))
         (old-wid (brush-width brush))
         (old-hgt (brush-height brush)))
    (shape-brush brush *default-brush-width* *default-brush-height*)
    (reshape-brush  self :viewport viewport)
   (if (typep brush 'angled-brush)
     (rotate-brush  self :viewport viewport))
    
    (draw-brush brush viewport (wb:mouse-x w) (wb:mouse-y w))
    (loop  for sv in (subviews-of self)
           for vis in (visible-subviews-p self)
           for vp = (select-viewport sv viewport)
           when (and vis  (funcall test sv brush vp))
           collect sv
           finally (draw-brush brush  viewport)
           (shape-brush brush old-wid old-hgt)
           (set-brush-angle brush old-ang))))))




(defmethod default-left-fn ((self brushable-view-mixin ) 
                            &key viewport position)
  ;;  select, no toggle
  (declare (ignore position))
   (short-description self) 
  (let ((sel (collect-views-in-region self :viewport viewport))
         )
    (with-update-style-cache
      (loop for v in *selected-views* 
            do (set-highlight-style v  nil))
        (loop for v in sel
            do
            (set-highlight-style v  t))
       (set-selected-views  sel))))
    
 
(defmethod shift-left-button-fn ((self brushable-view-mixin ) 
                                 &key viewport position)
  (declare (ignore position))
  (short-description self)
  (let ((new (collect-views-in-region self :viewport viewport)))
    (with-update-style-cache
      (loop for v in new do
            (set-highlight-style v t)
            (pushnew v *selected-views*)))))

(defmethod shift-middle-button-fn ((self brushable-view-mixin ) 
                                   &key viewport position)
  ;; intersection
   ;; turns off the parts of highlit views which are not linked to self
  (declare (ignore position))
  (short-description self) 
  (when *selected-views*
    (let* ((old-views (remove-duplicates (highlighted-views)))
           (sel-views (collect-views-in-region self :viewport viewport)))
       (loop for v in old-views
              do (set-highlight-style v  nil :not-from sel-views :draw-links? nil))
      (set-selected-views  sel-views))))


(defmethod shift-right-button-fn ((self brushable-view-mixin ) 
                                  &key viewport position)
  (declare (ignore position))
  (short-description self) 
  (when *selected-views*
    (let ((new (collect-views-in-region self :viewport viewport)))
      (with-update-style-cache
        (loop for v in new
              ;;   when (any-highlight? v)
              do (set-highlight-style v nil)
              (set-selected-views (delete v *selected-views*)))
        *selected-views* ))))



(defmethod drag-brush ((self draggable-brush-view-mixin ) 
                       &key viewport &allow-other-keys)
  
  (let ((bw (window-of viewport)))
    (draw-brush (brush-of self) viewport (wb:mouse-x bw)
                (wb:mouse-y bw))
    (loop until (not (wb:mouse-down-p)) do
          (move-brush (brush-of self) viewport))
    (draw-brush (brush-of self) viewport )))


(defmethod default-left-fn ((self draggable-brush-view-mixin) 
                           &key viewport &allow-other-keys)
  
  (if (drag-brushing-p (brush-of self))
    (progn (setf (brushing-mode-of (brush-of self)) :select)
           (drag-brush self :viewport viewport))
    (call-next-method)))

(defmethod shift-left-button-fn ((self draggable-brush-view-mixin ) 
                                &key viewport &allow-other-keys)
  
  (if (drag-brushing-p (brush-of self))
    (progn (setf (brushing-mode-of (brush-of self)) :union)
           (drag-brush self :viewport viewport))
    (call-next-method)))




(defmethod shift-middle-button-fn ((self draggable-brush-view-mixin ) 
                                &key viewport &allow-other-keys)
  
  (if (drag-brushing-p (brush-of self))
    (progn (setf (brushing-mode-of (brush-of self)) :intersection)
           (drag-brush self :viewport viewport))
    (call-next-method)))

(defmethod shift-right-button-fn ((self draggable-brush-view-mixin ) 
                                &key viewport &allow-other-keys)
  
  (if (drag-brushing-p (brush-of self))
    (progn (setf (brushing-mode-of (brush-of self)) :difference)
           (drag-brush self :viewport viewport))
    (call-next-method)))



