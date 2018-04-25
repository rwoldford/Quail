;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               pie.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(pie set-pie-colors set-weights)))
;;;----------------------------------------------------------------------------------

(defclass pie (view-with-size multiple-draw-style-mixin linkable-mixin simple-view )
                    
  ((collect-styles?  :initform t :initarg collect-styles? 
                     :accessor collect-styles-p)
   (weights :initform nil :initarg :weights :accessor weights-of )
   (style-keys :initform '(:fill? :color ) :allocation :class)
   (middle-menu :allocation :class :initform nil)
   (circle? :initarg :circle? :initform t :accessor circle-p))
  (:default-initargs :fill? *default-pie-fill?* :color  *default-pie-color* :size nil
    :viewed-elements :expand-viewed-object ))
;;;----------------------------------------------------------------------------------

(defmethod initialize-instance :after ((self pie)
                                       &rest initargs &key batches weights)
  (declare (ignore initargs))
  (if batches (setf (viewed-object-of self) batches))
  (unless weights (setf (weights-of self) #'(lambda(b) (length (list-cases b))))))

(defmethod fix-viewports ((self pie) &key viewport ) 
  (let ((size (slot-value self 'view-size))) 
   (if (numberp size) (loop for vp in (if viewport 
                      (list viewport) (viewports-of self) )
          do (set-viewport-size vp size)))))

(defmethod set-pie-colors ((self pie) colors )
  (with-update-style-cache
   (loop with n = (length (drawing-styles-of self))
         for i upfrom 0
         for j = (mod  i n)
        for e in (list-viewed-elements self) do
        (set-drawing-style self :element e :color (nth j colors)))))
        
(defmethod view-size-of ((self pie)) 
  (let ((size (slot-value self 'view-size)))
   (if (numberp size) size 10)))

(defmethod set-weights ((self pie) weights &key draw?)
   (setf (slot-value self 'weights) weights)
   (if draw? (draw-view self)))


(defmethod weights-of ((self pie))
  (let ((w (slot-value self 'weights)))
    (cond ((and (listp w) (and (= (length w) (length (list-viewed-elements self)))))
            w)
          ((functionp w) 
            (mapcar w (list-viewed-elements self)))
          ((identifier-p w) 
            (loop with vars = (list-variates (viewed-object-of self))
                  for c in (list-viewed-elements self) collect 
                  (value-of c w :variates vars)))
           
           (t nil))))


(defmethod pie-count ((self pie))
  (let ((w (weights-of self)))
    (if w
      (reduce #'+ w)
      (length (list-viewed-elements self)))))


(defmethod report-style-counts ((self pie) )
  (let* ((props 
         (second (if (collect-styles-p self)
           (style-proportions self '( :highlight? :fill? :color   ) 
                              (weights-of self))
           (style-proportions-in-order self '(:highlight? :fill? :color     )
                                       (weights-of self)))))
        (wsum (pie-count self))
        (counts (loop 
                  for c in props
                  collect (* wsum c))))
   (quail-print (format nil "Segment counts: ~{~A~^,~}, Total: ~A" counts wsum))
    (quail-print (format nil "Segment proportions: ~{~5,3F~^,~}" props))
    ))


(defmethod draw-view ((self pie) &key viewport )
  
  ;; color and fill self in proportion to the colors/fill of the drawing-style
  (with-exposed-viewports self viewport vp
    (let* ((c (centre-of vp ))
           (xc (2d-position-x c))
           (yc (2d-position-y c))
           (xr (truncate (width-of vp) 2))
           (yr (truncate (height-of vp) 2))
           (w  (window-of vp)))
      
      (if (circle-p self)
        (setq xr (min xr yr) yr xr))
      
      (loop with start = 0
            with styles  = (if (collect-styles-p self)
                             (style-proportions self '(:fill?  :color  :highlight? ) (weights-of self))
                             (style-proportions-in-order self '(:fill?  :color  :highlight? ) (weights-of self)))
            
            for ( fill col hi) in (first styles)
            for prop in (second styles)
            for angle = (round (* prop 360))
            do 
            (cond (hi (wb:canvas-highlight-arc w start angle xc yc xr yr
                                               :color *default-highlight-color*
                                               :operation :default))
                  (fill (wb:canvas-draw-filled-arc
                         w  start angle xc yc xr yr
                         :color col))
                  (t (wb:canvas-draw-arc
                      w  start angle xc yc xr yr
                      :color col)))
            (incf start angle)
            ))))

(defmethod highlight-view ((self pie) &key viewport )
  
  ;; highlight a proportion of each color-fill combintation
  ;; by examining drawing styles
  
   (with-exposed-viewports self viewport vp
    (let* ((c (centre-of vp ))
           (xc (2d-position-x c))
           (yc (2d-position-y c))
           (xr (truncate (width-of vp) 2))
           (yr (truncate (height-of vp) 2))
           (w  (window-of vp)))
      
      (if (circle-p self)
        (setq xr (min xr yr) yr xr))
      
      (loop with start = 0
              with styles  = (if (collect-styles-p self)
                               (style-proportions self '(:fill?  :color  :highlight? ) (weights-of self))
                               (style-proportions-in-order self '(:fill?  :color  :highlight? ) (weights-of self)))
              
              for ( fill col hi) in (first styles)
              for prop in (second styles)
              for angle = (round (* prop 360))
              do 
              (cond (hi (wb:canvas-highlight-arc w start angle xc yc xr yr
                                       :color *default-highlight-color*
                                       :operation :default))
                    (fill (wb:canvas-draw-filled-arc
                 w  start angle xc yc xr yr
                 :color col))
                    (t (wb:canvas-draw-arc
                 w  start angle xc yc xr yr
                 :color col)))
              (incf start angle)
              ))))




(defmethod highlight-operation ((self pie))
  :draw)

(defmethod get-menu-items ((self pie) (slot-name (eql 'middle-menu)))
  
  '(( "CollectStyles?" (set-collect-styles-p  :toggle :draw? t))
    ( "Counts" (report-style-counts))))


(defmethod style-menu-items ((self pie))
  '(("Fill?" (set-drawing-style :fill? :toggle :highlit?  *selected-subviews-only*))))

(defmethod set-collect-styles-p ((self pie) val &key draw? &allow-other-keys)
  (if (eq val :toggle)
    (setf (collect-styles-p self) (not (collect-styles-p self)))
    (setf (collect-styles-p self) val))
  (if draw? (draw-view self)))




(defmethod update-menu-items :after  ((self pie) 
                              (slot-name (eql 'middle-menu)))
  (wb:check-menu-item (slot-value self 'middle-menu)
                      "CollectStyles?" (collect-styles-p self)))


(defmethod fix-viewports ((self pie) &key viewport )
  (if (circle-p self)
    (let ((size (view-size-of   self)))
      (loop for vp in (if viewport 
                        (list viewport ) (viewports-of self) )
            do
            ;;(setf (radius-of vp) (1+ size))
            (set-square-viewport-size vp (+ 2 size))
            ))
    (call-next-method)))



