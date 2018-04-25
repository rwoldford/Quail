;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               barchart.lisp
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



;;;----------------------------------------------------------------------------------
(defclass barchart (histogram-view)
  ((middle-menu :allocation :class
                 :initform nil))
  (:default-initargs :color *default-bar-color* :fill? *default-bar-fill?*))




(defmethod valid-coord-test ((self barchart) c)
  c)



(defmethod smallest-bounding-region  ((self barchart) )
  (let ((coords (active-members self (plot-coords-of self)))
        min max hgt)
    (if (cases-of self)
      (progn
        (loop for c in coords
              when (numberp c)
              maximize c into amax and
              minimize c into amin
              finally (setq min (or amin 0)  max (or 1 amax)))
        
        
        (setq hgt (apply #'max (bar-heights-of self)))
        (if (eq (orientation-of self) :vertical)
          (make-region  0.0 hgt min max)
          (make-region  min max 0.0 hgt )
          ))
      (make-region))))
   

(defmethod bar-heights-of ((self barchart))
  (compute-bar-heights self (histogram-scale-of self)))


(defmethod construct-sub-views ((self barchart)
                                &key  bars bar-coords
                                fill?
                                color)
  
  (let* ((group-vals (or bar-coords
                         (remove-duplicates (coords-of self) :test #'equal)))
         (orientation (orientation-of self))
         (style-arglist (append
                         bars
                         (list :color color :fill? fill? )))
         sub-styles sub-vos)
    (setq bars (subview-arg-list bars 'bar))
    
    (multiple-value-setq ( sub-vos sub-styles)
      (group-viewed-objs self #'(lambda(x)
                                  (position x group-vals :test #'equal))
                         (length group-vals)
                         (get-viewed-obj-styles self :fill? (getf style-arglist :fill?) 
                                                :color (getf style-arglist :color))))
    
    
    
    (setf (subviews-of self)
          (loop for vos in sub-vos 
                for vo-style in sub-styles
                collect (apply #'view  :data vos :check-style? t
                               :orientation orientation
                               :drawing-style vo-style bars)))
    ) ; end let
  )




(defmethod init-position-subviews ((self barchart) &key)
  (let ((br (bounding-region-of self))
        (n (length (subviews-of self)))
        lo w hi)
    (when (subviews-of self)
    (if (eq (orientation-of self) :vertical)
      (setq lo (bottom-of br)
            hi (top-of br))
      (setq lo (left-of br)
            hi (right-of br)))
    (setq w (/ (- hi lo) n))
  (loop with reg
        with bounds = (or (break-points-of self)
                          (append (loop repeat n
                            for x from lo by w collect x) (list hi)))
        for bar in (subviews-of self)
        for b1 in bounds
        for b2 in (cdr bounds)
        for hgt in (bar-heights-of self) do
        (setq reg 
              (if (eq (orientation-of self) :vertical)
                (make-region 0.0 hgt b1 b2 )
                (make-region b1 b2 0.0 hgt)
                ))
        (place-subview self bar reg)) )))


(defmethod get-menu-items ((self barchart) (slot-name (eql 'middle-menu)))
  `(("-" nil)
     ("Scale" nil "" :sub-items 
     (("Frequency" (set-histogram-scale :scale :frequency))
      ("RelFrequency" (set-histogram-scale :scale :relative-frequency))
      ))))




