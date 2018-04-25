;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               lines.lisp
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
;;;     R.W. Oldford 1988-1991
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------

(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(lines )))

(defclass lines (brushable-view-mixin pass-draws-to-subviews 2d-view compound-view )
  ((middle-menu :allocation :class :initform nil)
   (order-fn :initform #'< :initarg :order-fn
              :accessor order-fn-of))
  (:default-initargs :dashing nil :width 1 :color *default-curve-color*)
                     
  (:documentation "Draws line segments objects connecting coords ordered by order-fn"))





(defmethod construct-sub-views ((self lines)
                                &key dashing width color segments)
  (setq segments (subview-arg-list segments 'line-segment))
  (let ((coords (active-members self (plot-coords-of self))  )
        (fn (order-fn-of self))
        (vo (active-members self (cases-of self)))
        
        coords-vo)
    (when fn
      (setq coords-vo (sort (mapcar #'list coords vo)
                            fn :key #'caar))
      (setq coords (mapcar #'car coords-vo))
      (setq vo (mapcar #'second coords-vo))
      )

     (setf (subviews-of self)
          (loop for case1 in vo for case2 in (cdr vo)
                for start in coords for end in (cdr coords)
                for xmin = (min (car start) (car end))
                for xmax = (max (car start) (car end))
                for ymin = (min (cadr start) (cadr end))
                for ymax = (max (cadr start) (cadr end)) 
                for sub-vo = (list case1 case2)
                
                collect
                (apply #'view  :data sub-vo
                       :viewed-elements sub-vo
                                   :endpoints (list start end)
                                   :bounding-region (make-region xmin xmax ymin ymax)
                                   :menu? nil :linkable? t
                                   :clip-draw? nil
                                   :color color :width width :dashing dashing
                                   segments
                                   )))))
  




(defmethod init-position-subviews ((self lines) &key )
  
  (loop 
    for sv in (subviews-of self)
    do
    (place-subview self sv (bounding-region-of sv))))
                   
                            

(defmethod distance-to-location ((self lines) viewport location)

  (if (selected-p self viewport location)
    (let ((locn-c (if (region-p location)
                  (centre-of location) location)))
      (loop for sv in (subviews-of self)
            for sv-vp = (select-viewport sv viewport)
            minimize (distance-to-location sv sv-vp locn-c)))

 10000))


(defmethod style-menu-items ((self lines) )
  (line-default-middle-items))

(defmethod initialize-instance :before  ((self lines) &key  simple?)
  (when (and simple? (eq (class-name (class-of self)) 'lines))
    (change-class self 'simple-lines)
    (setf (viewport-compute-method-of self) #'compute-lines-coords-for-viewport)))
