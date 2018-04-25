;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               boxplot.lisp
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
(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(boxplot-view boxplot-box)))


;;;----------------------------------------------------------------------------------

(defclass boxplot-view (styles-cache 1d-view compound-view )
  ((bp-stats :initform nil :accessor bp-stats-of)
   (proportional? :initform T :initarg :proportional?
                  :accessor equal-areas-p))
  (:default-initargs :orientation :vertical
    :color *default-bar-color*
                     :symbol *default-point-symbol*
                     :size *default-point-size*
                     :fill? *default-bar-fill?*))
                     
(defmethod get-bp-stats ((self boxplot-view))
  (or (slot-value self 'bp-stats)
            (let* ((coords (sort (active-members self (plot-coords-of self)) #'<)))
          
          (setf (bp-stats-of self) 
                (bp-stats coords)))))


(defclass boxplot-box (bar)
  ()
  (:documentation "A kind of bar which handles highlighting as appropriate for ~
                   the boxes in a boxplot."))

(defmethod construct-sub-views ((self boxplot-view) &key size symbol 
                                fill? color case-view boxes median-view)
  
  (let ((style-arglist (append
                        boxes case-view
                        (list :color color :size size
                              :fill? fill? :symbol symbol)))
        (group-fn #'(lambda(c)
                      (let* ((bounds (get-bp-stats self))
                             (g (or (position c bounds :test #'<=) 0)))
                        (if (= c (elt bounds 0))
                          (setq g 1))
                        g)))
        (box-o (if (eq (orientation-of self) :horizontal) :vertical :horizontal))
        sub-styles sub-vos)
    (setq boxes (subview-arg-list boxes 'boxplot-box))
    (setq case-view (subview-arg-list case-view 'point-symbol))
    (setq boxes (append boxes (list :orientation box-o)))
    (setq median-view (append (subview-arg-list median-view 'point-symbol)
                              (list  :size (* 2 (or (getf style-arglist :size) *default-point-size*)))
                              style-arglist
                              )) 
    (multiple-value-setq ( sub-vos sub-styles)
      (group-and-sort-viewed-objs self group-fn 5
                                  (get-viewed-obj-styles self 
                                                         :fill? (getf style-arglist :fill?)
                                                         :color (getf style-arglist :color)
                                                         :symbol (getf style-arglist :symbol) 
                                                         :size (getf style-arglist :size)
                                                         )))
    (setf (subviews-of self)
          (append
           (loop 
             for vos in (cdr sub-vos )
             for vo-style in (cdr sub-styles)
             collect (apply #'view  :data vos :check-style? t
                            :viewed-elements vos
                            :drawing-style vo-style
                            boxes))
           (loop for vo in (car sub-vos) 
                 for vo-style in (car sub-styles)
                 collect (apply #'view 
                                :data vo :check-style? t
                                :drawing-style vo-style
                                case-view) )
           (list  (apply #'view 
                       :data nil 
                       :fill? t
                       median-view))
           ))
    
     ) ; end let
  ); end defmethod
                             


(defmethod new-sub-views :before ((self boxplot-view)  &key)
  
  (setf (bp-stats-of self) nil))







(defmethod max-count-per-unit((self boxplot-view))
  (let ((min-len
         (loop for b1 in (bp-stats-of self)
               for b2 in (cdr (bp-stats-of self))
               unless (zerop (- b2 b1))
               minimize (- b2 b1)))
        (box-count (/ (length (cases-of self)) 4)))
    (if (not (null min-len))
      (/ box-count min-len ))))


(defmethod proportional-bar-widths ((self boxplot-view) &optional mcpu)
  (when (null mcpu)
    (setq mcpu (max-count-per-unit self)))
  (let ((box-count  (/ (length (cases-of self)) 4)))
           (unless (or (zerop mcpu) (zerop box-count))
      (if (eq (orientation-of self) :horizontal)
        (loop with maxh = (* .8 (height-of (bounding-region-of self)))
              with fac = (/ (* box-count maxh) mcpu)
                for r in (subview-locns-of self)
              for b1 in (bp-stats-of self)
              for b2 in (cdr (bp-stats-of self))
              for barh = (/ fac (- b2 b1))
              unless (zerop barh) do
              (setf (height-of r) barh))
        (loop with maxw = (* .8 (width-of (bounding-region-of self)))
              with fac = (/ (* box-count maxw) mcpu)
               for r in (subview-locns-of self)
              for b1 in (bp-stats-of self)
              for b2 in (cdr (bp-stats-of self))
              for barw = (/ fac (- b2 b1))
              unless (zerop barw) do
              (setf (width-of r) barw))))))
                
(defmethod init-position-subviews ((self boxplot-view) &key)
  
  (let* ((svs (subviews-of self))
         (bars (subseq  svs 0 4))
         (points (subseq svs 4 (length svs)))
         (point-vos (loop for p in points
                          collect (viewed-object-of p)))
         (orientation (orientation-of self))
         (br (subview-position-region self))
         (proportional?  (equal-areas-p self))
         min  width 
         reg)
    
    (if (eq orientation :horizontal)
      (setq min (bottom-of br)
          
            width (height-of br)
             )
      (setq min (left-of br) 
             width (width-of br)
               ))
    
    
      (loop for bar in bars
            for b1 in (bp-stats-of self)
            for b2 in (cdr (bp-stats-of self))
            for i upfrom 0
            do
            (setq reg 
                  
                  (ecase orientation
                    (:horizontal
                     (if (or (= i 0) (= i 3))
                       (make-region b1 b2 (+ min (* 0.3 width)) (+ min (* 0.7 width)))
                       (make-region b1 b2 (+ min (* 0.1 width)) (+ min (* 0.9 width)))))
                    (:vertical
                     (if (or (= i 0) (= i 3))
                       (make-region (+ min (* 0.3 width)) (+ min (* 0.7 width)) b1 b2 )
                       (make-region (+ min (* 0.1 width)) (+ min (* 0.9 width)) b1 b2))))
                  )
            (place-subview self bar reg))
      (if proportional? (proportional-bar-widths self))
    
    (loop with w = (* width .05)
          for p in (butlast points)
          for pc in   (plot-coords-of self :cases point-vos)
          do
          (setq reg 
                (ecase orientation
                  (:horizontal  (make-region (- pc w) (+ pc w) 
                                             (+ min (* 0.3 width))  (+ min (* 0.7 width))))
                  (:vertical (make-region (+ min (* 0.3 width))  (+ min (* 0.7 width)) 
                                          (- pc w) (+ pc w) ))))
          
          (place-subview self p reg)
          finally (let ((m (third (bp-stats-of self))))
                    (place-subview self (car (last points))  
                                   (ecase orientation
                                     (:horizontal  (make-region (- m w) (+ m w) 
                                                                (+ min (* 0.3 width))  (+ min (* 0.7 width))))
                                     (:vertical (make-region (+ min (* 0.3 width))  (+ min (* 0.7 width)) 
                                                             (- m w) (+ m w) ))))
                    ))))
                
#|  
(defmethod init-position-subviews ((self boxplot-view) &key)
  
  (let* ((svs (subviews-of self))
         (bars (subseq  svs 0 4))
         (points (subseq svs 4 (length svs)))
         (point-vos (loop for p in points
                          collect (viewed-object-of p)))
         (orientation (orientation-of self))
         (br (subview-position-region self))
         (proportional?  (equal-areas-p self))
         min max width length
         reg)
    
    (if (eq orientation :horizontal)
      (setq min (bottom-of br)
            max (top-of br)
            width (height-of br)
            length (width-of br)
            )
      (setq min (left-of br) 
            max (right-of br)
            width (width-of br)
            length (height-of br)
            ))
    
    (if proportional?
      (let* ((min-bar-length
              (loop for b1 in (bp-stats-of self)
                    for b2 in (cdr (bp-stats-of self))
                    unless (zerop (- b2 b1))
                    minimize (- b2 b1)))
             (max-bar-width (* 0.8 width))
             (bar-display-area
              (* (/ min-bar-length length)
                 (/ max-bar-width width)))
             )
        (loop for bar in bars
              for b1 in (bp-stats-of self)
              for b2 in (cdr (bp-stats-of self))
              for i upfrom 0
              do
              (let ((bar-length (- b2 b1))
                    margin
                    bar-width)
                (if (zerop bar-length)
                  (setf bar-width width)
                  (setf bar-width (* bar-display-area width (/ length bar-length)))
                  )
                (setf margin (/  (- width bar-width) 2))
                (setf
                 reg 
                 (ecase orientation
                   (:horizontal
                    (make-region b1 b2 (+ min margin) (- max margin)))
                   (:vertical
                    (make-region (+ min margin) (- max margin) b1 b2)))
                 )
                (place-subview self bar reg)))
        )
      
      (loop for bar in bars
            for b1 in (bp-stats-of self)
            for b2 in (cdr (bp-stats-of self))
            for i upfrom 0
            do
            (setq reg 
                  
                  (ecase orientation
                    (:horizontal
                     (if (or (= i 0) (= i 3))
                       (make-region b1 b2 (+ min (* 0.3 width)) (+ min (* 0.7 width)))
                       (make-region b1 b2 (+ min (* 0.1 width)) (+ min (* 0.9 width)))))
                    (:vertical
                     (if (or (= i 0) (= i 3))
                       (make-region (+ min (* 0.3 width)) (+ min (* 0.7 width)) b1 b2 )
                       (make-region (+ min (* 0.1 width)) (+ min (* 0.9 width)) b1 b2))))
                  )
            (place-subview self bar reg))
      )
    
    (loop with w = (* width .05)
          for p in (butlast points)
          for pc in   (plot-coords-of self :cases point-vos)
          do
          (setq reg 
                (ecase orientation
                  (:horizontal  (make-region (- pc w) (+ pc w) 
                                             (+ min (* 0.3 width))  (+ min (* 0.7 width))))
                  (:vertical (make-region (+ min (* 0.3 width))  (+ min (* 0.7 width)) 
                                          (- pc w) (+ pc w) ))))
          
          (place-subview self p reg)
          finally (let ((m (third (bp-stats-of self))))
                    (place-subview self (car (last points))  
                                   (ecase orientation
                                     (:horizontal  (make-region (- m w) (+ m w) 
                                                                (+ min (* 0.3 width))  (+ min (* 0.7 width))))
                                     (:vertical (make-region (+ min (* 0.3 width))  (+ min (* 0.7 width)) 
                                                             (- m w) (+ m w) ))))
                    ))))
    


|#

(defmethod highlight-view ((self boxplot-box) &key viewport )
  "Highlight a proportion of each color-fill combintation ~
   by examining drawing styles. "
  (with-exposed-viewports self viewport vp
    (multiple-value-bind (l r b tp) (bounds-of vp)
       (let ((op  (highlight-operation self))
            (axis (orientation-of self))
            (new-r (copy-region vp))
            (w (window-of vp))
            (highlight-proportion 0))
        (setf highlight-proportion
            (or  (loop with styles = (style-proportions self '(:highlight?))
                    
                    for (style)  in (first styles)
                    for prop in (second styles)
                    thereis (and style prop))
                 0)
              )
        (unless (zerop highlight-proportion)
        (if (eql axis :horizontal)
          (scale-sides-by new-r 1 highlight-proportion)
          (scale-sides-by new-r highlight-proportion 1))
           (wb:canvas-highlight-rectangle w (max l (left-of new-r))
                                          (min r (right-of new-r))
                                          (max b (bottom-of new-r))
                                          (min tp (top-of new-r))))
                                         :color *default-highlight-color*
                                         :operation op)
        
        )))







        



(defmethod use-x-axis-p ((self boxplot-view))
  t)

(defmethod use-y-axis-p ((self boxplot-view))
  t)



(defmethod show-x-axis-p ((self boxplot-view))
  (eql (orientation-of self) :horizontal))

(defmethod show-y-axis-p ((self boxplot-view))
  (eql (orientation-of self) :vertical))
