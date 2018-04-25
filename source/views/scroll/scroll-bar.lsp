;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               scroll-bar.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(scroll-bar  slider-of scroll-views-to scroll-views )))

;;;----------------------------------------------------------------------------------

(defclass scroll-bar(control-mixin orientation-mixin compound-view) 
  ((target :initarg :scrolling-views
                    :accessor scrolling-views-of :initform nil)
   (slider :accessor slider-of :initform nil)
   (max-arrow :accessor max-arrow-of :initform nil)
   (min-arrow :accessor min-arrow-of :initform nil)
   
   (left-fn :initarg :scroll-fn :accessor scroll-fn-of :initform nil))
  (:default-initargs :min 0 :max 1 :step nil :orientation :vertical ))

(defmethod construct-sub-views  ((self scroll-bar) &rest arg &key  max min orientation )
  
  
  (let* ((s (apply #'needle-slider 
                   :needle-size  10
                   :level (if (eq orientation :vertical) max  min)
                   :left-fn #'scroll-views-to
                   :draw? nil
                   arg))
         (amax (view :type 'control-arrow
                     :direction (if (eq orientation :vertical) :up :right)
                          :left-fn #'scroll-views 
                          :target (list self :max :viewport )))

         (amin (view :type 'control-arrow :direction (if (eq orientation :vertical) :down :left)
                            :left-fn #'scroll-views 
                            :target (list self :min :viewport))))
    
    (setf (target-of s) `(,self (slider-level-of ,s) :viewport ))
    (setf (slider-of self) s)
    (setf (max-arrow-of self) amax)
    (setf (min-arrow-of self) amin)
    (setf (subviews-of self) (list s amax amin))))



(defmethod compute-sub-viewports ((self scroll-bar)
                                  &optional viewport subviews)
  (declare (ignore subviews))
  (loop with orientation = (orientation-of self)
        with arrow-wid = 15
        for i upfrom 0
        for sv in (list (slider-of self) (min-arrow-of self) (max-arrow-of self))
        when sv do
        (loop for vp in (if viewport (list viewport) (viewports-of self))
              for sv-vp = (or (select-viewport sv vp)
                              (make-viewport (window-of vp)))
              do
              (multiple-value-bind (l r b tp) (bounds-of vp)
                
                (setf (bounds-of sv-vp)
                      (if (eq orientation :vertical)
                        (case i
                          (0 (list l r  (+ b arrow-wid -1) (- tp arrow-wid -1)))
                          (1 (list l r b (+ b arrow-wid) ))
                          (2 (list l r  (- tp arrow-wid) tp)))
                        (case i
                          (0 (list (+ l arrow-wid -1) (- r arrow-wid -1) b tp))
                          (1 (list l (+ l arrow-wid) b tp ))
                          (2 (list  (- r arrow-wid) r b tp))))))
              
              (add-viewport sv sv-vp vp))))


(defmethod reshape-sub-viewports ((self scroll-bar) viewport  
                                  &key new-location transform )
  (declare (ignore new-location transform))
  (compute-sub-viewports self viewport))





(defmethod slider-level-to-display-start ((self scroll-bar) display &optional viewport)
  (setq viewport (or viewport (which-viewport self)))
  (let* ((slider (slider-of self))
         (axis (axis-of-orientation self))
         (parent-vp (select-parent-viewport self viewport))                 
         (prop  (/ (if (eq axis :y)
                     (- (max-level slider) (slider-level-of slider))
                     (- (slider-level-of slider) (min-level slider) ))
                   (- (max-level slider) (min-level slider))))
         (max-view (max-view-start display
                                   :axis axis 
                                   :viewport (select-viewport display parent-vp))))
    (round (* prop max-view) )))


(defmethod scroll-views-to((self scroll-bar) level &key viewport)
  (declare (ignore viewport))
  (let ((lf (scroll-fn-of self))
        (slider (slider-of self))
        (axis (axis-of-orientation self))
        (views (scrolling-views-of self) ))
    (if (eq level :max)
      (set-slider-level slider 
                        :level (+ (slider-level-of slider) (* 10 (slider-step-of slider))))
      (if (eq level :min)
        (set-slider-level slider 
                          :level (- (slider-level-of slider) (* 10 (slider-step-of slider))))))
    

  (when (and views lf)
      (loop for v in views 
            for level = (slider-level-to-display-start self v )
            do
            (apply lf v (if (eq axis :x) (list :x level)  (list :y level)))))))
                    

(defmethod scroll-views((self scroll-bar) direction &key viewport)
  
  (let ((lf (scroll-fn-of self))
        (slider (slider-of self))
        (views (scrolling-views-of self) )
        (axis (axis-of-orientation self))
        (win (window-of viewport))
        )
    (loop while (and (wb:mouse-down-p)
                     (contains-p viewport (view-position (wb:mouse-position win)))
                     )
                     do
          
          (if (eq direction :max)
            (set-slider-level slider 
                              :level (+ (slider-level-of slider) (* 10 (slider-step-of slider))))
            (if (eq direction :min)
              (set-slider-level slider 
                                :level (- (slider-level-of slider) (* 10 (slider-step-of slider))))))
          
          
          (when (and views lf)
            (loop for v in views 
                  for level = (slider-level-to-display-start self v ) do
                  (apply lf v (if (eq axis :x) (list :x level)  (list :y level))))))))


(defmethod legal-subview-p ((self scroll-bar) view)
  (declare (ignore view))
  nil)    
    
  
      


