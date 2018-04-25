;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               moving-cloud-mixin.lisp
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


(eval-when (:compile-toplevel :load-toplevel :execute) (export '( moving-dview-mixin moving-cloud-mixin
            viewport-locns-of set-viewport-locns
           scaled-coords-of  compute-plot-coords common-coords-scale
          compute-scaled-coords )))
          
(defclass moving-dview-mixin ()
  ( (coords-scale-method
     :initarg :coords-scale-method
    :initform #'default-coords-scale
    :accessor coords-scale-method-of)
    (scaled-coords-cache 
    :initform nil 
    :accessor scaled-coords-cache-of)
    (plot-coords
    :initform nil 
    :accessor plot-coords-cache-of)
    
    (moving?
    :initform nil 
    :accessor moving-p))
  
  )          
          

(defclass moving-cloud-mixin (moving-dview-mixin 3d-one-per-case)
  ((draw-rate  :initform *default-draw-rate*
               :initarg :draw-rate 
               :accessor draw-rate-of) )
  (:default-initargs :size *default-point-size* :color *default-point-color*
    :case-view 'point-symbol))


(defgeneric compute-scaled-coords (moving-dview-mixin &key cases )
  (:documentation "computes and returns standardized coords of cases"))

(defgeneric scaled-coords-of (moving-dview-mixin &key cases)
  (:documentation "returns standardized coords of cases"))


(defgeneric compute-plot-coords (moving-dview-mixin )
  (:documentation "computes and returns plot coords "))




(defgeneric centered-viewport-coords-of (moving-dview-mixin 
                            viewport)
  (:documentation "returns coords for the cases scaled to the viewport, ~
                   but centered at 0,0 ."))
  


(defgeneric move-points (moving-dview-mixin &key viewport &allow-other-keys)
  (:documentation "moves the coordinates in viewport"))
;;------------------------------------------------------------------------------------------
(defgeneric viewport-locns-of (cloud  viewport)
  (:documentation "returns a list containing the locations of subviews in viewport coords"))
                   

(defgeneric set-viewport-locns (cloud &key viewport coords)
  (:documentation "sets subview viewport locations to coords"))

;;------------------------------------------------------------------------------------------
(defun default-coords-scale ( coords status)
  
  "standardizes coords to have zero mean and common variance"
  
  (let* (
         (n (loop for s in status count (active-status-p s)))
         (index (loop for i from 0 below (length (car coords)) collect i)))
    
    (flet ((mean(i)
             (/  (loop for dj in coords 
                       for s in status
                       for dij = (elt dj i) 
                       when (active-status-p s) 
                       sum dij ) n))
           (sd (i m)
             (sqrt (/  (loop for dj in coords 
                             for s in status
                             for dij = (elt dj i)
                             when (active-status-p s) 
                             sum (expt (- dij m) 2))
                       n))))
        (cond ( (> n 1)   
        (loop  with means = (mapcar #'mean index)
               with sds = (mapcar #'sd index means)
               for d in coords 
               for s in status 
               collect
               (unless (invalid-status-p s)
                 (loop for di in d
                       for m in means
                       for sd in sds 
                       collect 
                       (round (* most-positive-fixnum 
                                 (/ (- di m) (* 3 sd))))))))
            ((= n 1)  (list (list 0 0 0)))
            (t nil)))))

(defun common-coords-scale ( coords status)
  
  "standardizes coords to have zero mean and common variance, using the ~
   same scale factor in all directions. "
  
  (let* (
         (n (loop for s in status count (active-status-p s)))
         (index (loop for i from 0 below (length (car coords)) collect i)))
    
    (flet ((mean(i)
             (/  (loop for dj in coords 
                       for s in status
                       for dij = (elt dj i) 
                       when (active-status-p s) 
                       sum dij ) n))
           (sd (i m)
             (sqrt (/  (loop for dj in coords 
                             for s in status
                             for dij = (elt dj i)
                             when (active-status-p s) 
                             sum (expt (- dij m) 2))
                       n))))
      
      (cond ((> n 1)   
        (loop  with means = (mapcar #'mean index)
               with sd = (reduce  #'max (mapcar #'sd index means))
               for d in coords 
               for s in status 
               collect
               (unless (invalid-status-p s)
                 (loop for di in d
                       for m in means
                       collect 
                       (round (* most-positive-fixnum 
                                 (/ (- di m) (* 3 sd))))))))
            ((= n 1)  (list (list 0 0 0)))
            (t nil)))))


(defmethod compute-scaled-coords ((self moving-dview-mixin) 
                                  &key (cases (cases-of self)))
  
  
  (let* ((coords (coords-of self :cases cases))
         (status
          (if  (eq cases (cases-of self))
            (case-status-of self)
            (loop for c in cases collect (select-case-status self c))))
         (scale-method (coords-scale-method-of self)))
    (if scale-method
      (funcall scale-method coords status)
      coords)))


(defmethod scaled-coords-of ((self moving-dview-mixin) 
                             &key (cases (cases-of self)))
  (if (eq cases (cases-of self))
    (or (scaled-coords-cache-of self)
        (setf (scaled-coords-cache-of self) 
              (compute-scaled-coords self :cases cases)))
    (compute-scaled-coords self :cases cases)))


(defmethod compute-plot-coords ((self moving-dview-mixin) )
  ;; computes the plot coords of self 
  
   (coords-of self))


(defmethod plot-coords-of ((self moving-dview-mixin) 
                           &key (cases (cases-of self)))
  (let ((coords (plot-coords-cache-of self)))
    (unless coords
      (setq coords (compute-plot-coords self))
      (setf (plot-coords-cache-of self) coords))
    (if (eq cases (cases-of self))
      coords
      (loop with viewed-object = (cases-of self)
            for c in cases
            for p = (position c viewed-object)
            collect (if p (nth p coords))))))






(defmethod centered-viewport-coords-of ((self moving-dview-mixin) 
                            viewport)
  (scale-data-for-viewport self
                           (plot-coords-of self)
                           viewport))




(defmethod subview-styles ((self moving-cloud-mixin)  style &optional default)
  (loop for sv in (subviews-of self)
        collect (draw-style (drawing-style-of sv) style :default default)))






(defmethod draw-view :around ((self moving-dview-mixin)
                      &key viewport)
  ;; necessary because window system may cause extra draws!
  (declare (ignore viewport))
  (unless (moving-p self)  (call-next-method)))


(defmethod new-variable  :before ((self moving-dview-mixin) &key )
    (setf (scaled-coords-cache-of self) nil)
    (setf (plot-coords-cache-of self) nil)
    )


(defmethod new-case-status :before  ((self moving-dview-mixin) 
                                        cases status &key rescale?)
  (declare (ignore cases status)) 
  (when rescale? 
    (setf (scaled-coords-cache-of self) nil)
    (setf (plot-coords-cache-of self) nil)
    ))



;;------------------------------------------------------------------------------------------
(defmethod new-case-status :after  ((self moving-cloud-mixin) 
                                        cases status &key rescale?)
  (declare (ignore cases status)) 
  (when rescale? 
    (init-position-subviews self)))


(defmethod viewport-locns-of ((self moving-cloud-mixin) viewport)
  (setq viewport (or viewport (car (viewports-of self))))
  (loop with c
        for sv in (subviews-of self)
        for s in (case-status-of self)
        collect (when (active-status-p s)
                  (setq c (centre-of (select-viewport sv viewport)))
                  (list (2d-position-x c) (2d-position-y c)))))


(defmethod set-viewport-locns ((self moving-cloud-mixin) &key viewport  coords)
  (if (and viewport coords)
    (loop for sv in (subviews-of self)
          for vp-sv = (select-viewport sv viewport)
          for (x y) in coords
          do
          (set-square-viewport-center vp-sv x y))
  (compute-sub-viewports self viewport)))


(defmethod adjust-subviews ((self moving-cloud-mixin) 
                            &optional (new-coords (plot-coords-of self) ))

    (loop for (x y )  in new-coords
          for l in (sub-view-locns-of self)
          for s in (case-status-of self)
             unless (invalid-status-p s)
          do
          (set-square-region-center l x y)))


(defmethod smallest-bounding-region ((self moving-cloud-mixin) )
  (let ((br (make-region)))
    (if (cases-of self)
      (loop  for c in  (scaled-coords-of self )
             for s in (case-status-of self)
             when (active-status-p s)
             maximize (loop for ci in c
                            maximize (abs ci) )
             into radius
             finally 
             (if (zerop radius) (incf radius 1))
             (setf (bounds-of br) 
                   (list (- radius) radius 
                         (- radius) radius))))
    br))





(defmethod scale-data-for-viewport ((self moving-cloud-mixin) data viewport)
  "scales  data so that when rotated and shifted, points in bounding region will fit in a window~
   with minimum dimension SIZE ~
   Data should already be centered at 0"
  (let* ((region (get-map-portion self viewport))
         (ndims (ndims self))
         (size (min (width-of region) (height-of region)))
          (d (/ size (radius-of (bounding-region-of self)) 2)))
    (loop for di in data collect
       (if di   
         (loop for dij in di collect (round (* dij d)))
           (make-list ndims :initial-element 0)))))
