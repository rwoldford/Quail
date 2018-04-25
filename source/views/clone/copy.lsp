;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               copy.lisp
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
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(copy-view copy-view-fn *link-on-copy?* view-copy-list)))

(defgeneric view-copy-list (view)
  (:documentation "Returns an argument list to be passed to copies"))


(defmethod view-copy-list ((self view))
  (append (toplevel-copy-list self)
          (subview-copy-list self)))

(defmethod toplevel-copy-list ((self view))
  (toplevel-clone-list self))

(defmethod subview-copy-list ((self view))
  )

(defmethod toplevel-copy-list :around ((self view))
  (append
    (required-data-args-of self)
   (call-next-method)))


(defmethod subview-copy-list  ((self compound-view))
  (subview-clone-list self))


(defmethod toplevel-copy-list  ((self one-per-case-mixin))
  (append
   (list :case-views-from self :ordered-case-views? t)
   (call-next-method)))

(defmethod toplevel-copy-list  ((self pairs-layout))
  (append
   (list :cloud-subviews-from (car (subviews-of-type self 'one-per-case-mixin))
         :ordered-case-views? t)
   (call-next-method)))



(defmethod toplevel-copy-list ((self d-view ))
  (append
   (list 
    :dataset (dataset-of self)
     :cases (cases-of self)
    :coords (coords-cache-of self)
    :variates (variates-of  self)
    :value-fn (value-fn-of self))
   (call-next-method)))


(defmethod toplevel-copy-list ((self axis))
  (append (call-next-method)
          (multiple-value-bind (min max) (extent-of self)
    (list :tic-list (slot-value self 'tic-list)
          :min min
          :max max
          :pretty? nil
          ))))

(defmethod toplevel-copy-list  ((self histogram-view))
  (append (call-next-method)
  (list :break-points (break-points-of self))))

(defmethod toplevel-copy-list ((self label ))
  (append 
   (list :text (text-of self))
   (call-next-method)))

(defmethod toplevel-copy-list ((self rotating-cloud ))
  (append 
   (list :x-text (x-text-of self) :y-text (y-text-of self) :z-text (z-text-of self))
   (call-next-method)))


(defmethod required-data-args-of ((self 1d-view))
  (list :var (variate-of self)
        :function (func-of self)
        :transform (transform-of self)))

(defmethod required-data-args-of  ((self 2d-view))
  (list :x (x-variate-of self) 
          :y (y-variate-of self)
          :x-function (x-func-of self)
          :y-function (y-func-of self)
          :x-transform (x-transform-of self)
          :y-transform (y-transform-of self)))

(defmethod required-data-args-of   ((self 3d-view))
  (list   :x (x-variate-of self) 
          :y (y-variate-of self)
           :z (z-variate-of self)
           :x-function (x-func-of self)
          :y-function (y-func-of self)
          :z-function (z-func-of self)
          :x-transform (x-transform-of self)
          :y-transform (y-transform-of self)
          :z-transform (z-transform-of self)))


(defmethod required-data-args-of  ((self d-view))
  (list :vars (vars-of  self)
        :functions (functions-of self)
        :transforms (transforms-of self)))

(defmethod required-data-args-of  ((self view))
  )

(defmethod required-data-args-of :around ((self view))
  (let ((r (call-next-method)))
    (setf (getf r :data) (viewed-object-of self))
    r))




 

(defmethod toplevel-copy-list  ((self plot))
  (append (call-next-method)
  (list :initform-fn nil)))

(defmethod toplevel-copy-list  ((self view-layout))
  (append
   `(
     :bounding-region ,(copy-region (bounding-region-of self))
    :subview-locns ,(sub-view-locns-of self)
   )
   (call-next-method)))

(defmethod toplevel-copy-list  ((self table-layout)) 
  (append
   `( 
     :scales ,(scales-of self)
    )
   (call-next-method)))

(defmethod required-data-args-of  ((self pairs-layout))
  (loop 
    for row in (row-format self) 
    for (vi fi tri) = 
    (loop for (v) in row
          thereis (and (typep v '2d-view) 
                       (list (y-variate-of v) (y-func-of v)(y-transform-of v))))
    collect vi into v
    collect fi into f
    collect tri into tr
    finally (return (list :vars v :functions f :transforms tr))))

(defmethod required-data-args-of  ((self batch-layout))
  (list :batches nil))

(defmethod required-data-args-of  ((self batch-display-list))
  (list :batches (batches-of self) :by-vars (by-vars-of self))
  )


(defmethod toplevel-copy-list  ((self 1d-layout))
  (append (call-next-method)
  (list :subview-constructor #'default-layout-sub-views)))

(defmethod toplevel-copy-list  ((self xy-layout))
  (append (call-next-method)
  (list :subview-constructor #'default-layout-sub-views)))

(defmethod toplevel-copy-list  ((self grid-layout))
  (append
   `(:nrows ,(nrows-of self)
            :ncols ,(ncols-of self)
     )
   (call-next-method)))

(defmethod required-data-args-of  ((self 1d-layout))
  (list :vars nil ))

(defmethod required-data-args-of  ((self xy-layout))
  (list :x-vars nil :y-vars nil ))


(defmethod subview-copy-list  ((self view-layout)) 
  (flet ((get-copy-list(v)
           (if v (view-copy-list v))))
    `(:subviews
       ,(loop for sub in (layout-views-of self)
               collect (mapcar #'get-copy-list sub))
      )))


(defmethod subview-copy-list  ((self plot))
  (flet ((get-copy-list(v)
           (if v (view-copy-list v))))
    (let ((iv (interior-views-of self)) (ti (title-of self))
           (lv (left-views-of self)) (rv (right-views-of self))
           (bv (bottom-views-of self)) (tv (top-views-of self))
           (ll (left-labels-of self)) (rl (right-labels-of self))
           (bl (bottom-labels-of self)) (tl (top-labels-of self)))
        (loop for sub in (list ti iv lv rv bv tv ll rl bl tl)
             for key in (list :title :interior-view 
                              :left-view :right-view :bottom-view  :top-view
                              :left-label  :right-label :bottom-label :top-label )
             nconc (list key (if (listp sub) 
                                 (mapcar #'get-copy-list sub)
                                 (get-copy-list sub)))))))

(defmethod subview-copy-list  ((self grid-plot))
  (flet ((get-copy-list(v)
           (cond ((and v (listp v))
                  (loop for vi in v when vi collect (view-copy-list vi)))
                 (t 
           (if v (view-copy-list v))))))
          
    (let ((iv (interior-views-of self)) (ti (title-of self))
           (lv (left-views-of self))
            (rv (right-views-of self))
           (bv (bottom-views-of self))
           (tv (top-views-of self))
           (ll (left-labels-of self)) (rl (right-labels-of self))
           (bl (bottom-labels-of self)) (tl (top-labels-of self)))
       (loop for sub in (list ti iv lv rv bv tv ll rl bl tl)
             for key in (list :title :interior-view 
                              :left-view :right-view :bottom-view  :top-view
                              :left-label  :right-label :bottom-label :top-label )
             nconc (list key (if (listp sub) 
                                 (mapcar #'get-copy-list sub)
                                 (get-copy-list sub)))))))



  

(defun copy-view-fn (view &key (draw? nil supplied-p))
   (apply #'make-view-constructor-fn (class-name (class-of view))
          (if supplied-p
            (append (list :draw? draw?)
                 (view-copy-list view))
            (view-copy-list view))))

(defun copy-view (view &key (draw? nil) viewport)
  (setq viewport (or viewport (car (viewports-of view))))
  (let  ((new-instance 
          (funcall (copy-view-fn view :draw? nil))))
    (if draw? 
      (if viewport
        (let* ((l 10)
               (b 10)
               (w (make-view-window 
                   :title (view-window-title new-instance)
                   :region
                   (make-region l (+ l (width-of viewport) 15)
                                b (+ b (height-of viewport) 15))))) 
          (draw-view new-instance :viewport (make-viewport w)))
        (draw-view new-instance)))
    
    new-instance))
    
       
       
