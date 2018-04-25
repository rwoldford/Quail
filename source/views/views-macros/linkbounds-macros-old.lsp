;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               linkbounds-macros.lisp
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
;;;     C.B. Hurley 1988-1992  George Washington University
;;;     R.W. Oldford 1988-1991
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)


(defmacro with-tics=breaks (views axis &body do-form)
  (let ((v (gensym))  (d (gensym)) (i (gensym))
        (hist-list (gensym)))
    `(let ((,hist-list 
            (loop 
                  for ,v in ,views 
                  when (and (typep ,v 'histogram )
                            (eq ,axis (axis-of-orientation ,v)))
                  collect (list ,v))))
              
       (unless (null ,hist-list)
         (loop for ,v in ,views
               when (and (typep ,v 'axis )
                         (eq ,axis (axis-of-orientation ,v))
                         (setq ,i (position (tic-list-of ,v) ,hist-list
                                            :key #'(lambda(,d) (break-points-of (car ,d)))
                                            :test #'equal))) do
               (push ,v (cdr (elt ,hist-list ,i)))))

        ,@do-form

       (unless (null ,hist-list)
         (loop for ,d in ,hist-list do
               (mapcar #'(lambda(,v) 
                          (setf (tic-list-of ,v) (break-points-of (car ,d))))  
                                          (cdr ,d)))))))

        
(defmacro with-linked-extent (view axis  &body do-form)
  (let ((delta-regions (gensym)) 
        (old-brs (gensym))
        (views (gensym))
        (v (gensym)) (br (gensym)) (d (gensym)) 
        (vs (gensym)) (ds (gensym)) 
        (mod-views (gensym))
        (mod-deltas (gensym))
        )
    `(let* ((,views (if (listp ,view) ,view
                        (if (eq ,axis :x)
                     (link-bounds-x-of ,view)
                     (if (eq ,axis :y)
                       (link-bounds-y-of ,view)))))
            (,old-brs (mapcar #'bounding-region-of ,views))
           (,delta-regions (compute-region-shifts ,views)))
       (with-tics=breaks ,views ,axis
         ,@do-form
         
         (loop for ,v in ,views 
               for ,br in ,old-brs 
               for ,d in ,delta-regions 
               
               when (or (not (eq ,br (bounding-region-of ,v)))
                        (and (typep ,v 'd-view) (has-variates-p ,v)))
               collect ,v into ,vs
               collect ,d into ,ds
               finally (setq ,mod-views ,vs ,mod-deltas ,ds)))

       

       (if (eq ,axis :x)
         (set-view-extents ,views :x (maximize-x-extents ,mod-views ,mod-deltas)
                           ,delta-regions )
         (if (eq ,axis :y)
           (set-view-extents ,views :y (maximize-y-extents ,mod-views ,mod-deltas)
                             ,delta-regions )))

       (loop for ,v in ,views
             when (and (typep ,v 'axis)
                        (not (member ,v ,mod-views)))
             do
             (set-bounding-region ,v :region (bounding-region-of ,v)
                                     :pretty? nil
                                     :ignore-x? (eq ,axis :y)
                                     :ignore-y? (eq ,axis :x)))
       )))



(defmacro with-constrained-extent (view axis draw? &body do-form)
  (let ( (views (gensym)) (v (gensym)))
    `(let* ((,views (if (listp ,view) ,view
                        (if (eq ,axis :x)
                     (link-bounds-x-of ,view)
                     (if (eq ,axis :y)
                       (link-bounds-y-of ,view))))))

       (when ,draw? 
        (loop for ,v in ,views do (erase-view ,v)))
       
       (with-linked-extent ,view ,axis  ,@do-form)
      
       (loop for ,v in ,views 
             do (remap-to-viewports ,v :erase? nil :draw? ,draw?)))))



(defmacro with-constrained-extents (view  draw? &body do-form)
  (let ( (views (gensym))
        (v (gensym)) )
    `(let* ((,views (if (listp ,view) ,view
                        (link-bounds-of ,view))))

       (when ,draw? 
        (loop for ,v in ,views do (erase-view ,v)))
       (with-linked-extent ,view :x 
         (with-linked-extent ,view :y 
           ,@do-form))
      
       (loop for ,v in ,views 
             do (remap-to-viewports ,v :erase? nil :draw? ,draw?)))))