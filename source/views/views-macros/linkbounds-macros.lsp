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
  (let  ((i (gensym "i"))
        (hist-list (gensym "hist")))
    `(let* ((,i )
            (,hist-list 
            (loop 
                  for v in ,views 
                  when (and (typep v 'histogram-view )
                            (eq ,axis (axis-of-orientation v)))
                  collect (list v))))
              
       (unless (null ,hist-list)
         (loop for v in ,views
               when (and (typep v 'axis )
                         (eq ,axis (axis-of-orientation v))
                         (setq ,i (position (tic-list-of v) ,hist-list
                                            :key #'(lambda(d) (break-points-of (car d)))
                                            :test #'equal))) do
               (push v (cdr (elt ,hist-list ,i)))))

        ,@do-form

       (unless (null ,hist-list)
         (loop for d in ,hist-list do
               (mapcar #'(lambda(v) 
                          (setf (tic-list-of v) (break-points-of (car d))))  
                                          (cdr d)))))))

        
(defmacro with-linked-extent (view axis  &body do-form)
  (let ( 
        (old-brs (gensym  "old-brs"))
        (views (gensym  "views"))
        (v (gensym  "v")) (br (gensym  "br")) ;(d (gensym  "d")) 
        (vs (gensym  "vs")) ;(ds (gensym  "ds")) 
        (mod-views (gensym  "mod-views"))
        
        )
    `(let* ((,mod-views)
             (,views (if (listp ,view) ,view
                        (if (eq ,axis :x)
                     (link-bounds-x-of ,view)
                     (if (eq ,axis :y)
                       (link-bounds-y-of ,view)))))
            (,old-brs (mapcar #'bounding-region-of ,views))
           )
       (with-tics=breaks ,views ,axis
         ,@do-form
         
         (loop for ,v in ,views 
               for ,br in ,old-brs 
                
               when (or (not (eq ,br (bounding-region-of ,v)))
                        (and (typep ,v 'd-view) (has-variates-p ,v)))
               collect ,v into ,vs
                finally (setq ,mod-views ,vs)))

       

       (if (eq ,axis :x)
         (set-view-extents ,views :x :region (maximize-x-extents ,mod-views ))
         (if (eq ,axis :y)
           (set-view-extents ,views :y :region (maximize-y-extents ,mod-views ))))

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
  (let ( (views (gensym)))
    `(let* ((,views (if (listp ,view) ,view
                        (if (eq ,axis :x)
                     (link-bounds-x-of ,view)
                     (if (eq ,axis :y)
                       (link-bounds-y-of ,view))))))

       (when ,draw? 
        (loop for v in ,views do (erase-view v)))
       
       (with-linked-extent ,view ,axis  ,@do-form)
      
       (loop for v in ,views 
             do (remap-to-viewports v :erase? nil :draw? ,draw?)))))



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