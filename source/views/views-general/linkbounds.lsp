;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               linkbounds.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(linkable-bounds-mixin 
           link-bounds-x-of link-bounds-y-of link-bounds-of 
           compute-relative-transforms
           maximize-x-extents maximize-y-extents set-view-extents
           link-view-bounds unlink-view-bounds)))
;;;----------------------------------------------------------------------------------

;; For constraints between the bounding-regions of views.


;; Views can be constrained to have the same x,y (or both) extent,
;; More generally, an extent may be constrained to another by a 
;; (linear) transformation). 

(defclass linkable-bounds-mixin () 
  ((link-bounds  :initform (list nil nil ))))

(defmethod initialize-instance :after ((self linkable-bounds-mixin) &key
                                       (linkable-bounds-x? t)
                                       (linkable-bounds-y? t))
  (unless (eq t linkable-bounds-x?)
    (setf (first (slot-value self 'link-bounds)) :no-link))
  (unless (eq t linkable-bounds-y?)
    (setf (second (slot-value self 'link-bounds)) :no-link)))

(defmethod linkable-bounds-x-p ((self linkable-bounds-mixin ))
  (if (listp (first (slot-value self 'link-bounds)))
    self))

(defmethod linkable-bounds-y-p ((self linkable-bounds-mixin ))
  (if (listp (second (slot-value self 'link-bounds)))
      self))

(defmethod link-bounds-x-of (( self linkable-bounds-mixin ))
  (cons self 
        (if (linkable-bounds-x-p self)
          (remove self (first (slot-value self 'link-bounds))))))
        

(defmethod link-bounds-y-of (( self linkable-bounds-mixin ))
  (cons self 
        (if (linkable-bounds-y-p self)
          (remove self (second (slot-value self 'link-bounds))))))

(defmethod link-bounds-of (( self linkable-bounds-mixin ))
  (cons self 
        (remove self (union  (link-bounds-x-of self)
                             (link-bounds-y-of self)))))
(defmethod remove-y-link (( self linkable-bounds-mixin ) v)
  (if (linkable-bounds-y-p self)
    (setf (second (slot-value self 'link-bounds)) 
          (remove v (second (slot-value self 'link-bounds))))))

(defmethod remove-x-link (( self linkable-bounds-mixin ) v)
  (if (linkable-bounds-x-p self)
    (setf (first (slot-value self 'link-bounds)) 
          (remove v (first (slot-value self 'link-bounds)))))) 


(defmethod link-bounds-x-of((view-list list))
  (remove-duplicates
   (loop for v in view-list when v
         nconc (link-bounds-x-of v))))

(defmethod link-bounds-y-of((view-list list))
  (remove-duplicates
   (loop for v in view-list when v
         nconc (link-bounds-y-of v))))

(defmethod (setf link-bounds-x-of) (new-list ( self linkable-bounds-mixin ))
  (if (linkable-bounds-x-p self)
    (setf (first (slot-value self 'link-bounds)) new-list)))

(defmethod (setf link-bounds-y-of) (new-list ( self linkable-bounds-mixin ))
  (if (linkable-bounds-y-p self)
  (setf (second (slot-value self 'link-bounds)) new-list)))







(defun link-view-bounds(views axis)
  
  (if (eq axis :x)
    (loop for v in views do
          (setf (link-bounds-x-of v) views))
    (loop for v in views do
          (setf (link-bounds-y-of v) views))))

(defun unlink-view-bounds(views v axis)
  "Unlink v from views."
  
  (let (new-links )
    (if (eq axis :x)
      (setf new-links (remove v (link-bounds-x-of (car views)))
            (link-bounds-x-of v) nil)
      (setf new-links (remove v (link-bounds-y-of (car views)))
            (link-bounds-y-of v) nil))
    (link-view-bounds new-links axis)))
    


#|

(defun region+region (r1 r2)
  (cond ((and (region-p r1) (region-p r2))
         (make-region (+ (left-of r2) (left-of r1))
                      (+ (right-of r2) (right-of r1))
                      (+ (bottom-of r2) (bottom-of r1))
                      (+ (top-of r2) (top-of r1))))
        ((region-p r1) r1)
        ((region-p r2) r2)
        (t nil)))

(defun region-region (r1 r2)
  (cond ((and (region-p r1) (region-p r2))
         (make-region (- (left-of r1) (left-of r2))
                      (- (right-of r1) (right-of r2))
                      (- (bottom-of r1) (bottom-of r2))
                      (- (top-of r1) (top-of r2))))
        ((region-p r1) r1)
        ((region-p r2) r2)
        (t nil)))

 |#




  
(defun maximize-x-extents (views)
  "Computes the region maximizing the x-extent of views."
  (loop for v in views
            for br = (bounding-region-of v)
            maximize (right-of br) into right
            minimize (left-of br) into left
            finally (loop with b for v in views
                          when (typep v 'one-per-case-mixin) do
                          (setq b (adjusted-bounding-region v))
                          
                          (if (or (not (typep v 'orientation-mixin)) (eq (orientation-of v) :horizontal))
                              (setq left (min left (left-of b))
                                    right (max right (right-of b)))))
            (return (make-region left right 0 1))))

(defun maximize-y-extents (views)
  "Computes the region maximizing the y-extent of views."
  (loop for v in views
            for br = (bounding-region-of v)
            maximize (top-of br) into top
            minimize (bottom-of br) into bottom
            finally (loop with b for v in views
                          when (typep v 'one-per-case-mixin) do
                          (setq b (adjusted-bounding-region v))
                          
                          (if (or (not (typep v 'orientation-mixin)) (eq (orientation-of v) :vertical))
                              (setq bottom (min bottom (bottom-of b))
                                    top (max top (top-of b)))))
            (return (make-region 0 1 bottom top))))




(defun set-view-extents (views axis &key region  (recompute? t))
  "Gives all views the same  extent along axis-- from region.~
   If region is not present, maximize the extents of views."
  
 
  (let ((new-region nil)
          (new-extent (if (eq axis :x)  
                        #'(lambda (v new-br) 
                            (setf (x-extent-of (bounding-region-of v)) new-br)
                            (if recompute? (if (typep v 'line) 
                              (compute-line-endpoints v)
                              (if (and (typep v 'axis) 
                                       (eql (orientation-of v) :horizontal))
                                (let ((old (internal-tics-p v)))
                                  (setf (internal-tics-p v) t)
                                  (get-tic-interval v)
                                  (setf (internal-tics-p v) old))))))

                       #'(lambda (v new-br) 
                            (setf (y-extent-of (bounding-region-of v)) new-br)
                             (if recompute? (if (typep v 'line) 
                              (compute-line-endpoints v)
                              (if (and (typep v 'axis) 
                                       (eql (orientation-of v) :vertical))
                                (let ((old (internal-tics-p v)))
                                  (setf (internal-tics-p v) t)
                                  (get-tic-interval v)
                                  (setf (internal-tics-p v) old)))))))))
      

      (setq new-region
        (or region (if (eq axis :x)  
                       (maximize-x-extents views)
                       (maximize-y-extents views))))
     
      (loop for v in views 
            for i upfrom 0
            for newb = new-region do
            (funcall new-extent v newb))))




(defmethod change-bounding-region ((self linkable-bounds-mixin) region 
                      &key ignore-x? ignore-y? (pretty? t) (draw? t) )
  "Sets bounding-region of self to region ~
   and redraws everywhere.
  If ignore-x? is non-nil the x bounds view remain unchanged.~
   similarly ignore-y?. ~
  Views with linked bounds are also modified."
  
  (unless (and ignore-x? ignore-y? )
    (let* ((x-views (link-bounds-x-of self))
           (y-views (link-bounds-y-of self))
           (redraw-views nil) )
      (setq redraw-views (union (unless ignore-x? x-views)
                                (unless ignore-y? y-views)))
      (if draw? (loop for v in redraw-views do
            (erase-view v )))
      
      (unless ignore-x?
        (change-bounding-region-views x-views region 
                           :ignore-y? t 
                          :pretty? pretty? :draw? nil))
      
      (unless ignore-y?
        (change-bounding-region-views y-views region 
                           :ignore-x? t 
                          :pretty? pretty? :draw? nil))
      (if draw?
        (loop for v in redraw-views do
            (remap-to-viewports v :erase? nil))))))


(defun set-bounding-regions (views region 
                                   &key 
                                   ignore-x? ignore-y? (pretty? t) )
  "Sets bounding-region of views to region.~ 
  If ignore-x? is non-nil the x bounds view remain unchanged ~
  similarly ignore-y?.~
  A non-nil pretty? allows regions to be prettied."

  
  

  
  (loop for v in views 
        do
         (set-bounding-region v :region region
                             :ignore-x? ignore-x?
                             :ignore-y? ignore-y?
                             :pretty? pretty?)))

(defun change-bounding-region-views (views region 
                              &key  ignore-x? ignore-y? (pretty? t) (draw? t))
  "Sets bounding-region of views to region and redraws views.~
   If ignore-x? is non-nil the x bounds view remain unchanged,~
   similarly ignore-y?.~
   A non-nil pretty? allows regions to be prettied."
  
  
  (unless (and ignore-x? ignore-y? )
    (if draw? (loop for v in views do
                    (erase-view v )))
    
    (set-bounding-regions views region 
                          :ignore-x? ignore-x?
                          :ignore-y? ignore-y?
                          :pretty? pretty?)
    
    (when pretty? 
      (unless ignore-x?
        (set-view-extents views :x))
      (unless ignore-y?
        (set-view-extents views :y )))
    
    (if draw? (loop for v in views do
                    (remap-to-viewports v :erase? nil)))))





















