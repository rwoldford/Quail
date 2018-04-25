;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               change-cases.lisp
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

;;;----------------------------------------------------------------------------------
(defmethod case-links-of ((self d-view))
  (if (link-cases-p self)
    (loop for v in (link-bounds-of self)
          when (and (typep v 'd-view)
                    (link-cases-p v))
          collect v)
    (list self)))

(defmethod case-links-of ((self 1d-view))
  (if (link-cases-p self)
    (loop for v in (if (eq (orientation-of self ) :horizontal)
                     (link-bounds-x-of self)  (link-bounds-y-of self))
          when (and (typep v 'd-view)
                    (link-cases-p v)) 
          collect v)
    (list self)))



;;===================================================================================================

(defmethod new-case-status ((self d-view)  cases status  &key )
  (declare (ignore cases status)) 
  (new-sub-views self :reposition? t)
  )

(defmethod new-case-status :before ((self d-view)  cases status  
                                    &key   )
  
  (let ((case-status (case-status-of self))
        (new-status-fn 
           (if (ignore-status-p status)
             #'(lambda(old)
                (if (invalid-status-p old)
                  :invalid-ignore :ignore) )
             #'(lambda(old)
                (if (invalid-status-p old)
                  nil t) ))))

  (if (eq cases (cases-of self))
    (loop for s in case-status
          for new = (funcall new-status-fn s)
          for i upfrom 0 when new do
            (setf (elt case-status i) new))
    
    (loop with new
          for vo in (cases-of self)
          for s in case-status
          for i upfrom 0
          when (and (or (member vo cases :test #'eq-dataset)
                        (and (identifier-of vo)
                             (member (identifier-of vo) cases :test #'eq-identifiers)))
                    (setq new (funcall new-status-fn s) )) do
          (setf (elt case-status i) new)))))


(defmethod new-case-status :around ((self d-view)  cases status  
                                    &key (rescale? t) (draw? t ) )
  (declare (ignore cases status))
  (if draw?  (erase-view self))
  (call-next-method)
  (if rescale? (reset-bounding-region self :draw? nil ))
  (if draw? (remap-to-viewports self :erase? nil :draw? draw?)))



(defmethod activate-cases ((self d-view)  cases  &rest args &key )
  (apply #'change-case-status self cases t args))

(defmethod deactivate-cases ((self d-view)  cases  &rest args &key )
  (apply #'change-case-status self cases :ignore args))


(defmethod activate-all-cases ((self d-view)    &rest args &key )
  (apply #'change-case-status self (cases-of self) t args))




(defmethod change-case-status :around ((self d-view) cases status  &rest args
                                       &key (link? t)) 
  (cond ((eq cases :selected)
         (setq cases
               (loop for v in *selected-views*
                     append (list-viewed-elements v))))
        ((eq cases :unselected)
         (let ((sel-cases (loop for v in *selected-views*
                                append (list-viewed-elements v))))
           (setq cases
                 (loop for c in (cases-of self)
                       unless (member c sel-cases :test #'eq-dataset)
                       collect c))))
        (t nil)) 
  
  (if cases
    (if (or (eq link? t)
            (and (typep link? 'view) (not (eq self link?))))
      (apply #'call-next-method self cases status args)
      (if (null link?)
        (apply #'new-case-status self cases status  args)
        (apply #'new-case-status self cases status :draw? nil args)))))



(defmethod change-case-status ((self d-view) cases status
                                &key (rescale? t) (draw? t ))
  
  (new-case-status self cases status :rescale? rescale?  :draw? draw? ))


(defmethod change-case-status ((self 2d-view) cases status
                               &key  (rescale? t) (draw? t ))
  
  
  (flet ((pass-to-links (draw? )
           (loop 
             for v in (case-links-of self) do
             (new-case-status v cases status :draw? draw? :rescale? nil)
             (if rescale? (set-bounding-region v)))))
    (if rescale?
      (with-constrained-extents self  draw?
        (pass-to-links nil ))
      
      (pass-to-links draw?))))



(defmethod change-case-status ((self 1d-view) cases status
                               &key  (rescale? t) (draw? t ))
  

  (flet ((pass-to-links (draw? )
           (loop
             for v in (case-links-of self) do
             (new-case-status v cases status :draw? draw? :rescale? nil )
             (if rescale? (set-bounding-region v))
             )))
   (if rescale?
      (with-constrained-extents self  draw?
        (pass-to-links nil ))
      (pass-to-links draw?))))