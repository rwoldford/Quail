;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               connected-points.lisp
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
;;;     C.B. Hurley 1999 NUIM
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------

(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '( connected-points
            )))



(defclass connected-points (line-segment-mixin compound-view)
  ()
  )





(defmethod construct-sub-views ((self connected-points) &rest initargs
                                &key point-view)
  (declare (ignore initargs))
  (let ((subview-arglist (subview-arg-list point-view 'point-symbol))
        (vo (viewed-object-of self)))
  (setf (subviews-of self)
        (loop repeat (length (lines-coords-of self))
        collect (apply #'view  :data vo 
                                    :left-fn '(identify-view :viewport)
                                    subview-arglist)))))

(defmethod init-position-subviews ((self connected-points) &key )
  (let* ((br (bounding-region-of self))
          (wid/2 (/ (width-of br)  20))
         (ht/2 (/ (height-of br)  20))
         (coords (lines-coords-of self))
         
         ) 
        
    (setf (sub-view-locns-of self) (loop 
            for (x y) in coords
                   collect (make-region 
                      (- x wid/2) (+ x wid/2)
                      (- y ht/2) (+ y ht/2))))))


(defmethod compute-sub-viewports ((self connected-points)
                                  &optional viewport subviews)
  (let ((viewports (if viewport (list viewport) (viewports-of self)))
        (maps (if viewport (list (select-map-to-viewport self viewport))
                  (maps-to-viewports-of self)))
        (subview-locns 
         (if subviews 
           (loop for s in subviews  collect 
                 (select-sub-view-locn self s))
           (sub-view-locns-of self)))
        
        sv-vp)
    (setq subviews (or subviews (subviews-of self)))
    
    (loop with left and right and bottom and top
          for sv in subviews
          for svl in subview-locns 
           for sv-rad  = (if (typep sv 'view-with-size)
                          (view-size-of sv) 5)
           do
           (loop for vp in viewports for map in  maps
                 for xy = (apply-transform map (centre-of svl))
                 for x = (2d-position-x xy)
                 for y = (2d-position-y xy)
                 for vp-win = (window-of vp)
                 do
                 (setq left (- x sv-rad)
                       right (+ x sv-rad)
                       bottom (- y sv-rad)
                       top (+ y sv-rad)) 
                 
                 (cond ((typep sv 'justified-line) 
                        (case (justification-of sv)
                          
                          (:left (setq left (left-of vp) right x))
                          (:right (setq right (right-of vp) left x))
                          (:top (setq top (top-of vp) bottom y ))
                          (:bottom (setq bottom (bottom-of vp) top y))
                          (:left-right (setq right (right-of vp) left (left-of vp)))
                          (:top-bottom (setq bottom (bottom-of vp) top (top-of vp)))))
                       
                       
                       ((typep sv 'oriented-line) 
                        (if  (eql (orientation-of sv) :horizontal)
                          (setq left (left-of vp) right x)
                          (setq bottom (bottom-of vp) top y)))
                       ((typep sv 'label) 
                        (let ((rad (* 3 (length (get-text sv)))))
                          (when (> rad sv-rad)
                            
                            (if (eql (orientation-of sv) :horizontal)
                              (setq left (- x rad)
                                    right (+ x rad)
                                    )
                              (setq bottom (- y rad)
                                    top (+ y rad)
                                    )))))
                  (t nil))
                 
                 (setq sv-vp (make-viewport vp-win left right bottom top))
                   (add-viewport sv sv-vp vp)))))





    

(defmethod draw-view :after ((self connected-points)   &key viewport)
   (loop for vp in (enlist-viewport self viewport) do
  (loop for sv in (subviews-of self)
        for sv-vp = (select-viewport sv vp)
         do
        (draw-view sv :viewport sv-vp :check-viewport? nil))))


(defmethod erase-view :after ((self connected-points) 
                       &key viewport )
  (loop for vp in (enlist-viewport self viewport) do
  (loop for sv in (subviews-of self)
        for sv-vp = (select-viewport sv vp)
         do
        (erase-view sv :viewport sv-vp :check-viewport? nil))))

(defmethod invert-view :after ((self connected-points) 
                       &key viewport )
   (loop for vp in (enlist-viewport self viewport) do
  (loop for sv in (subviews-of self)
        for sv-vp = (select-viewport sv vp)
         do
        (invert-view sv :viewport sv-vp :check-viewport? nil))))

(defmethod highlight-view :after ((self connected-points) 
                       &key viewport )
   (loop for vp in (enlist-viewport self viewport) do
  (loop for sv in (subviews-of self)
        for sv-vp = (select-viewport sv vp)
         do
        (highlight-view sv :viewport sv-vp :check-viewport? nil))))

(defmethod downlight-view :after ((self connected-points) 
                       &key viewport )
   (loop for vp in (enlist-viewport self viewport) do
  (loop for sv in (subviews-of self)
        for sv-vp = (select-viewport sv vp)
         do
        (downlight-view sv :viewport sv-vp :check-viewport? nil))))



(defmethod set-endpoints ((self connected-points) &key endpoints (draw? t))
  (if draw? (erase-view self))
  (if (null endpoints)
    (setf endpoints 
          (list
           (wb::prompt-user :type 'list :read-type :read
                                 :prompt-string "Enter first point as list")
           (wb::prompt-user :type 'list :read-type :read
                                 :prompt-string "Enter second point as list"))))
  (setf (lines-coords-of self) endpoints)
  (init-position-subviews self)
  (remap-to-viewports self :draw? nil :erase? nil)
  (if draw? (draw-view self)))














(defmethod subsubview-styles ((self rotating-line-segments)  style &optional default)
  (when (typep (car (subviews-of self)) 'connected-points)
  (loop for sv in (subviews-of self) append
        (loop for s in (subviews-of sv)
        collect (draw-style (drawing-style-of s) style :default default)))))





