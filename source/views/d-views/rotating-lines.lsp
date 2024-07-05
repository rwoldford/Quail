;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               rotating-lines.lisp
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
;;;     C.B. Hurley 1995 George Washington University
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)


(eval-when (:compile-toplevel :load-toplevel :execute) (export '(rotating-line-segments  
          rotate-line-segments  )))




(defclass rotating-line-segments (line-segments-per-case-mixin rotating-cloud)
  ((middle-menu :allocation :class :initform nil)
  
 )
  (:default-initargs :axis-color nil
    :initform-fn  #'get-data-inits-3lists))
  
;; move-points is not a good name but move-view is used up.
(defmethod move-points ((self rotating-line-segments) &rest args )
  (apply #'rotate-line-segments  self args))



;;--------------------------------------------------------------------------------

;; coords are a list ((x1 x2) (y1 y2) (z1 z2)) for every segment (adjust compute status)
;; plots coords contain a list ((x1 x2) (y1 y2) (z1 z2)) for every segment
;; scaled coords are as above
;; centered-viewport coords are 2 lists for each case (x1 y1 z1) (x2 y2 z2)


(defun flatten-tuples(tuples)
  (loop for c in tuples
        append (if (and c (every #'listp c)) (apply #'mapcar #'list c))))



(defun list-tuples(tuples &optional (n 2))
  (loop for c on tuples by #'(lambda(l) (nthcdr n l))
        collect (apply #'mapcar #'list (subseq c 0 n))))




;;;------------------------------------------------------------
(defmethod compute-scaled-coords ((self rotating-line-segments) 
                                  &key (cases (cases-of self)))
  
  
  (let* ((coords (coords-of self :cases cases))
         (n (coords-per-segment self))
         (status
          (if  (eq cases (cases-of self))
            (case-status-of self)
            (loop for c in cases collect (select-case-status self c))))
         (scale-method (coords-scale-method-of self)))
    
    (if scale-method
      (list-tuples 
       (funcall (coords-scale-method-of self) 
                (flatten-tuples coords) 
                (loop for s in status
                      append (make-list n :initial-element s)))
       n)
      coords)
    ))
;;;------------------------------------------------------------







(defmethod rotate-paired-coords ((self rotating-line-segments) coords &key (integer? t))
  (setf (rotation-of self) (or (rotation-of self) (identity-3d-rotation)))
  (loop with fn = (if integer? #'round #'identity)
        with ((x1 y1 z1) 
              (x2 y2 z2)
              (x3 y3 z3)) float  = (rotation-of self)
        
        for (a b c) in coords
        collect
        (if (and (every #'numberp a) (every #'numberp b) (every #'numberp c))
          (loop for ai in a for bi in b for ci in c
                collect (funcall fn (+ (* x1 ai) (* x2 bi) (* x3 ci))) into newa
                collect (funcall fn  (+ (* y1 ai) (* y2 bi) (* y3 ci))) into newb
                collect (funcall fn   (+ (* z1 ai) (* z2 bi) (* z3 ci))) into newc
                finally (return (list newa newb newc))))))

(defmethod centered-viewport-coords-of ((self rotating-line-segments) 
                            viewport)
  (let ((n (coords-per-segment self))
        (c (scale-data-for-viewport self
                           (flatten-tuples (plot-coords-of self))
                           viewport)))
    (if (= n 2) c
        (loop for co on c by #'(lambda(l) (nthcdr n l))
              for x = (subseq co 0 n)
              collect (first x)
              append
                    (butlast
                     (loop for xi in (cdr x) collect xi collect xi))))))
              


(defmethod smallest-bounding-region ((self rotating-line-segments) )
  (let ((br (make-region)))
    (if (cases-of self)
      (loop   for (x y z) in  (scaled-coords-of self )
              for s in (case-status-of self)
              when (active-status-p s)
              maximize (loop for xi in x for yi in y for zi in z 
                             maximize (+ (* xi xi) (* yi yi) (* zi zi)))
              into radius
             finally 
             (setq radius (sqrt radius))
             (if (zerop radius) (incf radius 1))
             (setf (bounds-of br) 
                   (list (- radius) radius 
                         (- radius) radius))))
    br))

(defmethod recenter-scaled-coords ((self rotating-line-segments) &optional center)
  (let (cx cy cz )
    (cond ((listp center)
           (setq cx (first center) cy (second center) cz (third center)))
          ((eq center :selected)
           (loop  for  sub in (subviews-of self)
                  for s in (case-status-of self)
                  for (x y z)  in (scaled-coords-of self)
                  when (and (draw-style sub :highlight?) (active-status-p s))
                   sum (reduce #'+ x) into sumx and
                  sum (reduce #'+ y) into sumy and sum (reduce #'+ z) into sumz and
                  sum (length x) into n
                  finally (unless (zerop n)
                            (setq cx (round (/ sumx n))  
                                  cy (round (/ sumy n))  
                                  cz (round (/ sumz n))))))
          ((eq center :original)
           (setf (scaled-coords-cache-of self) nil)
           (setf (plot-coords-cache-of self) nil))
          (t nil))
    
    (when (and (numberp cx) (numberp cy) (numberp cz))
      (loop for s in (case-status-of self)
            for (x y z) in (scaled-coords-of self)
            unless (invalid-status-p s)
            do (loop for i from 0 below (length x) do
                     (decf (nth i x) cx)
                     (decf (nth i y) cy)
                     (decf (nth i z) cz)))


      (let* ((r (rotation-of self))
             (sx (round (+ (* (elt (elt r 0) 0) cx) 
                           (* (elt (elt r 1) 0) cy) 
                           (* (elt (elt r 2) 0) cz))))
             (sy (round (+ (* (elt (elt r 0) 1) cx) 
                           (* (elt (elt r 1) 1) cy) 
                           (* (elt (elt r 2) 1) cz))))
             (sz (round  (+ (* (elt (elt r 0) 2) cx) 
                            (* (elt (elt r 1) 2) cy) 
                            (* (elt (elt r 2) 2) cz)))))
        (loop for s in (case-status-of self)
              for (x y z) in (plot-coords-of self)
              unless (invalid-status-p s)
              do 
              (loop for i from 0 below (length x) do
                     (decf (nth i x) sx)
                     (decf (nth i y) sy)
                     (decf (nth i z) sz))
             
             )))))

(defmethod bounds-of-selected ((self rotating-line-segments))
   (loop  for sub in (subviews-of self)
             for status in (case-status-of self)
             for (x y z)  in (scaled-coords-of self)
             when (and (draw-style sub :highlight?) (active-status-p status))
              maximize (loop for xi in x for yi in y for zi in z 
                             maximize (+ (* xi xi) (* yi yi) (* zi zi))) into radius
             finally 
             (when radius
             (setq radius (sqrt radius))
             (if (zerop radius) (incf radius 1))
             (return (make-region (- radius) radius (- radius) radius)))))

;;;------------------------------------------------------------


  






(defmethod compute-plot-coords ((self rotating-line-segments))
  (rotate-paired-coords self (scaled-coords-of self)))






(defmethod expand-styles ((self rotating-line-segments) style &optional default)
  (let ((n (coords-per-segment self)))
    (if (<= n 2)
      (loop for sv in (subviews-of self)
        collect (draw-style (drawing-style-of sv) style :default default))

      (loop for sv in (subviews-of self)
            for s = (draw-style (drawing-style-of sv) style :default default)
                   append (make-list (- n 1) :initial-element s)))))
  



(defmethod rotate-line-segments ((self rotating-line-segments) &key 
                          viewport (direction :y) (steps 1000) 
                          (increment (increment-of self)))
  
  (unless viewport
    (setq viewport
          (if (> (length (viewports-of self)) 1)
            (which-viewport self)
            (car (viewports-of self)))))
  
  (if (and viewport (not (moving-p self))
           (active-viewport-p viewport))
    (let* ((temp-coords (centered-viewport-coords-of self  viewport))
           (axis-color (draw-style self :axis-color))
           (colors (expand-styles self :color))
           (sizes (expand-styles self :width 1))
           (invisible? (expand-styles self :invisible?))
           (pcolors (subsubview-styles self :color))
           (psymbols (subsubview-styles self :symbol :box))
           (psizes (subsubview-styles self :size 4))
           (pfill? (subsubview-styles self :fill?))
           (pinvisible? (subsubview-styles self :invisible?))
           (draw-rate (draw-rate-of self))
           (draw-region (get-draw-portion self viewport))
           (viewport-coords (viewport-locns-of self  viewport))
           (ignore-lines (loop with n =  (max 1 (- (coords-per-segment self) 1))
                               with vis = (visible-subs-p self)
                               with status = (case-status-of self)
                               for i in invisible?
                               for j upfrom 0
                               for k = (truncate j n)
                               collect (or i (not (nth k vis))  (not (active-status-p (nth k status))))) 
                         )
           (ignore-points (loop with n =  (coords-per-segment self)
                               with vis = (visible-subs-p self)
                               with status = (case-status-of self)
                               for i in pinvisible?
                               for j upfrom 0
                               for k = (truncate j n)
                               collect (or i (not (nth k vis))  (not (active-status-p (nth k status))))) 
                         ))
      
      (unwind-protect
        (progn
          (setf (moving-p self) t)
         (set-draw-style self :highlight? nil)
          (loop for s in (subviews-of self) 
                when (draw-style s :highlight?) do
                (set-drawing-style s :highlight? nil))
                
           (if (draw-axis-p self)
            (erase-tripod self :viewport viewport))
          (if (draw-label-p self)
              (erase-tripod-labels self :viewport viewport))
             
          (multiple-value-bind 
            (rot end-coords end-axes)
            (if (typep (car (subviews-of self)) 'connected-points)
            (wb::rotate-line-segments-points (window-of viewport) 
                                   temp-coords
                                   :axes (if (draw-axis-p self) 
                                           (centered-viewport-axis-coords-of self  viewport))
                                   :axis-color axis-color
                                   :width sizes 
                                   :color colors  :invisible? ignore-lines
                                   :psize psizes :psymbol psymbols :pfill? pfill?
                                   :pcolor pcolors  :pinvisible? ignore-points
                                 
                                   :erase-points viewport-coords
                                   :erase-axes (if (draw-axis-p self) 
                                                 (viewport-axis-coords-of self viewport))
                                   :stop-fn (stop-rotate self)
                                   :plot-rgn (wb-region draw-region)
                                   :direction direction :steps steps :increment increment
                                   :viewport-coords? nil
                                   :draw-rate draw-rate)
            (wb::rotate-line-segments (window-of viewport) 
                                   temp-coords
                                   :axes (if (draw-axis-p self) 
                                           (centered-viewport-axis-coords-of self  viewport))
                                   :axis-color axis-color
                                   :width sizes 
                                   :color colors  :invisible? ignore-lines
                                      :erase-points viewport-coords
                                   :erase-axes (if (draw-axis-p self) 
                                                 (viewport-axis-coords-of self viewport))
                                   :stop-fn (stop-rotate self)
                                   :plot-rgn (wb-region draw-region)
                                   :direction direction :steps steps :increment increment
                                   :viewport-coords? nil
                                   :draw-rate draw-rate))
            
            (wb::mapply-rotation! rot (rotation-of self) :integer? nil)
            (setf (plot-coords-cache-of self) nil)
          
            (adjust-subviews self )
            (set-viewport-locns self :viewport viewport :coords end-coords)
            (set-viewport-coords self :viewport viewport :coords end-axes)
            (if (draw-label-p self)
              (draw-tripod-labels self :viewport viewport))
            
            (loop for vp in (viewports-of self)
                  unless (eq vp viewport) 
                  do
                  (remove-viewport-coords self :viewport vp)
                  (set-viewport-locns self :viewport vp)
                  (draw-view self :erase? nil :viewport vp))))
        (setf (moving-p self) nil)
       (draw-view self :erase? t :viewport viewport)
        
        ))))




(defmethod viewport-locns-of ((self rotating-line-segments)  viewport)
  (setq viewport (or viewport (car (viewports-of self))))
  (loop  with n = (coords-per-segment self)
         with m = (* 2 (- n 1))
         for sv in (subviews-of self)
        for s in (case-status-of self)
        for c = (lines-coords-for-viewport sv (select-viewport sv viewport))
           append (if (active-status-p s)
                  (cons (list (caar c) (cdar c))
                       (butlast 
                        (loop for ci in (cdr c )
                              for a = (list (car ci) (cdr ci))
                              collect a collect a)))
                  (make-list m))))

(defmethod set-viewport-locns ((self rotating-line-segments) &key viewport  coords)
  (if (and viewport coords)
    (loop with n = (coords-per-segment self)
          with m = (* 2 (- n 1))
          for sv in (subviews-of self)
          for vp = (select-viewport sv viewport)
          for vpc = (viewport-coords-of sv :viewport vp)
          for c on coords by #'(lambda(l) (nthcdr m l))
          do (loop for (x y) in (append (list (car c)) (subseq c 0 m)) by #'cddr
                   for vc in vpc 
                   for point-sym in (subviews-of sv) 
                   for vp-sv = (select-viewport point-sym vp)
                  do (setf (car vc) x) (setf (cdr vc) y)
                   (set-square-viewport-center vp-sv x y)
                   maximize x into x2
                   maximize y into y2
                   minimize x into x1
                   minimize y into y1
                   finally (setf (bounds-of vp) (list x1 x2 y1 y2))))
   ;; (compute-sub-viewports self viewport)
    (map-subviews-to-viewport self viewport)
    ))

(defmethod adjust-subviews ((self rotating-line-segments) 
                            &optional (new-coords (plot-coords-of self) ))
  
  (loop for (x y) in new-coords
        for l in (sub-view-locns-of self)
        for sub in (sub-views-of self)
        for s in (case-status-of self)
        unless (invalid-status-p s)
        do
        (setf (lines-coords-of sub) (mapcar #'list x y))
        (setf (bounds-of (bounding-region-of sub)) (list (apply #'min x) (apply #'max x)
                                  (apply #'min y) (apply #'max y)))
        
        (when (typep sub 'connected-points)
          (setf (bounds-of l) (list (apply #'min x) (apply #'max x)
                                 (apply #'min y) (apply #'max y)))
          (loop  for point-l in (sub-view-locns-of sub)
                for xi in x for yi in y
                do
                (set-square-region-center point-l xi yi)))))





(defmethod visible-subviews-p ((self rotating-line-segments))
  
  (loop with br = (bounding-region-of self)
        with sr = (expt (radius-of br) 2)
        
        for (x y z) in  (scaled-coords-of self )
        for s in (case-status-of self)
        collect (and (active-status-p s)
                       (<=  (loop for xi in x for yi in y for zi in z 
                             minimize (+ (* xi xi) (* yi yi) (* zi zi))) sr))))



(defmethod cases-in-selected-region ((self rotating-line-segments))
  (let (cx cy cz new-rad )
    (loop  for  sub in (subviews-of self)
           for s in (case-status-of self)
           for (x y z)  in (scaled-coords-of self)
           when (and (draw-style sub :highlight?) (active-status-p s))
           sum (reduce #'+ x) into sumx and
           sum (reduce #'+ y) into sumy and sum (reduce #'+ z) into sumz and
           sum (length x) into n
           finally (unless (zerop n)
                     (setq cx (round (/ sumx n))  
                           cy (round (/ sumy n))  
                           cz (round (/ sumz n)))))
    
    (when (and (numberp cx) (numberp cy) (numberp cz))
      (setq new-rad
            (loop for s in (case-status-of self)
                  for  sub in (subviews-of self)
                  for (x y z) in (scaled-coords-of self)
                  when (and (draw-style sub :highlight?) (active-status-p s))
                  do (loop for i from 0 below (length x) do
                     (decf (nth i x) cx)
                     (decf (nth i y) cy)
                     (decf (nth i z) cz))
                  and
                  maximize (loop for xi in x for yi in y for zi in z 
                             maximize (+ (* xi xi) (* yi yi) (* zi zi))) into radius
                  finally 
                  (return radius)))
      (loop   for (x y z) in  (scaled-coords-of self )
              for vo in (cases-of self)
              for s in (case-status-of self)
              when (and (active-status-p s)
                        (loop for i from 0 below (length x) do
                              (decf (nth i x) cx)
                              (decf (nth i y) cy)
                              (decf (nth i z) cz))
                        (<=  (loop for xi in x for yi in y for zi in z 
                             maximize (+ (* xi xi) (* yi yi) (* zi zi))) new-rad))
              collect vo))))





(defmethod subsubview-styles ((self rotating-line-segments)  style &optional default)
  (when (typep (car (subviews-of self)) 'connected-points)
  (loop for sv in (subviews-of self) append
        (loop for s in (subviews-of sv)
        collect (draw-style (drawing-style-of s) style :default default)))))

