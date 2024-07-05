;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               table-layout.lisp
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
;;;     C.B. Hurley 1996 
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(table-layout proportional-height proportional-area proportional-width rescale-entries
 set-cell-text mosaicify)))
;;;----------------------------------------------------------------------------------



(defclass table-layout(batch-layout justification-mixin)
  ((scaled-dimension :initform nil :initarg :scaled-dimension
                     :accessor scaled-dimension-of)
   (scales :initform nil :initarg :scales
                     :accessor scales-of)
   (middle-menu :allocation :class :initform nil))
  (:default-initargs :gap-x 0.02 :gap-y 0.02 :justification nil
    :box-views? nil :justification-menu? NIL
    :subview-type 'bar-with-text :default-subview-type 'bar-with-text :orientation :horizontal))


(defgeneric proportional-width(table-layout &key viewport  scale &allow-other-keys)
  (:documentation "Make the widths of the table entries proportional to width. ~
                   Scale should be a list of non negative numbers, ~
                   one per subview."))

(defgeneric proportional-height(table-layout &key viewport scale &allow-other-keys)
  (:documentation "Make the heights of the table entries proportional to scale. ~
                   Scale should be a list of non negative numbers, ~
                   one per subview."))

(defgeneric proportional-area(table-layout &key viewport  scale &allow-other-keys)
  (:documentation "Make the areas of the table entries proportional to scale. ~
                   Scale should be a list of non negative numbers, ~
                   one per subview."))


(defgeneric  rescale-entries (table-layout
                            &key  viewport draw? dimension scale &allow-other-keys)
  (:documentation "Make the dimension specified of the table entries proportional to scale. ~
                   Scale should be a list of non negative numbers, ~
                   or a function which will be applied to the view ~
                   of the table entry. If scale is a number, all table entries ~
                   will be given the same dimension. "))
(defgeneric  mosaicify (table-layout
                        &key  viewport draw?  scale recursive?)
  (:documentation "Mosaicifies the table. ~
                   If recursive? is non-nil, mosaicify is applied recursively to subviews. ~
                   Scale should be a list of non negative numbers, ~
                   or a function which will be applied to the view ~
                   of the table entry. If scale is a number, all table entries ~
                   will be given the same dimension. "))


(defgeneric  set-cell-text (table-layout text
                            &key  draw? )
  (:documentation "Use text to display in cells"))


(defmethod construct-sub-views :after (( self table-layout) &rest keyword-pairs 
                                &key cell-text)
  (declare (ignore keyword-pairs cell-text)) 
  ;;(set-cell-text self cell-text :draw? nil)
  )


(defmethod set-cell-text (( self table-layout) bar-text &key (draw? t))
      (if bar-text
    (if (and (listp bar-text) (= (length bar-text) (length (layout-views-of self))))
      (loop for v in (layout-views-of self)
            for b in bar-text 
            for l = (find '(or label text-display-mixin titled-view-mixin) v :test #'(lambda(a b) (typep b a)))
            when l do
            (if (typep l 'titled-view-mixin) (setq l (title-of l)))
            (setf (text-of l) b))
      (loop for s in (subviews-of self)
             when (typep s '(or label text-display-mixin titled-view-mixin)) do
            (if (typep s 'titled-view-mixin) (setq s (title-of s)))
            (setf (text-of s) bar-text))))
      (if draw? (draw-view self :erase? t)))
    

(defmethod reshape-viewport :after ((self table-layout)  vp  
                             &key new-location transform draw?)
  (declare (ignore new-location transform))
  (rescale-entries self :viewport vp :draw? draw?))

(defmethod reposition-view :after ((self table-layout)
                            &key default-positions draw?)
  (declare (ignore default-positions))
  (rescale-entries self  :draw? draw?))


(defmethod map-subviews-to-viewport :after ((self table-layout) &optional vp
                                            subviews) 
  (declare (ignore subviews))
  (rescale-entries self :viewport vp :draw? nil))
  
(defmethod rescale-entries ((self table-layout)
                            &key  viewport justification (draw? t) dimension scale)
  (if (eq scale :prompt)
    (setq scale
          (get-function
           (wb::prompt-user :result-type  t
                            :read-type :read
                            :prompt-string "Enter scaling function, applied to each view"))))
  (if justification
    (setf (justification-of self) justification))
  (if dimension
    (setf (scaled-dimension-of self) dimension))
  (setq scale (or scale (scales-of self)))
  
  (let ( (subs (subviews-of self)))
    (setq scale
          (cond ((and scale (listp scale) 
                      (= (length scale) (length subs)))
                 scale)
                ((functionp scale)
                 (mapcar #'(lambda(v) (funcall scale v))
                         subs))
                ((numberp scale)
                 (make-list  (length subs) :initial-element 1))
                (t (mapcar #'(lambda(v) (if (typep v 'bar) (bar-count v) 
                                            (length (list-cases (viewed-object-of v)))))
                           subs))))
    (setf (scales-of self) scale)
    
    (setq viewport (or viewport  (car (viewports-of self))))
    (if draw? (erase-view self  :viewport viewport))
    (case (scaled-dimension-of self)
      (:width (proportional-width self :viewport viewport :scale scale ))
      (:height (proportional-height self :viewport viewport :scale scale ))
      (:area (proportional-area self :viewport viewport :scale scale ))
      (t (setq draw? nil)))
    (loop for sv in (subviews-of self) do
          (map-to-viewport sv (select-viewport sv viewport)))
    (when draw?  ;; (draw-view self :erase? nil :viewport viewport)
      (draw-view self :erase? nil :viewport viewport))))

(defmethod proportional-width ((self table-layout) 
                               &key  viewport  scale)
  
  (let ((max-c 0) fac tr l max-w 
        (just (justification-of self))
        (subs (subviews-of self))
        (nr (nrows-of self)) (nc (ncols-of self)) 
        (gap-x (getf (default-positions-of self) :gap-x))
        (gap-vp ))
    
    
    (if (eq just :free)
      (progn
        (setq max-c
              (loop with s = scale
                    for i from 0 below nr maximize
                     (loop for j from 0 below nc sum (car s) do (setf s (cdr s)))))
        (loop for vport in (enlist-viewport self viewport) do
              (setq tr (select-map-to-viewport self vport))
              (setq l (car (sub-view-locns-of self)))
              (setq max-w (if l
                            (* nc (width-of (apply-transform tr l)))
                            (width-of vport)))
              ;;(setq gap-vp (ceiling (* gap-x (/ max-w nc))))
              (setq gap-vp (round (* gap-x (/ max-w nc))))
              (unless (zerop gap-x) (setq gap-vp (max gap-vp 1)))
              (setf fac   (/ max-w max-c))
              (loop  with vp-left with left with right
                     for v in subs 
                     for c in scale
                     for i upfrom 0
                     for col = (mod  i nc)
                     for w = (round (* fac c ))
                     for l in (sub-view-locns-of self)
                     for vp = (select-viewport v vport) 
                     do
                     (if (and (= i 0) (null left))
                       (setq left (round (left-of (apply-transform tr l)))))
                     (if (and (null right) (= i (- nc 1)))
                       (setq right (round (right-of (apply-transform tr l)))))
                     
                     (cond 
                      ((= col 0)
                       (setf (left-of vp) left)
                       (setf (right-of vp)  (+ (left-of vp) w )))
                      
                      (t
                       (setf vp-left (select-viewport (nth (- i 1) subs) vport))
                       (setf (left-of vp) (+ (right-of vp-left) gap-vp))
                       (setf (right-of vp) 
                             (if (= col (- nc 1))
                               right (+ (left-of vp) w ))))))))
      
      
      (progn
        (setq max-c (apply #'max scale))
        (unless (<= max-c 0)
          (loop for vport in (enlist-viewport self viewport) do
                (setq tr (select-map-to-viewport self vport))
                (setq l (car (sub-view-locns-of self)))
                (setq max-w (truncate 
                             (if l
                               (width-of (apply-transform tr l))
                               (/ (width-of vport) (ncols-of self)))))
                (setf fac   (/ max-w max-c))
                
                
                (loop with vp1 = (make-viewport (window-of vport)) 
                      for v in subs 
                      for c in scale
                      for w = (round (* fac c ))
                      for l in (sub-view-locns-of self)
                      for vp = (select-viewport v vport) 
                      for h = (height-of vp)
                      do
                      
                      (setf (bounds-of vp1) (apply-transform tr l)) 
                      (case  just
                        (:bottom  
                         (setf (bottom-of vp) (bottom-of vp1))
                         (setf (top-of vp) (+ (bottom-of vp) h ))
                         (set-viewport-width-height vp w nil))
                        (:top  
                         (setf (top-of vp) (top-of vp1))
                         (setf (bottom-of vp) (- (top-of vp) h ))
                         (set-viewport-width-height vp w nil))
                        (:left 
                          (setf (left-of vp) (left-of vp1))
                          (setf (right-of vp) (+ (left-of vp) w)))
                        (:right
                          (setf (right-of vp) (right-of vp1))
                          (setf (left-of vp) (- (right-of vp) w))
                          (setf (height-of vp) h))
                        (t (set-viewport-width-height vp w h))) 
                      )
                ))
        ))
    ))



(defmethod proportional-height ((self table-layout) 
                                &key  viewport   scale)
  
  
  (let (  (max-c 0) fac tr l max-h 
          (just (justification-of self))
          (subs (subviews-of self))
          (nr (nrows-of self)) (nc (ncols-of self)) 
          (gap-y (getf (default-positions-of self) :gap-y))
          gap-vp)
    
    (if (eq just :free)
      (progn
        (setq max-c
              (loop  for j from 0 below nc maximize
                     (loop for i from 0 below nr sum (nth (+ j (* i nc))  scale) )))
        (loop for vport in (enlist-viewport self viewport) do
              (setq tr (select-map-to-viewport self vport))
              (setq l (car (sub-view-locns-of self)))
              (setq max-h (if l
                            (* nr (height-of (apply-transform tr l)))
                            (height-of vport)))
            ;;  (setq gap-vp (ceiling (* gap-y (/ max-h nr))))
              (setq gap-vp (round (* gap-y (/ max-h nr))))
              (unless (zerop gap-y) (setq gap-vp (max gap-vp 1)))
              (setf fac   (/ max-h max-c))
              (loop  with vp-above with top
                     for v in subs 
                     for c in scale
                     for i upfrom 0
                     for row = (truncate  i nc)
                     for h = (round (* fac c ))
                     for l in (sub-view-locns-of self)
                     for vp = (select-viewport v vport) 
                     do
                     (if (= i 0)
                       (setq top (round (top-of (apply-transform tr l)))))
                     
                     (cond 
                      ((= row 0)
                       (setf (top-of vp) top)
                       (setf (bottom-of vp)  (- (top-of vp) h )))
                      
                      (t
                       (setf vp-above (select-viewport (nth (- i nc) subs) vport))
                       (setf (top-of vp) (- (bottom-of vp-above) gap-vp))
                       (setf (bottom-of vp)  (- (top-of vp) h )))))))
      
      
      (progn
        (setq max-c (apply #'max scale))
        
        (unless (<= max-c 0)
          (loop for vport in (enlist-viewport self viewport) do
                (setq tr (select-map-to-viewport self vport))
                (setq l (car (sub-view-locns-of self)))
                (setq max-h (truncate 
                             (if l
                               (height-of (apply-transform tr l))
                               (/ (height-of vport) nr))))
                (setf fac   (/ max-h max-c))
                
                
                (loop  with vp1 = (make-viewport (window-of vport)) 
                       for v in subs 
                       for c in scale
                       for h = (round (* fac c ))
                       for l in (sub-view-locns-of self)
                       for vp = (select-viewport v vport) 
                       for w = (width-of vp)
                       do
                       (setf (bounds-of vp1) (apply-transform tr l))
                       (case  just
                         (:bottom  
                          (setf (bottom-of vp) (bottom-of vp1))
                          (setf (top-of vp) (+ (bottom-of vp) h )))
                         (:top
                          (setf (top-of vp) (top-of vp1))
                          (setf (bottom-of vp) (- (top-of vp) h ) ))
                         (:left 
                          (setf (left-of vp) (left-of vp1))
                          (setf (right-of vp) (+ (left-of vp) w))
                          (set-viewport-width-height vp nil h))    
                         (:right
                          (setf (right-of vp) (right-of vp1))
                          (setf (left-of vp) (- (right-of vp) w))
                          (set-viewport-width-height vp nil h))
                         (t (set-viewport-width-height vp w h))))
                ))))))

(defmethod justification-of ((self table-layout))
  (let ((j (slot-value self 'justification)))
  (if (null j)
    (cond ((= 1 (nrows-of self)) :bottom)
          ((= 1 (ncols-of self)) :left)
          (t :center))
    j)))

(defmethod proportional-area ((self table-layout) 
                               &key  viewport   scale)
  
  (let ((size (mapcar #'sqrt scale)))
      (proportional-width self :viewport viewport  :scale size)
      (proportional-height self  :viewport viewport :scale size)))    
  

(defmethod get-menu-items ((self table-layout) (slot-name (eql 'middle-menu)))
  '(("-" nil)
    ( "Justification" (set-subview-justification  :prompt :draw? T)
                    "Prompt for a change in the justification of the text.")
    ("Width" nil "" :sub-items (("Proportional" (rescale-entries :dimension :width ))
                                ("Proportional to" 
                                 (rescale-entries :dimension :width :scale :prompt ))
                           ("Constant" (rescale-entries :dimension :width :scale 1))))
    ("Height" nil "" :sub-items (("Proportional" (rescale-entries :dimension :height ))
                                 ("Proportional to" (rescale-entries :dimension :height :scale :prompt))
                                  ("Constant"  (rescale-entries :dimension :height :scale 1))))
    ("Area" nil "" :sub-items (("Proportional" (rescale-entries :dimension :area ))
                               ("Proportional to" (rescale-entries :dimension :area :scale :prompt))
                                  ("Constant" (rescale-entries :dimension :area :scale 1 ))))
    ("Mosaic" nil "" :sub-items (("Proportional" (mosaicify ))
                               ("Proportional to" (mosaicify :scale :prompt))
                                  ("Constant" (mosaicify :scale 1 ))))
                           ))


(defmethod set-subview-justification ((self table-layout) justification &key (draw? nil) )
  (if (eq :prompt justification)
    (setq justification
          (car (wb:prompt-for-items (list :center :left :right  :bottom :top )
                                    :prompt-text "Choose subview justification"))))
  (rescale-entries self  :draw? draw? :justification justification)
  
  (loop for s in (subviews-of self) 
        when (and (typep s 'justification-mixin) (member justification
                                                         (list-legal-justifications s))) do
        (set-justification s justification :draw? draw?)))

                                       
(defmethod mosaicify ((self table-layout) &key scale (draw? t) viewport (recursive? t))
  (setq viewport (or viewport  (car (viewports-of self))))
  (if (eq scale :prompt)
    (setq scale
          (get-function
           (wb::prompt-user :result-type  t
                            :read-type :read
                            :prompt-string "Enter scaling function, applied to each view"))))
  
  (if draw? (erase-view self  :viewport viewport))
  (cond ((= 1 (ncols-of self))
         (rescale-entries self  :dimension :height :scale scale :justification :free :draw? nil))
        ((= 1 (nrows-of self))
         (rescale-entries self  :dimension :width :scale scale :justification :free :draw? nil))
        (t
         (rescale-entries self  :dimension :area :scale scale :justification :center :draw? nil)))
  
  (if recursive?
    (loop for b in  (subviews-of self) 
        when (typep b 'table-layout) do
        (mosaicify b   :scale scale :draw? nil :viewport (select-viewport b viewport) )))
  (when draw?  ;; (draw-view self :erase? nil :viewport viewport)
    (draw-view self :erase? nil :viewport viewport)))