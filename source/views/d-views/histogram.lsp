;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               histogram.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(break-points-of compute-break-points
           set-nbins  set-break-points histogram-view histogram-bar
           bar-heights-of )))

;;;----------------------------------------------------------------------------------
#|
(defclass histogram-bar (bar)
  ()
  (:documentation "A kind of bar which handles highlighting as appropriate for ~
                   a histogram bar."))

(defmethod highlight-view ((self histogram-bar) &key viewport )
  "Highlight a proportion of each color-fill combintation ~
   by examining drawing styles. "
  (with-exposed-viewports self viewport vp
    (multiple-value-bind (l r b tp) (bounds-of vp)
      (let ((op  (highlight-operation self))
            (axis (orientation-of self))
            (x-wid  (- r l))
            (y-wid  (- tp b))
            (w (window-of vp))
            (highlight-proportion 0))
        (setf highlight-proportion
            (or  (loop with styles = (style-proportions self '(:highlight?))
                    
                    for (style)  in (first styles)
                    for prop in (second styles)
                    thereis (and style prop))
                 0)
              )
        (unless (zerop highlight-proportion)
        (if (eql axis :horizontal)
          (setq y-wid  (round (* highlight-proportion (+ 1 y-wid))))
          (setq x-wid  (round (* highlight-proportion (+ 1 x-wid)))))
        (wb:canvas-highlight-rectangle w l (min r (+ l x-wid))
                                         b   (min tp (+ b y-wid))
                                         :color *default-highlight-color*
                                         :operation op)
        
        )))))


(defmethod draw-view ((self histogram-bar) &key viewport )
  
  "Color and fill self in proportion to the colors/fill of the drawing-style."
  (if (collect-styles-p self) 
    (call-next-method)
  (with-exposed-viewports self viewport vp
    (multiple-value-bind (l r b tp) (bounds-of vp)
      (let ((axis (orientation-of self))
            (x-wid (- r l))
            (y-wid (- tp b)) (w (window-of vp)))
        (cond ((zerop x-wid)
               (wb:canvas-draw-line 
                w l  b l tp  :color (draw-style self :color)))
              ((zerop y-wid)
               (wb:canvas-draw-line 
                w l  b r b :color (draw-style self :color)))
              
              (t
               (loop with l1 and b1 and tp1 and  r1 and
                     last-fill = nil
                     with styles = (style-proportions self '(:highlight? :fill?  :color  ))
                     with nstyles = (length (first styles))
                     for (hi fill col) in (first styles)
                     for (next-fill) in (append (cdr (first styles)) (list nil nil nil))
                     for prop in (second styles)
                     for i upfrom 1
                     do 
                     (if (eql axis :horizontal)
                       (setq y-wid  (round (* prop (+ 1 (height-of vp)))))
                       (setq x-wid  (round (* prop (+ 1 (width-of vp))))))
                     (setq b1 b tp1 (min tp (+ b y-wid))
                           l1 l r1 (min r (+ l x-wid))) 
                     (cond (fill
                            (wb:canvas-draw-filled-rectangle  w l1  r1 b1 tp1 :color col))
                           ((eql axis :horizontal)
                            (wb:canvas-draw-line w l1 b1  l1 (1- tp1) :color col)
                            (wb:canvas-draw-line w r1 b1 r1 (1- tp1) :color col)
                            (unless last-fill
                              (wb:canvas-draw-line w l1 b1 r1 b1 :color col))
                            (unless next-fill
                              (if (= nstyles i)
                                (wb:canvas-draw-line w l1 tp1 r1 tp1 :color col)
                              (wb:canvas-draw-line w l1 (1- tp1) r1 (1- tp1) :color col))))
                           (t
                            (wb:canvas-draw-line w l1 b1  (1-  r1) b1 :color col)
                            (wb:canvas-draw-line w l1 tp1 (1-  r1) tp1 :color col)
                            (unless last-fill
                              (wb:canvas-draw-line w l1 b1 l1 tp1 :color col))
                            (unless next-fill
                              (if (= nstyles i)
                             (wb:canvas-draw-line w r1 b1 r1 tp1 :color col)
                              (wb:canvas-draw-line w (1-  r1) b1 (1- r1) tp1 :color col))))
                       )
                     
                     (if (eql axis :horizontal)
                       (incf b y-wid) (incf l x-wid))
                     (setq last-fill fill)
                     ))
              ))))))


(defmethod highlight-view ((self histogram-bar) &key viewport )
  "Highlight a proportion of each color-fill combintation ~
   by examining drawing styles. "
  (with-exposed-viewports self viewport vp
    (multiple-value-bind (l r b tp) (bounds-of vp)
      (let ((op  (highlight-operation self))
            (axis (orientation-of self))
            (x-wid  (- r l))
            (y-wid  (- tp b))
            (w (window-of vp))
            (highlight-proportion 0))
        (setf highlight-proportion
              (loop with styles = (if (collect-styles-p self)
                                    (style-proportions self '(:fill?  :color :highlight?))
                                    (style-proportions-in-order self '(:fill?  :color :highlight?)))
                    
                    for style  in (first styles)
                    for prop in (second styles)
                    sum
                    (if (third style) prop 0))
              )
        
        (if (eql axis :horizontal)
          (setq y-wid  (round (* highlight-proportion (+ 1 (height-of vp)))))
          (setq x-wid  (round (* highlight-proportion (+ 1 (width-of vp))))))
        (wb:canvas-highlight-rectangle w l (min r (+ l x-wid))
                                         b   (min tp (+ b y-wid))
                                         :color *default-highlight-color*
                                         :operation op)
        
        ))))
|#

(defclass histogram-view (boxed-subview-mixin styles-cache 1d-view compound-view)
  ((middle-menu :allocation :class
                 :initform nil)
   (filled? :allocation :class :initform nil :accessor filled-p)

   (style-keys :initform '(:size :fill? :box-color :color) :allocation :class)
    
   (break-points :initform nil
                 :initarg :break-points
                 :accessor break-points-of)
   (scale :initform :frequency
                        :initarg :histogram-scale
                        :accessor histogram-scale-of
               :documentation "Possibilities are :frequency :relative-frequency or :density"))
  (:default-initargs :justification :bottom :fill? *default-bar-fill?*
    :orientation :horizontal :box? nil
                     :color  *default-bar-color*))

(defgeneric set-nbins (histogram-view   &key  nbins draw?)
  (:documentation "Set number of bins.~
                  If nbins is nil, the user is prompted."))

(defgeneric set-break-points (histogram-view   &key  break-points draw?)
  (:documentation "Set break-points.~
                  If break-points is nil, the user is prompted."))

(defgeneric set-histogram-scale (histogram-view   &key  scale draw?)
  (:documentation "Sets histogram scale to scale"))
 

(defgeneric bar-heights-of (bars)
  (:documentation "Returns heights of bars"))


(defgeneric compute-bar-heights (histogram-view scale)
  (:documentation "Computes and returns the bar heights using scale~
                   :frequency :relative-frequency or :density"))

(defgeneric compute-break-points ( histogram-view &optional nbins )
  (:documentation "Returns a list of break points used to form the histogram bins.~
                   If nbins is provided, there will be nbins+1 break points."))

;;;----------------------------------------------------------------------------------




(defun default-nbins (n)
  (if (zerop n) 0
  (max 2 (truncate (+ 1 (log n 2))))))


(defun compute-bins(min max nbins &optional (fudge 0))
  (if (= min max) (incf max 1))
  (let ((tic-info (scale-axis min  max  (1+ nbins)) ))
    
    (when (= (tic-max tic-info) max)  
      (setq fudge (* fudge (- max min)))
      (setq tic-info 
            (scale-axis min (+ max fudge) (1+ nbins))))
    (loop with wid = (tic-inc tic-info)
          for i from 0 to nbins
          collect (+ (tic-min tic-info) (* i wid)))))
  
(defmethod compute-break-points ((self histogram-view) &optional nbins )
  
  (unless (break-points-of self)
    (let* ((coords (active-members self (plot-coords-of self)))
           max min)
      (if (null coords)
        (setf (break-points-of self) nil)
        (progn
          (loop for c in coords
                maximize c into amax
                minimize c into amin
                finally (setq min amin max amax))
          (unless nbins 
            (setf nbins (default-nbins
                          (length ( active-members self (cases-of self))))))
          (setf (break-points-of self) (compute-bins min max nbins 0.01))))))
  (break-points-of self))
  




(defun equispaced-list-p (l &optional fudge)
  "Tests if list elements are equispaced"
  (if l
    (if fudge
      (loop with diff = (- (second l) (first l))
            with d2 = (* diff (1+ fudge))
            with d1 = (* diff (1- fudge))
            for b1 in (cdr l)
            for b2 in (cddr l)
            always (<= d1 (- b2 b1) d2))
      (loop with diff = (- (second l) (first l))
            for b1 in (cdr l)
            for b2 in (cddr l)
            always (= (- b2 b1) diff)))))




(defun which-interval-fn (l)
  "Returns a function which gives the~
  index 0..length(l) -2 of the interval~
   containing its argument"
  (let ((n (- (length l) 1)))
    (if (equispaced-list-p l)
      (let* ((left (first l))
             (right (first (last l)))
             (fac (/ n (- right left))))
        #'(lambda (c) (let ((bin (floor (* (- c left) fac))))
                        (if (and ( >= bin 0) (< bin  n))
                          bin))))
      #'(lambda (c)
          (let ((p (position c l :test #'<)))
            (if (and p (> p 0))
              (- p 1)))))))





(defmethod construct-sub-views ((self histogram-view)
                                &key nbins break-points bars
                                fill?
                                color)
  (if break-points (setf (break-points-of self) break-points))
  (let* ((bp (compute-break-points self nbins ))
         (orientation (orientation-of self))
         (style-arglist (append
                         bars
                         (list :color color :fill? fill? )))
         sub-styles sub-vos)
    (setq bars (subview-arg-list bars 'bar)) 
    (when bp
      (multiple-value-setq ( sub-vos sub-styles)
        (group-and-sort-viewed-objs self (which-interval-fn bp) (- (length bp) 1)
                           (get-viewed-obj-styles self :fill? (getf style-arglist :fill?) 
                                                  :color (getf style-arglist :color))))
      
      (loop while (null (first sub-vos)) do
            (setf sub-vos (cdr sub-vos))
            (setf sub-styles (cdr sub-styles))
            (setf (break-points-of self) (cdr (break-points-of self) )))
      
      (loop while (null (car (last sub-vos))) do
            (setf sub-vos (butlast sub-vos))
            (setf sub-styles (butlast sub-styles))
            (setf (break-points-of self)  (butlast (break-points-of self))))
      
      
      (setf (subviews-of self)
            (loop for vos in sub-vos 
                  for vo-style in sub-styles
                  collect (apply #'view  :data vos :check-style? t
                                 :viewed-elements vos
                                 :orientation orientation
                                 :drawing-style vo-style bars)))
      ) 
    ))


(defmethod set-nbins ((self histogram-view)   &key  nbins (draw? t))
  (if (null nbins)
    (setq nbins (wb:prompt-user :type 'number :read-type :eval
                                :prompt-string "Enter a positive integer")))
  
  (with-constrained-extents self 
    draw?
    (let ()
      (new-sub-views self :nbins nbins)
      (init-position-subviews self)
      (set-bounding-region self ))))

(defmethod set-break-points ((self histogram-view)   &key  break-points (draw? t))
  (if (null break-points)
    (setq break-points 
          (wb:prompt-user :type 'list :read-type :read
                          :prompt-string "Enter a list of break points")))
  (with-constrained-extents self 
    draw?
    (let ()
      (new-sub-views self :break-points  break-points)
      (init-position-subviews self)
      (set-bounding-region self ))))

(defmethod set-histogram-scale ((self histogram-view)   &key  scale (draw? t))
  (setf (histogram-scale-of self) scale)
  (with-constrained-extents self 
    draw?
    (let ()
      (init-position-subviews self)
      (set-bounding-region self )))
  (update-text-links self))


 (defmethod new-sub-views :before ((self histogram-view)  &key)
  
  (setf (break-points-of self) nil))
  
  


(defmethod bar-heights-of ((self histogram-view))
  (let ((b (break-points-of self)))
  (if (and b (not (equispaced-list-p b .0001)))
    (setf (histogram-scale-of self) :density)))
  
  (compute-bar-heights self (histogram-scale-of self)))


(defmethod compute-bar-heights ((self histogram-view) (scale t))
    (loop for bar in (subviews-of self)
          when (typep bar 'bar)
          collect  (bar-count bar)))


(defmethod compute-bar-heights ((self histogram-view) (scale (eql :relative-frequency)))

  (let* ((heights (compute-bar-heights self :frequency))
        (scale (loop for h in heights 
                     sum h)))
    (mapcar #'(lambda(x) (/ x scale)) heights)))

(defmethod compute-bar-heights ((self histogram-view) (scale (eql :density)))

  (let* ((heights (compute-bar-heights self :frequency))
         (break-points (break-points-of self))
         (scale (loop for h in heights 
                     sum h))
        )
    (unless (zerop scale) (setq scale (/ 1 scale)))
    (loop for h in heights 
                      for b1 in break-points
                      for b2 in (cdr break-points)
                      collect (/ (* scale h)  (- b2 b1)))))

  

   

(defmethod smallest-bounding-region  ((self histogram-view) )
  (let ((br (call-next-method self))
        hgt)
    (when (and (bar-heights-of self) (cases-of self))
      (setq hgt (apply #'max (bar-heights-of self)))
      
      (multiple-value-bind (l r b tp) (bounds-of br )
        (setf (bounds-of br)
              (if (eq (orientation-of self) :vertical)
                (list 0.0 hgt b tp)
                (list l r 0.0 hgt)
                ))))
    br))

(defmethod set-bounding-region :after ((self histogram-view) &key (pretty? t) 
                                       ignore-x? ignore-y? &allow-other-keys)
  (let ((dir (axis-of-orientation self)))
 
  (if (and pretty? (break-points-of self)
           (not (or (and ignore-x? (eq dir :x)) (and ignore-y? (eq dir :y))) ))
    (let ((min-pt (first (break-points-of self)))
          (max-pt (first (last (break-points-of self)))))
      (expand-extent self (list min-pt max-pt))))))
    



(defmethod init-position-subviews ((self histogram-view) &key)
  
  (loop with reg
        for bar in (subviews-of self)
        for b1 in (break-points-of self)
        for b2 in (cdr (break-points-of self))
        for hgt in (bar-heights-of self) do
        (setq reg 
              (if (eq (orientation-of self) :vertical)
                (make-region 0.0 hgt b1 b2 )
                (make-region b1 b2 0.0 hgt)
                ))
        (place-subview self bar reg)) )

(defmethod allowed-subview-movement ((self histogram-view) subview)
  ;; histogram bars may not be moved
  (declare (ignore subview))
  :none)

(defmethod style-menu-items ((self histogram-view))
  `(( "Color" nil "" :sub-items ,(color-menu-items))
    ("Invisible?"  (set-drawing-style :invisible? :toggle :highlit?  *selected-subviews-only* ))
    ("Fill?" (set-drawing-style :fill? :toggle :highlit?  *selected-subviews-only*))
    ))

(defmethod get-menu-items ((self histogram-view) (slot-name (eql 'middle-menu)))
  `(("# Bins" (set-nbins))
        ("BreakPoints" (set-break-points))
        ("Scale" nil "" :sub-items 
         (("Frequency" (set-histogram-scale :scale :frequency))
          ("RelFrequency" (set-histogram-scale :scale :relative-frequency))
          ("Density" (set-histogram-scale :scale :density))))))


(defmethod toggle-fill ((self histogram-view) &key keys )
  (setf (filled-p self) (not (filled-p self)))
  (apply #'set-drawing-style self :fill? (filled-p self) keys))


(defmethod update-menu-items ((self histogram-view) 
                              (slot-name (eql 'middle-menu)))
  (let* ((m (slot-value self slot-name)))
    (wb:check-menu-item m "Fill?" (draw-style self :fill?))))

(defmethod styles-to-subs ((self histogram-view) ) 
  (list :fill? :highlight? :color))


(defmethod free-coord-string ((self histogram-view))
  (let ((scale (histogram-scale-of self)))
      (case scale
        (:relative-frequency "Relative Frequency")
        (:density "Density")
        (t "Frequency"))))




(defmethod compute-default-clip-region ((view histogram-view) (on-view view))
  :max)


(defmethod change-variable ((self histogram-view) &rest args
                            &key var function (transform :none) (draw? t) (link? t) )
  
  (let* ( (axis  (axis-of-orientation self))
          (other-axis (if (eq axis :x) :y :x))
        (free-dir (free-axis self link?)))

    (flet ((pass-to-links()
             (loop for v in (variate-links-of self)
                   do
                   (cond 
                    ((and (eq axis :x) (typep v '2d-view ))
                     (change-variable v :x var :x-function function 
                                      :x-transform transform 
                                      :draw? draw? :link? self ))
                    ((and (eq axis :x) (typep v '2d-view ))
                     (change-variable v :y var :y-function function 
                                      :y-transform transform 
                                      :draw? draw? :link? self ))
                    
                    ((typep v '1d-view )
                     (change-variable v :var var :function function 
                                      :transform transform :draw? draw? :link? self )
                     )))))

      (cond ((eq free-dir :both)
             (with-constrained-extents self  draw? 
                (pass-to-links) ))
            ((eq free-dir axis)
             (with-constrained-extent self axis draw? 
               (pass-to-links)))
            ((eq free-dir other-axis)
             (with-constrained-extent self other-axis draw? 
               (apply #'new-variable self :draw? nil args)))
               
            (t  (apply #'new-variable self :draw? nil args))))))

#|
(defmethod coord-strings ((self histogram-view))
  (list (coord-string-x self) (coord-string-y self)))

|#

(defmethod use-x-axis-p ((self histogram-view))
  t)

(defmethod use-y-axis-p ((self histogram-view))
  t)


