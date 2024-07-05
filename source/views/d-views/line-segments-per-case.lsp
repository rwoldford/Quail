;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               line-segments-per-case.lisp
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
;;;     C.B. Hurley 1996 Maynooth College
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)


(eval-when (:compile-toplevel :load-toplevel :execute) (export '(line-segments-per-case-mixin  
          line-segments-per-case )))



(defclass line-segments-per-case-mixin()
   ( (style-keys :initform '(:width :font  :color :axis-color) :allocation :class)) 
  (:default-initargs :case-view 'line-segment
    :width 1
    :initform-fn #'get-data-inits-2lists))

#|
(defclass line-segments-per-case-mixin()
   ( (style-keys :initform '(:width :font  :color :axis-color) :allocation :class)) 
  (:default-initargs :case-view 'connected-points
    :width 1
    :initform-fn #'get-data-inits-2lists))
|#

(defclass line-segments-per-case(line-segments-per-case-mixin 2d-one-per-case)
   ((middle-menu :allocation :class :initform nil)) 
  )


(defmethod case-access-fn ((self line-segments-per-case-mixin) var)
  ;; returns a function to access  value from a case
  (let ((vars (variates-of self))
        (val-fn (value-fn-of self)))
    (cond ((functionp var) var)
          ((listp var)
           #'(lambda (c) (funcall val-fn c var :vars vars)))
          (t #'(lambda (c)  (funcall val-fn c var :vars vars))))))
    
(defmethod compute-coords ((self line-segments-per-case-mixin) &key (cases (cases-of self)))
  ;; returns list of case coords used in plot
  (let* ((vo (cases-of self))
        (ans (loop for f in (functions-of self)
                 for tr in (transforms-of self)
                 for v in (vars-of self)
                 collect 
                 (if (eql :iseq v)
                   (if (eql cases vo)
                     (loop for i from 0 below (length cases) collect (list i))
                     (loop for c in cases collect (list (position c vo))))
                   (case-coords cases f tr))))
        (len (loop for (c) in ans 
                   maximize (if (listp c) (length c) 1))))
    (setq ans
          (loop for c in ans
                collect (if (listp (car c)) c
                            (loop for ci in c
                                  collect (make-list len :initial-element ci)))))
    (apply #'mapcar #'list ans)))

(defmethod smallest-bounding-region ((self line-segments-per-case) )
  (if (and (cases-of self) (car (cases-of self))
           (x-variate-of self) (y-variate-of self))
    (loop for (x y) in (plot-coords-of self )
          for s in (case-status-of self)
          when (active-status-p s)
          minimize (apply #'min x) into x-min and maximize (apply #'max x) into x-max and
          minimize (apply #'min y) into y-min and maximize (apply #'max y) into y-max 
          finally 
          
          (return (progn
                    
                    (when (= x-min x-max) 
                      (let ((fudge (* 0.1  (abs x-max))))
                        (if (zerop fudge) (setq fudge 1))
                        (incf x-max fudge)
                        (decf x-min fudge)))
                    (when (= y-min y-max) 
                      (let ((fudge (* 0.1  (abs y-max))))
                        (if (zerop fudge) (setq fudge 1))
                        (incf y-max fudge)
                        (decf y-min fudge)))
                    (make-region x-min x-max y-min y-max))))
    
    (make-region)))


(defmethod bounds-of-selected ((self line-segments-per-case))
  (loop  for sub in (subviews-of self)
             for status in (case-status-of self)
             for (x y)  in (plot-coords-of self)
             when (and (draw-style sub :highlight?) (eq status t))
             minimize (apply #'min x) into x-min maximize (apply #'max x) into x-max 
             minimize (apply #'min y) into y-min maximize (apply #'max y) into y-max 
             finally (if (= x-min x-max)
                       (incf x-max 0.01))
             (if (= y-min y-max)
               (incf y-max 0.01))
             (return (make-region x-min x-max y-min y-max))))

(defmethod visible-subviews-p ((self line-segments-per-case))
  
  (loop with br = (bounding-region-of self)
        for svl in (sub-view-locns-of self) 
        for s in (case-status-of self)
        collect (and (active-status-p s)
                         (contains-p br svl)
                         t)))


(defmethod coord-string-x ((self line-segments-per-case-mixin))
  (let ((vars (variates-of self)))
    (plot-axis-string (mapcar #'(lambda(a)
                                  (plot-axis-string a nil nil vars))
                              (x-variate-of self) ) (x-func-of self)
                      (x-transform-of self))))


(defmethod coord-string-y((self line-segments-per-case-mixin))
  (let ((vars (variates-of self)))
    
    (plot-axis-string (mapcar #'(lambda(a)
                                  (plot-axis-string a nil nil vars))
                              (y-variate-of self) ) (y-func-of self)
                      (y-transform-of self))))

   (defmethod coord-string-z((self line-segments-per-case-mixin))
     (if (z-variate-of self)
       (let ((vars (variates-of self)))
         
         (plot-axis-string (mapcar #'(lambda(a)
                                       (plot-axis-string a nil nil vars))
                                   (z-variate-of self) ) (z-func-of self)
                           (z-transform-of self)))))


(defmethod coord-strings ((self line-segments-per-case-mixin))
  (let ((varsl (variates-of self)))
       
  
  (loop for v in (vars-of self)
        for f in (funcs-of self)
        for tr in (transforms-of self)
        collect 
        (plot-axis-string (mapcar #'(lambda(a)
                                      (plot-axis-string a nil nil varsl))
                                  v ) f tr))))







;; same as rotating-line-segments
(defmethod init-position-subviews ((self line-segments-per-case-mixin) &key )
  (let* ((subs (subviews-of self))
          (status (case-status-of self))
         
        (coords (plot-coords-of self))
         )
    (setf (sub-view-locns-of self)
          (loop with region
                for sub in subs
                for (x y ) in coords
                 for s in status 
                   do
                (setf (lines-coords-of sub) (mapcar #'list x y))
                (setq region (if (invalid-status-p s) 
                               (make-region)
                               
                                  (make-region  (apply #'min x) (apply #'max x) 
                                             (apply #'min y) (apply #'max y))))
                (setf (bounding-region-of sub) region)
                collect region))))


;; same as rotating-line-segments
(defmethod compute-sub-viewports ((self line-segments-per-case-mixin)
                                   &optional viewport subviews)
  
  ;;same as that for view 
  (let ((viewports (if viewport (list viewport) (viewports-of self)))
        (maps (if viewport (list (select-map-to-viewport self viewport))
                  (maps-to-viewports-of self)))
        (subview-locns 
         (if subviews 
           (loop for s in subviews  collect 
                 (select-sub-view-locn self s))
           (sub-view-locns-of self)))
        (status
         (if subviews
           (loop for s in subviews collect (select-subview-status self s))
           (case-status-of self))))
    (setq subviews (or subviews (subviews-of self)))
    (loop for sv in subviews
          for svl in subview-locns
          for s in status do
          (loop for vp in viewports for map in maps
                for sv-vp = (or (select-viewport sv vp)
                                (make-viewport (window-of vp)))
                do
                (setf (bounds-of sv-vp) (apply-transform map svl))
                (unless (active-status-p s) (deactivate-viewport sv-vp))
                (add-viewport sv sv-vp vp)))))


;; same as rotating-line-segments
(defmethod get-menu-items ((self line-segments-per-case-mixin) (slot-name (eql 'middle-menu)))
  (labels ((add-to-set-style(arg)
             (cond 
              ((not (listp arg))
               arg)
              ((eq (car arg) 'set-drawing-style) 
               (setf (cdr (last arg)) `(:highlit?  *selected-subviews-only*))
               arg)
              ((eq (car arg) 'set-color-style) 
               (setf (cdr (last arg)) `(:highlit?  *selected-subviews-only*))
               arg)
             ((equal (car arg) "Width")
               nil)
              
              ((equal (car arg) "Invisible?")
               nil)
              ((equal (car arg) "Endpoints")
               nil)
              (t (mapcar #'add-to-set-style arg)))
             ))
    (let* ((sv (car (subviews-of self)))
           (sv-items (if sv (add-to-set-style
                             (copy-tree
                              (get-menu-items sv 'middle-menu)))))
           (width (loop for key in '(:fatter :thinner :prompt)
                       collect `(,(string-downcase key)
                                 (set-subview-width ,key 
                                                   :highlit? *selected-subviews-only*))))
           
                
           (bounds
            (bounds-menu-item-list self)))
        
        `(,@sv-items
          ( "Toggle hi" (toggle-highlight ))
          ( "Width" nil "" :sub-items ,width)
          ( "Invisible?" (set-invisible-subviews t :highlit? *selected-subviews-only* ))
          ( "AllVisible?" (set-invisible-subviews nil :highlit? nil))
          
          ("-" nil)
          ( "Bounds" nil "" :sub-items ,bounds)
          ))))

(defmethod variable-transform-list ((self line-segments-per-case-mixin ))
  nil)


(defmethod menu-variates-of ((self line-segments-per-case-mixin)) nil)

;; same as
(defmethod styles-to-subs ((self line-segments-per-case-mixin ) ) 
  (list :highlight? :width :color ))


;;  same as


(defmethod subview-location-string ((self line-segments-per-case-mixin) (subview view))
  (let ((l
         (car (coords-of self :cases (list (viewed-object-of subview)))))) 
    (if l 
       (format nil "~{~A~^ ~}" (apply #'mapcar #'list l)))))

;;  same as

(defmethod  get-subview-width  ((self line-segments-per-case-mixin) new)
  (let* ((old (draw-style self :width)))
        
    (case new
      (:fatter (+  old 1)) 
      (:thinner (max 1 (-  old 1)))
      (:prompt (wb:prompt-user :result-type 'number 
                               :read-type :eval
                               :prompt-string 
                               (format nil "Change width from ~A" old)))
      (t old))))

;;  same as
(defmethod set-subview-width ((self line-segments-per-case-mixin) new &key (draw? t) highlit?)
  (unless (numberp new)
    (setq new (get-subview-width self new)))
  (set-draw-style self :width new)
  (loop for v in (some-subviews self :highlit? highlit?) do 
        (if (typep v 'line-segment) 
          (set-drawing-style v :width new :draw? draw?))))



;;  same as
(defmethod distance-to-location ((self line-segments-per-case-mixin) viewport location)
  
  (if (selected-p self viewport location)
    (let ((locn-c (if (region-p location)
                    (centre-of location) location)))
      (loop with dist
                with mdist = (* 2 (radius-of  viewport))
                for sv in (subviews-of self)
                for s in (case-status-of self)
                for sv-vp = (select-viewport sv viewport)
                when (active-status-p s) do
                (setq dist (distance-to-location sv sv-vp locn-c))
                (setq mdist 
                      (min mdist dist))
                finally (return mdist)))
    
    10000))

;;  same as
(defmethod subview-highlight-styles ((self line-segments-per-case-mixin))
(loop for sv in (subviews-of self)
      for sv-style = (assoc :highlight? (car (drawing-styles-of  sv)))
      collect sv-style))



;;  same as

(defmethod valid-coord-test ((self line-segments-per-case-mixin) c)
  (and c (every #'(lambda(ci) (and (listp ci) (> (length ci) 1) (every #'realp ci))) c)))



(defmethod coords-per-segment ((self line-segments-per-case-mixin))
  (let ((c (caar (coords-of self))))
    (if c ( length c)
        2)))






(defmethod construct-sub-views ((self line-segments-per-case-mixin) &rest initargs
                                &key case-view (linkable-case-views? t)
                                (labels #'identifier-name) colors
                                (point-symbols *point-symbol-types*)
                                (point-colors (list *red-color* *blue-color* *green-color* *yellow-color*)))
  (if (member labels (variates-of self) :test #'eq-variate-exprs)
    (setq labels (case-access-fn self labels)))
  (let* ((subview-arglist (subview-arg-list case-view 'connected-points))
         (vo  (cases-of self)))
    (when case-view
      (loop for key in (slot-value self 'style-keys)
            for hold = (getf subview-arglist key :xxx) 
            unless (eq hold :xxx) do
            (set-draw-style self key hold)))
    
    
    (setq subview-arglist (append subview-arglist (list :linkable? linkable-case-views? :menu? nil :clip-draw? nil)))
    (unless (subviews-of self)
      (setf  (subviews-of self)
             (loop with sub-styles
                   with first-col with new-sub
                   for case in vo
                   for c in (plot-coords-of self)
                   for i upfrom 0
                   for l in (if (listp labels) labels
                                (make-list (length vo) :initial-element labels))
                   collect 
                   (let ()
                     (setq new-sub (apply #'view  :data case 
                                          :lines-coords (apply #'mapcar #'list c)
                                          :left-fn '(identify-view :viewport)
                                          ;;  :label l :text l :linkable? t
                                          (append subview-arglist (list :label l :text l))))
                     (when (null sub-styles)
                       (setq sub-styles 
                             (loop with new = (append subview-arglist initargs   )
                                   for k in (slot-value new-sub 'style-keys)
                                   for m = (member k new)
                                   when m
                                   collect k and
                                   collect (cadr m)))
                       
                       
                       (setq first-col (getf  sub-styles :color)))
                     (when colors
                       (setf (getf sub-styles :color) (or (nth i colors) first-col)))
                     
                     (set-drawing-style new-sub :styles sub-styles) 
                     
                     new-sub)  )))
    
    (if point-symbols
      (loop with n = (length point-symbols)
            for s in (subviews-of self) do
            (loop for sub in (subviews-of s)
                  for i upfrom 0
                  do
                  (set-draw-style sub :symbol (nth  (mod i n) point-symbols)))))
    (if point-colors
      (loop with n = (length point-colors)
            for s in (subviews-of self) do
            (loop for sub in (subviews-of s)
                  for i upfrom 0
                  do
                  (set-draw-style sub :color (nth  (mod i n) point-colors)))))
    ))












