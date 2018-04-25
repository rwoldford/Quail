;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               plot.lisp
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
;;; 7/15/93 added label handling to delete-subview :before ((self plot) view)
;;; also allowed subview argument to be function
;;;----------------------------------------------------------------------------------


(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(plot *default-plot-gap* standard-plot prompt-for-plot-interior row-views col-views
          margin-string-left margin-string-right margin-string-bottom margin-string-top )))


(defvar *default-plot-gap* 0)

;;;----------------------------------------------------------------------------------
(defclass plot-mixin (bordered-view-mixin titled-view-mixin 
                        margined-view-mixin)
                                          
  ()
  )




(defclass plot (plot-mixin compound-view)
  ((right-menu :allocation :class :initform nil)
   (middle-menu :allocation :class :initform nil)
   (default-interior :initform 'view
     :accessor default-interior-of :initarg :default-interior)
   (position-keys :allocation :class 
                  :initform '(:left-view-size :right-view-size 
                              :bottom-view-size :top-view-size
                              :bottom-label-size :top-label-size
                              :left-label-size :right-label-size 
                              :title-height :title-width
                              :left-margin-size :right-margin-size
                              :bottom-margin-size :top-margin-size
                              :gap-x :gap-y ) ))
  (:default-initargs 
     :link-bounds-x? :by-col :link-bounds-y? :by-row
    :gap-x *default-plot-gap* :gap-y *default-plot-gap*
    :interior-view :default
    )
  (:documentation "The basic plot. "))



(defclass standard-plot (plot)
  ()
  (:default-initargs 
    :right-margin-size 0.07
    :left-view 'axis :bottom-view 'axis 
    :left-label 'label :bottom-label 'label
      )
  (:documentation "Plot with left and bottom axes and labels . "))


(defmethod initialize-instance :around ((self plot)
                                        &rest initargs
                                        &key (margins :default) 
                                      &allow-other-keys) 
  (case margins
    (:none (if (getf initargs :title)
             (apply #'call-next-method self 
                  :no-margin-views? t
                  :no-labels? t
                         initargs)
             (apply #'call-next-method self 
                  :title nil :no-margin-views? t
                  :no-labels? t
                         initargs)))
    
    (t (call-next-method))))

(defgeneric prompt-for-plot-interior (plot)
  (:documentation "Prompts user for plot interior")
  )


(defgeneric margin-string-bottom (view)
  (:documentation "A string to be used on a bottom plot margin around view."))

(defgeneric margin-string-right (view)
  (:documentation "A string to be used on a right plot margin around view."))

(defgeneric margin-string-left (view)
  (:documentation "A string to be used on a left plot margin around view."))

(defgeneric margin-string-top (view)
  (:documentation "A string to be used on a top  plot margin around view."))



(defmethod prompt-for-plot-interior ((self plot))
    (choose-view-from-menu 
                    :prompt-string "Select plot interior"
                    :superclass 'view))


  







;;;----------------------------------------------------------------------------------

(defmethod auto-draw-p ((self plot))
  *auto-draw-plot?*)

(defmethod subview-position-region-of :before ((self plot))
  (if (null (slot-value self 'subview-position-region))
    (setf (slot-value self 'subview-position-region) 
          (make-region (bounding-region-of self)))))



(defmethod allowed-subview-movement ((self plot) subview)
  ;; may returns :x :y :both or :none
  ;; determines how subview may be moved within view
  (cond 
   ((or (member subview (left-views-of self))
        (member subview (right-views-of self)))
    :x)
   ((or (member subview (bottom-views-of self))
        (member subview (top-views-of self)))
    :y)
   ((member subview (interior-views-of self))
    :none)
   (t :both)))


(defmethod layer-subview ((self plot) (view linkable-bounds-mixin )
                          (on linkable-bounds-mixin )
                          &rest arg
                          &key (ignore-x? :default)
                               (ignore-y? :default))

  (if 
    (member on (append (bottom-views-of self)
                       (top-views-of self)))
    (if (eq ignore-y? :default) (setq ignore-y? t))
    (if (member on (append (left-views-of self)
                           (right-views-of self)))
      (if (eq ignore-x? :default) (setq ignore-x? t))))
  (let ((locn (select-sub-view-locn self on)))
    (if (null locn)
      (quail-error "~S is not a subview of ~S" on self))
    (place-subview self view (make-region locn)) 
    
    (apply #'layer-link-view view  on :parent self
           :ignore-x? ignore-x? :ignore-y? ignore-y? arg)
    (add-to-named-slot self on view)
    )
  view
  )

;;;----------------------------------------------------------------------------------

(defun keyword-list-p(arg)
  (and arg
       (listp arg)
       (evenp (length arg))
       (loop for key in arg by #'cddr
             always (keywordp key))))
  
(defun legal-view-construct-p(arg)
  "Tests if the argument can be used to construct a view.~
   Argument should be a view, symbol,function or a list. ~
   If the argument is a list, it should contain~
   keywords followed by their values."
  
  (or (symbolp arg) (functionp arg)
      (view-p arg)
      (keyword-list-p arg)))
                

(defun legal-label-construct-p(arg)
  "Tests if the argument can be used to construct a label.~
   Argument should be a label, symbol, function, string or a list. ~
   If the argument is a list, it should contain~
   keywords followed by their values."
  
  (or (symbolp arg) (functionp arg) (stringp arg)
      (typep arg 'label)
      (keyword-list-p arg)))


(defun subview-arg-list(arg &optional default-type)
  (let* ((arglist
         (cond 
          ((or (null arg) (eq t arg)) nil)
          ((or (symbolp arg) (functionp arg)) (list :type arg))
          ((and (listp arg) (evenp (length arg)))
           arg)
          (t nil)))
        (type-val (getf arglist :type)))
    (if (and type-val (not (eq t type-val)))
      arglist
      (setf (getf arglist :type)  default-type))
    
    (setf (getf arglist :draw?) nil) 
    arglist))

(defun sublabel-arg-list(arg &optional (default-type 'label))
  (let* ((arglist
          (cond ((or (symbolp arg) (functionp arg)) (list :type arg))
                ((stringp arg) (list :text arg))
                ((and (listp arg) (evenp (length arg)))
                 arg)
                (t nil)))
         (type-val (getf arglist :type)))
    (if (and type-val (not (eq t type-val)))
      arglist
      (setf (getf arglist :type)  default-type))
    
    (setf (getf arglist :draw?) nil) 
    arglist))


(defun view-from-arg(view-args other-args 
                               &optional 
                               (default-type 'view)
                               (arg-list-fn #'subview-arg-list))
 (if (view-p view-args)
    view-args
    (apply #'view
           (append 
            (funcall arg-list-fn view-args  default-type)
            other-args))))

(defun margin-view-from-arg(view-args other-args data-args 
                               &optional 
                               (default-type 'view)
                               (arg-list-fn #'subview-arg-list))
 (if (view-p view-args)
    view-args
    (let* ((args (funcall arg-list-fn view-args  default-type))
          (new-var (getf args :var)) (new-data (getf args :data)))
      (setq args (append args other-args))
      (cond (new-data
             (apply #'view args))
            (new-var
              (apply #'view (append args (list :function nil :transform nil :coords nil)
                                   data-args)))
            (t (apply #'view (append args data-args)))))))




(defmethod construct-margin-labels ((self plot) &key label-font)
  "Constructs label views using information provided by~
   the :left-label :right-label :bottom-label :top-label initargs.~
   Each label initarg should pass the legal-label-construct-p test,~
   or be a list whose elements pass the legal-label-construct-p test.~
   If an initarg is null, the corresponding label is not constructed.~
   If the initarg does not provide a string, it will be obtained from~
   the interior-view.~
   Each legal-label-construct, together with keyword-pairs,~
   is used to construct labels, which are stored in the~
   corresponding label slot.~
   Label font will be used as the font for any constructed label, when~
   the legal-label-construct does not have a font value."
  
  
  (with-accessors ((ll  left-labels-of) (rl right-labels-of)
                   (tl top-labels-of) (bl bottom-labels-of)
                   (vo viewed-object-of))
                  self
     (flet  ((label-from-arg(arg margin &optional string)
              (let ((new (view-from-arg 
                          arg 
                          (list :orientation nil :font label-font  :data vo :text string)
                          'label #'sublabel-arg-list)))
               (init-label-text self margin new)
                new)))
      
      (multiple-value-setq (tl bl rl ll)
        (values-list
         (loop  for l in (list tl bl rl ll)
               for margin in (list :top :bottom :right :left)
                   collect
               (if (null l) nil
                   
                   (cond 
                    ((typep l 'view)
                     (list (label-from-arg l margin)))
                    ((eql l :if-string)
                     (loop for s in (case margin
                                      (:top (margin-string-top self))
                                      (:bottom (margin-string-bottom self)) 
                                      (:right (margin-string-right self))
                                      (:left (margin-string-left self)))
                            collect
                           (label-from-arg t margin s)))

                    ((legal-label-construct-p l)
                     (loop for s in (or (case margin
                                      (:top (margin-string-top self))
                                      (:bottom (margin-string-bottom self)) 
                                      (:right (margin-string-right self))
                                      (:left (margin-string-left self)))
                                        (list ""))
                            collect
                           (label-from-arg l margin s)))
                    
                    ((and (listp l)   (every #'legal-label-construct-p l))
                     (loop for li in l 
                           collect  (label-from-arg li margin)))
                    
                    (t (quail-error "Illegal subview argument ~A" l))))))))))



(defmethod construct-interior-view ((self plot) 
                                    &rest keyword-pairs  &key  )
  "Constructs an interior-view  using information provided by~
   the :interior-view initarg.~
   The interior-view initarg should pass the legal-view-construct-p test,~
   or be a list whose elements pass the legal-view-construct-p test.~
   Each legal-view-construct, together with keyword-pairs,~
   is used to construct a view placed in the interior-view slot. ~
   There must be at least one such view."
  
  
  (with-accessors ((iv interior-views-of))
                  self
    (let (var-args mem) 
      (labels ((local-make-view(arg) 
                 (let ((v
                        (view-from-arg arg (append keyword-pairs var-args))))
                   (setq var-args (var-info v var-args))
                   v)))
        
        (cond ((eql iv :prompt)
               (setf iv (prompt-for-plot-interior self)))
              ((or (null iv) (eql iv :default))
               (setf iv (default-interior-of self)))
              ((and (listp iv) (setq mem (member :default iv)))
               (let ((default (default-interior-of self)))
                 (if (legal-view-construct-p default)
                   (setf (car mem) default)
                   (setf iv (append (butlast
                                     (nreverse (member :default (reverse iv))))
                                    default
                                    (cdr mem)))))))
        
        (setf iv    
              (cond ((legal-view-construct-p iv)
                     (list (local-make-view iv)))
                    ((and (listp iv) (every #'legal-view-construct-p iv))
                     (mapcar #'local-make-view iv))
                    (t (quail-error "Illegal subview argument ~A" iv))))
         ))))

(defmethod view-title-string ((self plot))
  nil
  )

(defmethod construct-sub-views :after ((self plot) 
                                &rest keyword-pairs &key  )
  (declare (ignore keyword-pairs))
  (let ((ti (title-of self))
        (iv (interior-view-of self)))
    (when (and ti iv (null (text-of ti)))
      (setf (text-of ti)
            (view-title-string iv)))))
     
(defmethod plot-margin-br-info ((self plot) axis &optional from-view)
  (let* ((iv (or from-view (interior-view-of self)))
         (br (if (typep iv 'd-view) 
               (smallest-bounding-region iv)
               (bounding-region-of iv)))
         (iv-axis (if (typep iv '1d-view) (orientation-of iv))))
    (cond 
     
     
     ((typep iv '2d-view)
      (if (eq axis :horizontal)
        (list    :bounding-region (if (null (x-variate-of iv)) br)
                 :pretty? t
                 :flip-x? (flip-x-p iv))
        (list  :bounding-region (if (or (null (y-variate-of iv))
                                        (typep iv 'line-segments-per-case)) br)
               :pretty? t
               :flip-y? (flip-y-p iv))))
     
     ((and (typep iv '1d-view) (eq iv-axis axis))
      (list    :bounding-region (if (null (variate-of iv)) br)
               :pretty? t
               :flip-x? (if (eq iv-axis :horizontal) (flip-x-p iv))
               :flip-y? (if (eq iv-axis :vertical) (flip-y-p iv))))
     
     ((typep iv '1d-view)
      (list    :bounding-region (smallest-bounding-region iv)
               :pretty? t
               :flip-x? (if (eq iv-axis :vertical) (flip-x-p iv))
               :flip-y? (if (eq iv-axis :horizontal) (flip-y-p iv))))
     (t (list  :pretty? t
               :bounding-region br)))))

(defmethod plot-margin-coord-info ((self plot) axis &optional from-view)
  (let* ((iv (or from-view (interior-view-of self)))
         
         (iv-axis (if (typep iv '1d-view) (orientation-of iv))))
    (append (if (typep iv 'd-view)
              (list :cases (cases-of iv)
                    :value-fn (value-fn-of iv)
                    :variates (variates-of iv)))
    (cond 
     ((typep iv 'line-segments-per-case)
      (if (eq axis :horizontal)
        (list :var (x-variate-of iv)
              :function (x-func-of iv)
              :transform (x-transform-of iv)
              :coords (mapcar #'first (x-coords-of iv)))
        (list :var (y-variate-of iv)
              :function (y-func-of iv)
              :transform (y-transform-of iv)
              :coords (mapcar #'first (y-coords-of iv)))))

     ((typep iv '2d-view)
      (if (eq axis :horizontal)
        (list :var (x-variate-of iv)
              :function (x-func-of iv)
              :transform (x-transform-of iv)
              :coords (x-coords-of iv))
        (list :var (y-variate-of iv)
              :function (y-func-of iv)
              :transform (y-transform-of iv)
              :coords (y-coords-of iv))))

       ((and (typep iv '1d-view) (eq iv-axis axis))
        (list :var (variate-of iv)
                :function (func-of iv)
                :transform (transform-of iv)
                :coords (coords-of iv)))

       
       (t (list :var nil
                :function nil
                :transform nil))))))


(defmethod construct-margin-views ((self plot) 
                                   &key  (delete-useless-axes? t)
                                   link-bounds-x?
                                   link-bounds-y?)
  "Constructs margin views using information provided by~
   the :left-view :right-view :bottom-view :top-view initargs.~
   Each  initarg should pass the legal-view-construct-p test,~
   or be a list whose elements pass the legal-view-construct-p test.~
   If an initarg is null, the corresponding view is not constructed.~
   The default view type is axis. ~
   Each legal-view-construct, together with keyword-pairs,~
   is used to construct a view which is placed in the appropriate slot."
  
  (with-accessors ((lv  left-views-of) (rv right-views-of)
                   (tv top-views-of) (bv bottom-views-of)
                   (iv interior-views-of)
                   (vo viewed-object-of))
                  self
    (multiple-value-setq (lv rv bv tv)
      (values-list
       (loop for v in (list lv rv bv tv)
             for axis in (list :vertical :vertical :horizontal :horizontal)
             for just in (list :right :left :top :bottom )
             for coord-info = (plot-margin-coord-info self axis)
             for br-info = (append (list :data vo :orientation axis :justification just)
                                     (plot-margin-br-info self axis))
             
             collect
             
             (cond ((null v) nil)
                   ((legal-view-construct-p v)
                    (list (margin-view-from-arg v br-info coord-info 'axis)))
                   ((and (listp v) (every #'legal-view-construct-p v) )
                    (loop for vi in v collect  (margin-view-from-arg vi br-info coord-info 'axis)))
                   (t (quail-error "Illegal subview argument ~A" v))))))
   
    (constrain-bounds self :link-bounds-x? link-bounds-x? 
                      :link-bounds-y? link-bounds-y?)
     (if delete-useless-axes? (delete-useless-axes self))
    (loop with x-views = (append bv tv) with y-views = (append lv rv)
          for vi in iv
          when (typep vi 'histogram-view) do
          (if (eq :horizontal (orientation-of vi))
            (loop for v in x-views
                  when (typep v 'axis) do
                  (setf (tic-list-of v) (break-points-of vi)))
            (loop for v in y-views
                  when (typep v 'axis) do
                  (setf (tic-list-of v) (break-points-of vi)))))))


(defmethod compute-interior-region ((self plot)
                                   &key left-view-size right-view-size 
                                   bottom-view-size top-view-size
                                   bottom-label-size top-label-size
                                   left-label-size right-label-size
                                    xy-ratio )
  "Computes the region to be occupied by the interior view"
  (with-bordered-plot-subs 
    self iv lv rv bv tv ll rl bl tl
    (declare (ignorable iv))
    (let* ((space (subview-position-region self))
           y-view-len x-view-len x-mid  y-mid
           interior-region)
 
      (setq y-view-len (- (height-of space)
                          (if bv bottom-view-size 0.0) (if tv top-view-size 0)
                          (if bl bottom-label-size 0) (if tl top-label-size 0)))
      
      (setq x-view-len (- (width-of space)
                               (if lv left-view-size 0.0) (if rv right-view-size 0 )
                               (if ll left-label-size 0 ) (if rl right-label-size 0)))
                          
      (setq y-mid (+ (bottom-of space) (if bv bottom-view-size 0 )
                     (if bl bottom-label-size  0) (* 0.5 y-view-len)))
      (setq x-mid (+ (left-of space) (if ll left-label-size 0)
                     (if lv left-view-size 0 ) (* 0.5 x-view-len)))
      (when xy-ratio
        (setq y-view-len (min y-view-len (/ x-view-len xy-ratio)))
        (setq x-view-len (* xy-ratio y-view-len )))

      (setq interior-region
            (make-region (- x-mid (* .5 x-view-len)) (+ x-mid (* .5 x-view-len))
                         (- y-mid (* .5 y-view-len)) (+ y-mid (* .5 y-view-len))))
      (valid-region-check interior-region)
      interior-region)))


(defmethod init-position-margin-labels ((self plot) outer-space inner-space
                                 &key bottom-label-size top-label-size
                                 left-label-size right-label-size gap-x gap-y)
  (with-accessors ((ll  left-labels-of) (rl right-labels-of)
                   (tl top-labels-of) (bl bottom-labels-of))
                  self
    (multiple-value-bind (li ri bi ti)
                         (bounds-of inner-space)
      (multiple-value-bind (lo ro bo to)
                           (bounds-of outer-space)
        
        (if ll
          (loop for lli in ll
                for l-region in 
                (tile-region-list 
                 (make-region lo (+ lo left-label-size)  bi ti)
                 (length ll) 1 nil 0 gap-y) do
                (place-subview self lli l-region)))
        
        (if rl
          (loop for rli in rl
                for l-region in 
                (tile-region-list 
                 (make-region (- ro right-label-size) ro bi ti)
                 (length rl) 1 nil 0 gap-y) do
                (place-subview self rli l-region)))
        
        (if bl
          (loop for bli in bl
                for l-region in 
                (tile-region-list 
                 (make-region li ri bo (+ bottom-label-size bo))
                 1 (length bl) nil gap-x 0 ) do
                (place-subview self bli l-region)))
        (if tl
          (loop for tli in tl
                for l-region in 
                (tile-region-list 
                 (make-region  li ri (- to top-label-size ) to)
                 1 (length tl) nil gap-x 0 ) do
                (place-subview self tli l-region)))))))


(defmethod init-position-margin-views ((self plot) outer-space inner-space
                                       &key bottom-view-size top-view-size
                                       left-view-size right-view-size )
  (declare (ignore outer-space))
  (with-accessors ((lv  left-views-of) (rv right-views-of)
                   (tv top-views-of) (bv bottom-views-of))
                  self
    (multiple-value-bind (li ri bi ti)
                         (bounds-of inner-space)
      
      (if lv
        (loop for lvi in lv 
              for mv-region =
              (make-region (- li left-view-size) li bi ti) do
              (place-subview self lvi mv-region)))
      
      (if rv
        (loop for rvi in rv
              for mv-region = 
              (make-region ri (+ ri right-view-size) bi ti) do
              (place-subview self rvi mv-region)))
      
      (if bv
        (loop for bvi in bv
              for mv-region = 
              (make-region li ri (- bi bottom-view-size) bi) do
              (place-subview self bvi mv-region)))
      
      (if tv    
        (loop for tvi in tv
              for mv-region = 
              (make-region li ri ti (+ ti top-view-size)) do
              (place-subview self tvi mv-region))))))

(defmethod init-position-margin-views :after ((self plot) outer-space inner-space &key)
  (declare (ignore outer-space inner-space))
  (hide-useless-axes self))


(defmethod init-position-interior-views ((self plot) space &key)
  (loop for v in (interior-views-of self) do
        (place-subview self v (make-region space))))
      


(defmethod constrain-bounds ((self plot) &key draw? link-bounds-x? link-bounds-y? recompute?)
  (if (and link-bounds-x? (or (top-views-of self) (bottom-views-of self))
           )
    
    (loop for views in (case link-bounds-x?
                         (:by-row (row-views self))
                         (:by-col (col-views self))
                         (:by-block-row (merge-rows self))
                         (:by-block-col (merge-cols self))
                         (t (list (apply #'append (row-views self)))))
          for x-views = (loop for v in views
                              when (linkable-bounds-x-p v) collect v)
          for d-views = (loop for v in x-views
                              when (and (typep v 'd-view) (use-x-axis-p v)) collect v)
          when (> (length x-views) 1)
          do
          (link-view-bounds x-views :x)
          (set-view-extents x-views :x :recompute? recompute?
                            :region (if d-views (maximize-x-extents d-views)) )))
  (if (and link-bounds-y? (or (left-views-of self) (right-views-of self)))
    (loop for views  in (case link-bounds-y?
                          (:by-row (row-views self))
                          (:by-block-row (merge-rows self))
                          (:by-block-col (merge-cols self))
                          (:by-col (col-views self))
                          (t (list (apply #'append (row-views self)))))
          for y-views = (loop for v in (flatten-views views)
                              when (linkable-bounds-y-p v) collect v)
          for d-views = (loop for v in y-views 
                              when (and (typep v 'd-view) (use-y-axis-p v)) collect v)
          when (> (length y-views) 1)
          do
          (link-view-bounds y-views :y)
          (set-view-extents y-views :y :recompute? recompute?
                            :region (if d-views (maximize-y-extents d-views)) )))
  (when (or link-bounds-x? link-bounds-y?)
    (remap-to-viewports self :erase? draw? :draw? draw?)))



(defmethod enlist-views ((self plot) view)
  (if (listp view) view (list view)))

(defmethod set-aspect-ratio ((self plot) 
                             &key viewport (ratio 1) (draw? t))

  ;; an x unit / y unit  becomes ratio for interior view and margin views
  (setq viewport (or viewport (car (viewports-of self))))
  (when viewport
    (if draw? (erase-view self :viewport viewport))
    (let*  ((iv (interior-view-of self))
          (vp-iv (select-viewport iv viewport))
            (tr (select-map-to-viewport self viewport))
            (max-vp (apply-transform tr (subview-position-region self)))
            
            (br (bounding-region-of iv))
            (s (min (/ (width-of max-vp) (width-of br))
                    (* ratio (/ (height-of max-vp) ( height-of br)))))
            (new-w (* s (width-of br)))
            (new-h (/ (* s (height-of br)) ratio))
            (new-loc ))
        (set-viewport-width-height max-vp new-w new-h)
        
        (setq new-loc (apply-transform 
                      (invert-transform tr) 
                      max-vp))
      (loop for v in (interior-views-of self) do
            (place-subview self v (make-region new-loc   )))
      (when (left-views-of self)
      (loop with views = (left-views-of self)
            with region = ( select-sub-view-locn self (car views))
            for v in views
            for mv-region = 
            (make-region 
             (left-of region) (right-of region)
             (bottom-of new-loc) (top-of new-loc))
            do
            (place-subview self v mv-region)))
      (when (right-views-of self)
      (loop with views = (right-views-of self)
            with region = ( select-sub-view-locn self (car views))
            for v in views
            for mv-region = 
            (make-region 
             (left-of region) (right-of region)
             (bottom-of new-loc) (top-of new-loc))
            do
            (place-subview self v mv-region)))
      (when (bottom-views-of self)
      (loop with views = (bottom-views-of self)
            with region = ( select-sub-view-locn self (car views))
            for v in views
            for mv-region =
            (make-region 
             (left-of new-loc) (right-of new-loc)
             (bottom-of region) (top-of region))
            do
            (place-subview self v mv-region)))
      (when (top-views-of self)
      (loop with views = (top-views-of self)
            with region = ( select-sub-view-locn self (car views))
            for v in views
            for mv-region = 
            (make-region 
                                 (left-of new-loc) (right-of new-loc)
                                 (bottom-of region) (top-of region))
            do
            (place-subview self v mv-region)))

      (map-subviews-to-viewport   self )
      (if draw? (draw-view self)))))




(defmethod add-to-named-slot  ((self plot) old new)
  
    (cond ((member old (interior-views-of self))
           (setf (interior-views-of self)
                 (append (interior-views-of self) (list new))))
          ((member old (left-views-of self))
           (setf (left-views-of self)
                 (append (left-views-of self) (list new))))
          ((member old (right-views-of self))
           (setf (right-views-of self)
                 (append (right-views-of self) (list new))))
          ((member old (bottom-views-of self))
           (setf (bottom-views-of self)
                 (append (bottom-views-of self) (list new))))
          ((member old (top-views-of self))
           (setf (top-views-of self)
                 (append (top-views-of self) (list new))))
          (t nil)))
            
(defmethod delete-subview :before ((self plot) view)
  (cond ((member view (interior-views-of self))
           (setf (interior-views-of self)
                 (delete view (interior-views-of self))))
          ((member view (left-views-of self))
           (setf (left-views-of self)
                 (delete view (left-views-of self))))
          ((member view (right-views-of self))
           (setf (right-views-of self)
                 (delete view (right-views-of self))))
          ((member view (bottom-views-of self))
           (setf (bottom-views-of self)
                 (delete view (bottom-views-of self))))
          ((member view (top-views-of self))
           (setf (top-views-of self)
                 (delete view (top-views-of self))))
          ((member view (left-labels-of self))
           (setf (left-labels-of self)
                 (delete view (left-labels-of self))))
          ((member view (right-labels-of self))
           (setf (right-labels-of self)
                 (delete view (right-labels-of self))))
          ((member view (bottom-labels-of self))
           (setf (bottom-labels-of self)
                 (delete view (bottom-labels-of self))))
          ((member view (top-labels-of self))
           (setf (top-labels-of self)
                 (delete view (top-labels-of self))))
          ((eq view (title-of self)) (setf (title-of self) nil))
          (t nil)))

(defmethod change-variable ((self plot) &rest keyword-args 
                            )
  (let ((iv (interior-view-of self)))
    (if (typep iv 'd-view)   
      (apply #'change-variable-when-vars iv keyword-args)
      (if (compute-applicable-methods  #'change-variable  (list iv) )
        (apply #'change-variable iv keyword-args)))))

(defmethod add-fitted-line ((self plot) &rest keyword-args )
                           
  (apply #'add-fitted-line (interior-view-of self) keyword-args))
  

(defmethod add-lines ((self plot) &rest keyword-args)
  (apply #'add-lines (interior-view-of self) keyword-args))

(defmethod add-simple-lines ((self plot) &rest keyword-args)
  (apply #'add-simple-lines (interior-view-of self) keyword-args))

(defmethod add-smooth ((self plot) &rest keyword-args)
  (apply #'add-smooth (interior-view-of self) keyword-args))

(defmethod add-simple-smooth ((self plot) &rest keyword-args)
  (apply #'add-simple-smooth (interior-view-of self) keyword-args))


(defmethod change-case-status ((self plot)  cases status  &rest args  )
  (let ((iv (interior-view-of self)))
    (if (compute-applicable-methods #'change-case-status  
                                    (list iv  cases status args))
      (apply #'change-case-status iv cases status args))))

(defmethod activate-cases ((self plot)  cases  &rest args &key )
  (let ((iv (interior-view-of self)))
    (if (compute-applicable-methods #'activate-cases  
                                    (list iv  cases args))
  (apply #'activate-cases iv cases  args))))

(defmethod deactivate-cases ((self plot)  cases  &rest args &key )
  (let ((iv (interior-view-of self)))
    (if (compute-applicable-methods #'deactivate-cases  
                                    (list iv  cases args))
  (apply #'deactivate-cases iv cases  args))))


(defmethod activate-all-cases ((self plot)    &rest args &key )
  (let ((iv (interior-view-of self)))
    (if (compute-applicable-methods #'activate-all-cases  (list iv args) )
      (apply #'activate-all-cases iv args))))




(defmethod menu-properties ((self plot) 
                            (slot-name (eql 'middle-menu)))
  (let ((iv (interior-view-of self)))
    (if iv
      (acons :iv-type (class-name (class-of iv))
              (if (typep iv 'data-menu-item-mixin)
                (copy-tree (menu-properties iv 'middle-menu )))))))





(defmethod get-menu-items ((self plot) (slot-name (eql 'middle-menu)))
  (let ((iv (interior-view-of self))
        menu-items
        var-menu
        case-menu)
    (when (typep iv 'data-menu-item-mixin)
      (setq var-menu (var-menu-items (interior-view-of self) 
                                     :target 'interior-view-of))
      (setq case-menu (case-menu-items (interior-view-of self) 
                                       :target 'interior-view-of))
      (if var-menu
         (setq menu-items 
              `(("Variables" nil "" :sub-items ,var-menu))))
      (if case-menu
        (setq menu-items 
              (append menu-items 
                      `(("Cases" nil "" :sub-items  ,case-menu))))
        
        ))
    (setq menu-items 
              (append menu-items 
                      `(("Point symbols" nil "" :sub-items  ,(ps-subview-menu-items)))))

     (unless (string-equal "-" (caar menu-items))
        (push '("-" nil) menu-items))
    menu-items
    ))



(defun ps-subview-menu-items()
  (labels ((get-ps(self)
             (let* ((point-clouds (descendant-views-of-type self 'one-per-case-mixin))
                   (ps (or (loop for p in point-clouds append (some-subviews p :highlit? t))
                           (loop for p in point-clouds append (subviews-of p)))))
               ps))
                 
           (set-ps-colours(self color)
             (if (eq color :prompt)
               (setq color (wb::prompt-user-for-color)))
             (with-update-style-cache
             (loop for s in (get-ps self)
                   do  (set-drawing-style s :color color))))
         (set-ps-fill(self fill) 
           (with-update-style-cache
           (loop for s in (get-ps self)
                 do   (set-drawing-style s :fill? fill))))
         
         (set-ps-size(self size)
           (with-update-style-cache
           (loop for s in (remove-duplicates (get-ps self))
                 when (typep s 'view-with-size)
                 do (set-view-size   s size))))
         
         (set-ps-symbol(self symbol)
            (loop for s in (get-ps self)
                 do  (set-drawing-style s :symbol symbol )))
         
         )
    
    (let ((color-change-list
           (loop for c in 
                 (if (wb:color-device-p)
                   *color-menu-list*
                   *shade-menu-list*)
                 collect 
                 (list (car c) (list #'set-ps-colours  (cadr c)))))
          ( shape-change-list
            (loop for s in *point-symbol-types* 
                  collect
                  (list (string-downcase s)  (list #'set-ps-symbol  s))))
          ( size-change-list
            (loop for s in (list :smaller  :larger )
                  collect
                  (list (string-downcase s)  (list #'set-ps-size s))))
          ( fill-change-list
            (list (list "On" (list #'set-ps-fill t))
            (list "Off" (list #'set-ps-fill nil))))
            )
      (list (list "Colors" nil "" :sub-items color-change-list)
            (list "Shape" nil "" :sub-items shape-change-list)
            (list "Size" nil "" :sub-items size-change-list)
            (list "Fill?" nil "" :sub-items fill-change-list)))))
         
(defmethod hide-useless-axes ((self plot)) 
  (loop for sv in (flatten-views (append (left-views-of self) 
                          (right-views-of self)
                          (bottom-views-of self)
                          (top-views-of self)))
        when (and (typep sv 'axis)
                  (hide-axis-p sv))
        do
        (set-drawing-style sv :invisible? t)))










(defmethod delete-useless-axes ((self plot)) 
 (setf  (left-views-of self) 
        (loop for sv in (left-views-of self)
        unless (and (typep sv 'axis)
                  (hide-axis-p sv))
        collect sv))
 (setf  (right-views-of self) 
        (loop for sv in (right-views-of self)
        unless (and (typep sv 'axis)
                  (hide-axis-p sv))
        collect sv))
 (setf  (bottom-views-of self) 
        (loop for sv in (bottom-views-of self)
        unless (and (typep sv 'axis)
                  (hide-axis-p sv))
        collect sv))
 (setf  (top-views-of self) 
        (loop for sv in (top-views-of self)
        unless (and (typep sv 'axis)
                  (hide-axis-p sv))
        collect sv)))







(defmethod margin-string-bottom ((self view))
  (let* ((s (coord-string-x self)))
    (if (listp s) s (list s))))

(defmethod margin-string-top ((self view))
  (let* ((s (coord-string-x self)))
    (if (listp s) s (list s))))

(defmethod margin-string-left ((self view))
  (let* ((s (coord-string-y self)))
    (if (listp s) s (list s))))

(defmethod margin-string-right ((self view))
  (let* ((s (coord-string-y self)))
    (if (listp s) s (list s))))

;;------------------------------------------------------------------------------

(defmethod margin-string-top ((self plot)) 
  (let* ((iv (interior-view-of self))
         (s (if iv (margin-string-top iv))))
    (if (listp s) s (list s))))

(defmethod margin-string-bottom ((self plot)) 
  (let* ((iv (interior-view-of self))
         (s (if iv (margin-string-bottom iv))))
    (if (listp s) s (list s))))

(defmethod margin-string-left ((self plot)) 
  (let* ((iv (interior-view-of self))
         (s (if iv (margin-string-left iv))))
    (if (listp s) s (list s))))

(defmethod margin-string-right ((self plot)) 
  (let* ((iv (interior-view-of self))
         (s (if iv (margin-string-right iv))))
    (if (listp s) s (list s))))


(defmethod initialize-instance :before ((self plot)
                                        &key margin-string-left margin-string-right 
                                        margin-string-top margin-string-bottom)
  (if margin-string-left
    (defmethod margin-string-left ((view (eql self)))
      (if (functionp margin-string-left)
        (funcall margin-string-left view) margin-string-left)))
  (if margin-string-right
    (defmethod margin-string-right ((view (eql self)))
      (if (functionp margin-string-right)
        (funcall margin-string-right view) 
        margin-string-right)))

  (if margin-string-bottom
    (defmethod margin-string-bottom ((view (eql self)))
      (if (functionp margin-string-bottom)
        (funcall margin-string-bottom view) 
        margin-string-bottom)))

  (if margin-string-top
    (defmethod margin-string-top ((view (eql self)))
      (if (functionp margin-string-top)
        (funcall margin-string-top view) 
        margin-string-top))))


(defmethod row-format ((self plot))
  (with-accessors ((lv  left-views-of) (rv right-views-of)
                   (tv top-views-of) (bv bottom-views-of)
                   (iv interior-views-of))
                  self

  (let (row1 row2 row3)
    (setq row1 (if tv (list tv)))
    (setq row2 (remove nil (list lv iv rv)))
    (setq row3 (if bv (list bv)))
    (remove nil (list row1 row2 row3)))))
  

(defmethod col-format ((self plot))
  (with-accessors ((lv  left-views-of) (rv right-views-of)
                   (tv top-views-of) (bv bottom-views-of)
                   (iv interior-views-of))
                  self

  (let (col1 col2 col3)
    (setq col1 (if lv (list lv)))
    (setq col2 (remove nil (list tv iv bv)))
    (setq col3 (if rv (list rv)))
    (remove nil (list col1 col2 col3)))))




#|
(defmethod row-views ((self plot))
  (with-accessors ((lv  left-views-of) (rv right-views-of)
                   (tv top-views-of) (bv bottom-views-of)
                   (iv interior-views-of))
                  self
    (cond ((not (or lv bv tv lv rv))
           (or (if (= 1 (length iv)) (row-views (car iv)))
               (if iv (list iv))))
          
          (t 
           (remove nil (list tv (append lv iv rv) bv))))))

(defmethod col-views ((self plot))
  (with-accessors ((lv  left-views-of) (rv right-views-of)
                   (tv top-views-of) (bv bottom-views-of)
                   (iv interior-views-of))
                  self
    (cond ((not (or lv bv tv lv rv))
           (or (if (= 1 (length iv)) (col-views (car iv)))
               (if iv (list iv))))
          
          (t 
           (remove nil (list lv (append tv iv bv) rv))))))
    
|#

(defgeneric row-views (view)
  (:documentation "Returns lists of descendant views in view with ~
                   sublist i containing views in row i."))

(defgeneric col-views(view)
  (:documentation "Returns lists of descendant views in view with ~
                   sublist i containing views in column i."))


 
(defmethod row-views ((self plot))
  (with-accessors ((lv  left-views-of) (rv right-views-of)
                   (tv top-views-of) (bv bottom-views-of)
                   (iv interior-views-of))
                  self
    (cond ((not (or lv bv tv lv rv))
           (list (loop for v in iv
                 append (car (row-views v)))))
          
          (t 
           (remove nil (list tv 
                             (append lv 
                                     (loop for v in iv
                                           append (car (row-views v)))
                                     rv)
                             bv))))))

(defmethod col-views ((self plot))
  (with-accessors ((lv  left-views-of) (rv right-views-of)
                   (tv top-views-of) (bv bottom-views-of)
                   (iv interior-views-of))
                  self
    (cond ((not (or lv bv tv lv rv))
           (list (loop for v in iv
                 append (car (col-views v)))))
          
          (t 
           (remove nil (list lv 
                             (append tv 
                                     (loop for v in iv
                                           append (car (col-views v)))
                                     bv)
                             rv))))))



(defmethod col-views ((self view))
  (list (list self)))

(defmethod row-views((self view))
  (list (list self)))


#|
(defmethod merge-rows ((self plot))
  (loop with all = (loop for views in (row-format self)
                         collect (mapcar #'row-views (reduce #'append views)))

        with n = (loop for v in all
                       maximize (apply #'max (mapcar #'length v)))
        with new = (make-list n )
        for views in all
        do      (loop for vi in views do
                      (loop for vij in vi
                            for j upfrom 0
                            do  (setf (nth    j new) (append (nth j new) vij))))
        finally (return new)))


 


(defmethod merge-cols ((self plot))
 (loop with all = (loop for views in (col-format self)
                         collect (mapcar #'col-views (reduce #'append views)))
        with n = (loop for v in all
                       maximize (apply #'max (mapcar #'length v)))
        with new = (make-list n )
        for views in all
        do      (loop for vi in views do
                      (loop for vij in vi
                            for j upfrom 0
                            do  (setf (nth    j new) (append (nth j new) vij))))
        finally (return new)))

|#

(defmethod merge-rows ((self plot))
  (row-views self))

(defmethod merge-cols ((self plot))
  (col-views self))



