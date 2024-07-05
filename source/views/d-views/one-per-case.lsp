;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               one-per-case.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '( one-per-case-mixin   
            collect-views-in-region justified-line)))
;;;----------------------------------------------------------------------------------
(defclass one-per-case-mixin (draggable-brush-view-mixin pass-draws-to-subviews)
  ((middle-menu :allocation :class :initform nil)
   (style-keys :initform '(:size :fill? :font :color) :allocation :class)
  )
  (:default-initargs :size *default-point-size*
    :font *default-label-font*
    :fill? *default-point-fill?*))
 

                    


(defgeneric collect-views-in-region (one-per-case-mixin &key viewport region)
  (:documentation "Select  case views in region.~
                  If region is nil, create using mouse. "))
  
(defclass justified-line (justification-mixin oriented-line )
  ((middle-menu :allocation :class :initform nil))
  (:default-initargs :justification :bottom :justification-menu? t))

(defmethod set-justification ((self justified-line) justification &key (draw? nil) )
  (if draw? (erase-view self))
  (setf (justification-of self) justification)
  (compute-line-endpoints self)
  
  (loop for vp in (viewports-of self)
        for p = (find-parent self :viewport vp)
        when p do
        
        (map-subviews-to-viewport p nil (list self)))
        (if draw? (draw-view self)))

(defmethod list-legal-justifications ((self justified-line))
  (list :left :right :left-right :bottom :top :top-bottom))  

#| Already in views/views-generl/view-ops.lsp .. email from rwo 25SEP2023
(defmethod identify-view ((self view) 
                          &key viewport )
  (setq viewport (or viewport (car (viewports-of self))))
  (select-one self)
   (let ((s (viewed-object-description-string self))
        (parent (find-parent self :viewport viewport)))
    (if parent
      (setq s
            (format nil "~A: At ~A"
                         s (subview-location-string  parent self)
                         )))
    (quail-print s)))
    |#

(defmethod orientation-of ((self justified-line))
  (case (justification-of self)
    (:left :horizontal)
    (:right :horizontal)
    (:top :vertical)
    (t :vertical)))

;;;----------------------------------------------------------------------------------



(defmethod construct-sub-views ((self one-per-case-mixin) &rest initargs
                                &key case-view ordered-case-views? (linkable-case-views? t)
                                case-views-from (labels #'identifier-name) colors )
  (if (member labels (variates-of self) :test #'eq-variate-exprs)
    (setq labels (case-access-fn self labels)))
  (let* ((subview-arglist (subview-arg-list case-view 'point-symbol))
         (dummy (apply #'view  subview-arglist))
         (sub-type (class-name (class-of dummy)))
         (vo  (cases-of self)))
    (when case-view
      (loop for key in (slot-value self 'style-keys)
            for hold = (getf subview-arglist key :xxx) 
            unless (eq hold :xxx) do
            (set-draw-style self key hold)))

   (if (typep case-views-from 'view)
      (setf case-views-from 
            (descendant-views-of-type 
             case-views-from sub-type))
      )
   (unless (listp case-views-from)
     (setq case-views-from nil))
   (unless (= (length vo) (length case-views-from))
     (setq ordered-case-views? nil))
   (flet ((get-subview(case i)
            (if (and ordered-case-views? case-views-from)
              (nth i case-views-from)
              (or (loop for cv in case-views-from
                        for case-cv = (viewed-object-of cv)
                        thereis (and (eq-dataset case case-cv)
                                     cv))
                  (and (listp case)
                       (loop for cv in case-views-from
                             for case-cv = (viewed-object-of cv)
                             thereis (and  (listp case-cv)
                                           (= (length case) (length case-cv))
                                           (every #'eq-dataset case case-cv)
                                           cv)))))))
      (unless (subviews-of self)
        (setf  (subviews-of self)
               (loop with sub-styles
                    with first-col with new-sub
                      for case in vo
                     for i upfrom 0
                     for l in (if (listp labels) labels
                                  (make-list (length vo) :initial-element labels))
                     for old-sub = (get-subview case i)
                     collect 
                     (cond ((typep old-sub sub-type)
                            old-sub)
                           (t (setq new-sub (apply #'view  :data case 
                                    :left-fn '(identify-view :viewport)
                                  ;;  :label l :text l :linkable? t
                                    (append subview-arglist (list :label l :text l :linkable? linkable-case-views?))))
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
                        (if (and old-sub (link-table-of old-sub))
                          (fast-link2-views  old-sub new-sub :draw? nil :link-table (link-table-of old-sub)))
                        new-sub))  ))))))



(defmethod init-position-subviews ((self one-per-case-mixin) &key )
  (let* ((br (subview-position-region self))
         (subs (subviews-of self))
         
         (wid/2 (/ (width-of br)  20))
         (ht/2 (/ (height-of br)  20))
         (coords (plot-coords-of self ))
         (status (case-status-of self))
         ) 
        
    (setf (sub-view-locns-of self)
          (loop 
            for (x y) in coords
            for s in status
            for sv in subs
            for region = 
            (cond ((invalid-status-p s) 
                   (make-region))
                  #|
                  ((typep sv 'justified-line) 
                   (case (justification-of sv)
                     (:left (make-region (left-of br) x
                                         (- y ht/2)  (+ y ht/2)))
                     (:right (make-region x (right-of br) 
                                          (- y ht/2)  (+ y ht/2)))
                     (:top (make-region  (- x wid/2) (+ x wid/2)
                                         y (top-of br)))
                     (t  (make-region (- x wid/2) (+ x wid/2)
                                      (bottom-of br) y))))
                  ((typep sv 'oriented-line)
                   (if  (eql (orientation-of sv) 'horizontal)
                     (make-region (left-of br) x (- y ht/2)  (+ y ht/2))
                     (make-region (- x wid/2) (+ x wid/2) (bottom-of br) y)))
                  |#
                  (t (make-region 
                      (- x wid/2) (+ x wid/2)
                      (- y ht/2) (+ y ht/2))))
            collect region))))

(defmethod select-subview-status ((self one-per-case-mixin) subview)
  (let ((p (position subview (subviews-of self))))
    (if p  (elt (case-status-of self)  p))))




(defmethod compute-sub-viewports ((self one-per-case-mixin)
                                  &optional viewport subviews)
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
           (case-status-of self)))
        sv-vp)
    (setq subviews (or subviews (subviews-of self)))
    
    (loop with left and right and bottom and top
          for sv in subviews
          for svl in subview-locns 
          for s in status
          for sv-rad  = (cond ((typep sv 'view-with-size)
                               (view-size-of sv))
                              ((typep sv 'fixed-size-rectangle)
                               (list (rectangle-width-of sv) 
                                     (rectangle-height-of sv)))
                              (T 5))
           do
          (loop for vp in viewports for map in  maps
                 for xy = (apply-transform map (centre-of svl))
                 for x = (2d-position-x xy)
                 for y = (2d-position-y xy)
                 for width = (if (listp sv-rad) (car sv-rad) sv-rad)
                 for height = (if (listp sv-rad) (cadr sv-rad) sv-rad)
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
                         (when (> rad width) ;or height which?
                           
                           (if (eql (orientation-of sv) :horizontal)
                             (setq left (- x rad)
                                   right (+ x rad)
                                   )
                             (setq bottom (- y rad)
                                   top (+ y rad)
                                   )))))

                    

                   (t nil))
                
                (setq sv-vp (make-viewport vp-win left right bottom top))
                                  
                                     
                
                (unless (active-status-p s) (deactivate-viewport sv-vp))
                (add-viewport sv sv-vp vp)))))


(defmethod distance-to-location ((self one-per-case-mixin) viewport location)
  
  (if (selected-p self viewport location)
    (let ((locn-c (if (region-p location)
                    (centre-of location) location)))
       (loop with dist
                with mdist = (* 2 (radius-of  viewport))
                for sv in (subviews-of self)
                for s in (case-status-of self)
                for sv-vp = (select-viewport sv viewport)
                when (active-status-p s) do
                (setq dist (distance-from sv-vp locn-c))
                (setq mdist
                      (min mdist dist))
                finally (return mdist)
          ))
    
    10000))

(defmethod  get-subview-size  ((self one-per-case-mixin) new)
  (let* ((old (draw-style self :size))
        (sub (car (subviews-of-type self 'view-with-size)))
        (inc
         (if sub 
           (progn
             (setf (view-size-of sub) old)
             (view-size-increment sub))
           (if (>= old 3) 2 1))))
    (case new
      (:larger (+  old inc)) 
      (:smaller (-  old inc))
      (:prompt (wb:prompt-user :result-type 'number 
                               :read-type :eval
                               :prompt-string 
                               (format nil "Change size from ~A" old)))
      (t old))))


(defmethod set-subview-size ((self one-per-case-mixin) new &key (draw? t) highlit?)
  (unless (numberp new)
    (setq new (get-subview-size self new)))
  (set-draw-style self :size new)
  (loop for v in (some-subviews self :highlit? highlit?) do 
        (if (typep v 'view-with-size) 
          (set-view-size v new :draw? draw?)
          (loop for vp in (viewports-of v) 
                for vp-copy = (copy-region vp)
                do
                (set-viewport-size vp-copy (+ 2 new))
                (reshape-viewport v vp :new-location 
                                  vp-copy :draw? t)))))




(defmethod set-invisible-subviews ((self one-per-case-mixin) new &key (draw? t) highlit?)
   (loop for v in (some-subviews self :highlit? highlit?) do 
        (set-drawing-style v :invisible? new :draw? draw?)))

(defmethod change-subview-class ((self one-per-case-mixin) new &key (draw? t) highlit?)
  (loop for v in (some-subviews self :highlit? highlit?) do
        (if draw? (erase-view v))
        (change-class v new)
        (if draw? (draw-view v))))

(defmethod apply-choice-to-sub ((self one-per-case-mixin) highlit?
                                &rest choice)
  (loop for v in (some-subviews self :highlit? highlit?)
        do (apply-choice v choice)))

(defmethod bounds-menu-item-list ((self one-per-case-mixin))
  (list '("Blowup" (new-bounds :region :compute))
                  '("Original" (new-bounds :region :original))))

(defmethod get-menu-items ((self one-per-case-mixin) (slot-name (eql 'middle-menu)))
  (labels ((add-to-set-style(arg)
             (cond 
              ((not (listp arg))
               arg)
              ((eq (car arg) 'set-drawing-style) 
               (setf (cdr (last arg)) `(:highlit?  *selected-subviews-only*))
               arg)
            ;;  ((eq (car arg) 'set-color-style) 
            ;;   (setf (cdr (last arg)) `(:highlit?  *selected-subviews-only*))
            ;;   arg)
              ((and (symbolp (car arg))
                    (fboundp (car arg))
                    (not
                     (compute-applicable-methods  (symbol-function (car arg))
                                                  `(,self ,@(cdr arg)))))
               (push   *selected-subviews-only* arg)
               (push 'apply-choice-to-sub arg)
               
               arg)
              ((equal (car arg) "Size")
               nil)
              ((equal (car arg) "Invisible?")
               nil)
              (t (mapcar #'add-to-set-style arg)))
             ))
    (let* ((sv (car (subviews-of self)))
           (sv-items (if sv (add-to-set-style
                             (copy-tree
                              (get-menu-items sv 'middle-menu)))))
           (size (loop for key in '(:smaller :larger :prompt)
                       collect `(,(string-downcase key)
                                 (set-subview-size ,key 
                                                   :highlit? *selected-subviews-only*))))
                (classes
            (loop for (c s) in *simple-view-menu-list* 
                  collect
                  (list s  
                        `(change-subview-class  ,c :highlit?  *selected-subviews-only*))))
           (bounds
            (bounds-menu-item-list self)))
        
        `(,@sv-items
          ( "Toggle hi" (toggle-highlight ))
          ( "Size" nil "" :sub-items ,size)
          ( "Invisible?" (set-invisible-subviews t :highlit? *selected-subviews-only* ))
          ( "AllVisible?" (set-invisible-subviews nil :highlit? nil))
          ( "Class" nil "" :sub-items ,classes)
          ("-" nil)
          ( "Bounds" nil "" :sub-items ,bounds)
          ))))


(defmethod toggle-highlight((self one-per-case-mixin) )
  (loop for s in ( some-subviews self :highlit? nil)
        do
        (set-drawing-style s :highlight? :toggle)
        (if (any-highlight? s) (push s *selected-views*))
  ))

(defmethod update-menu-items ((self one-per-case-mixin) 
                              (slot-name (eql 'middle-menu)))
  (let* ((m (slot-value self slot-name)))
    (if (and m (has-draw-style-p self :fill?))
      (wb:check-menu-item m "Fill?" (draw-style self :fill?))
      )

    ))


(defmethod menu-properties ((self one-per-case-mixin ) 
                            (slot-name (eql 'middle-menu)))
  (let ((sv (car (subviews-of self))))
    (if sv
      (acons :point-type (class-name (class-of sv)) nil))))


(defmethod styles-to-subs ((self one-per-case-mixin ) ) 
  (list :highlight? :fill? :size :font :color ))




(defmethod drag-brush ((self one-per-case-mixin) 
                       &key viewport &allow-other-keys)
  ;; mouse is already depressed
  (brush-high self :viewport viewport))
    




(defmethod high-to-down ((self one-per-case-mixin) )
;; converts  highs to down
  (loop for sv in (subviews-of self) 
        for s in (case-status-of self)
        when (and (active-status-p s) (any-highlight?  sv))
        do (set-drawing-style sv :highlight? nil)))


(defmethod subview-highlight-styles ((self one-per-case-mixin))
  (loop for sv in (subviews-of self)
        for sv-style = (assoc :highlight? (car (drawing-styles-of  sv)))
        collect sv-style))




(defmacro with-moving-brush (brush viewport &body forms)
  (let ((move? (gensym))
        (x (gensym))
        (y (gensym)))
    
    `(let* ((,x (wb:mouse-x (window-of ,viewport)))
            (,y  (wb:mouse-y (window-of ,viewport)))
            (,move? (not (and (= ,x (brush-x ,brush)) (= ,y (brush-y ,brush)))))
            )
       (when ,move?
         (draw-brush ,brush ,viewport)
         ,@forms
         (draw-brush ,brush ,viewport ,x ,y)))))


(defmethod brush-high ((self one-per-case-mixin ) 
                       &key viewport)
  
  (let* ((bw (window-of viewport))
         (points (subviews-of self))
         (status (case-status-of self))
         (sv-vps (loop for sv in points
                       collect (select-viewport sv viewport)))
         (brush (brush-of self))
         (visible-sub (visible-subviews-p self))
         (f (brush-test-of self))
         (highlight-styles (subview-highlight-styles self))
         (mode  (brushing-mode-of self))
         
         )
    
    (draw-brush brush viewport (wb:mouse-x bw) (wb:mouse-y bw))
    (case  mode
      (:select
       (loop while (wb:mouse-down-p) 
              do
              (with-moving-brush brush viewport
                (loop for sv in points
                      for vis in visible-sub
                      for s in status
                      for vp in sv-vps 
                      for sv-style in highlight-styles
                      when (and vis (active-status-p s))  do
                      (if (not (funcall f sv brush vp))
                        (if (cdr sv-style)
                          (set-highlight-style sv nil))
                        (unless (cdr sv-style)
                          (set-highlight-style sv t))
                        )))))
      (:union
       (loop while (wb:mouse-down-p) 
               do
               (with-moving-brush brush viewport
                 (loop for sv in points
                       for vis in visible-sub
                       for s in status
                       for vp in sv-vps 
                       for sv-style in highlight-styles
                       when (and vis (active-status-p s))  do
                       (if (funcall f sv brush vp)
                         (unless (cdr sv-style)
                           (set-highlight-style sv t))
                         )))))
      (:intersection
       (loop while (wb:mouse-down-p) 
               do
               (with-moving-brush brush viewport
                 (loop for sv in points
                       for vis in visible-sub
                       for s in status
                       for vp in sv-vps 
                       for sv-style in highlight-styles
                       when (and vis (active-status-p s))  do
                       (if (not (funcall f sv brush vp))
                         (if (cdr sv-style)
                           (set-highlight-style sv nil))
                         )))))
       (:difference
       (loop while (wb:mouse-down-p) 
               do
               (with-moving-brush brush viewport
                 (loop for sv in points
                       for vis in visible-sub
                       for s in status
                       for vp in sv-vps 
                       for sv-style in highlight-styles
                       when (and vis (active-status-p s))  do
                       (if (funcall f sv brush vp)
                         (if (cdr sv-style)
                           (set-highlight-style sv nil))
                         )))))
       (t nil))
    (draw-brush brush viewport)
    (set-selected-views
           (loop for sv in points
                         for sv-style in highlight-styles
                         for vis in visible-sub
                       for s in status
                      
                         when (and vis (active-status-p s) (cdr sv-style))
                         collect sv))))




















               


 





(defmethod visible-subviews-p ((self one-per-case-mixin))
  
  (loop with br = (bounding-region-of self)
        for svl in (sub-view-locns-of self) 
        for s in (case-status-of self)
        collect (and (active-status-p s)
                         (intersects-p br svl)
                         t)))





(defmethod print-subview-coords ((self one-per-case-mixin) sub)
  
  (let ((coords (coords-of self :cases (list (viewed-object-of sub)))))
    (print (first coords))))






(defmethod bounds-of-selected ((self one-per-case-mixin))
  (loop  for sub in (subviews-of self)
             for status in (case-status-of self)
             for (x y)  in (plot-coords-of self)
             when (and (draw-style sub :highlight?) (eq status t))
             minimize x into x-min maximize x into x-max 
             minimize y into y-min maximize y into y-max 
             finally (if (= x-min x-max)
                       (incf x-max 0.01))
             (if (= y-min y-max)
               (incf y-max 0.01))
             (return (make-region x-min x-max y-min y-max))))



 


(defmethod new-bounds ((self one-per-case-mixin) &key (region :compute) pretty?)
  (unless (region-p region)
    (setq region
          (case region
            (:prompt
             (apply #'make-region
                    (wb:prompt-user :result-type 'list :read-type :read
                                    :prompt-string "(left right bottom top)")))
            (:original (original-bounds self :draw? t) nil)
            (:compute (bounds-of-selected self))
            (t NIL))))
  (if (region-p region) (change-bounding-region self region :pretty? pretty?)))



(defmethod new-case-status ((self one-per-case-mixin)  cases status  
                            &key rescale? draw? )
  (let ((vp-fn (if (ignore-status-p status )
                 #'(lambda(v vp)
                    (when (active-viewport-p vp)
                      (unless (or rescale? draw? )
                        (erase-view v :viewport vp))
                      (deactivate-viewport vp)))
                 #'(lambda(v vp)
                    (unless (active-viewport-p vp)
                      (activate-viewport vp)
                      (unless (or rescale? draw? )
                        (draw-view v :viewport vp))))))
        (viewports (viewports-of self))
        (case-status (case-status-of self)))
    
    (if (eq cases (cases-of self))
      (loop for s in case-status
            for sv in (subviews-of self)
            unless (invalid-status-p s) do
            (loop for vp in viewports do
                  (funcall vp-fn sv (select-viewport sv vp)))) 
      
      (loop for sv in (subviews-of self)
            for vo = (viewed-object-of sv)
            for s in case-status
            unless (invalid-status-p s) do
            (if (or (member vo cases :test #'eq-dataset)
                    (member (identifier-of vo) cases :test #'eq-identifiers)) 
              (loop for vp in viewports do
                    (funcall vp-fn sv (select-viewport sv vp))))))) )

(defmethod new-sub-views ((self one-per-case-mixin)  &key) 
  (let* ((status (case-status-of self))
         (viewports (viewports-of self)))
    (loop for sv in (subviews-of self)
          for s in status
          do
          (loop for vp in viewports 
                for vp-sub = (select-viewport sv vp) do
                (if (active-status-p s)
                  (activate-viewport vp-sub)
                  (deactivate-viewport vp-sub)) ))))
     




(defmethod   remove-subview ((self one-per-case-mixin) subview )
  (loop for vp in (viewports-of self) 
         for sv-vp = (select-viewport subview vp )
         do
         (erase-view subview :viewport sv-vp )
         (deactivate-viewport sv-vp)))

(defmethod delete-subview ((self one-per-case-mixin) subview )
  (loop for vp in (viewports-of self) 
         for sv-vp = (select-viewport subview vp )
         do
         (deactivate-viewport sv-vp)))



