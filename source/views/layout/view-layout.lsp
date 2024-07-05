;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               view-layout.lisp
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
;;;     C.B. Hurley 1992 George Washington University
;;;     
;;;
;;;
;;;------------------------------------------------


(in-package :views)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(view-layout default-layout-sub-views 
          batch-sub-views view-layout-position-subviews 1d-sub-views)))

(defgeneric default-layout-sub-views (view &key &allow-other-keys))


(defclass view-layout(data-menu-item-mixin boxed-subview-mixin compound-view ) 
  ((positions :initform nil :initarg :positions :accessor positions-of)
   (middle-menu :allocation :class :initform nil)
   (link-bounds-x? :initform nil :initarg :link-bounds-x? :accessor link-bounds-x-p)
   (link-bounds-y? :initform nil :initarg :link-bounds-y? :accessor link-bounds-y-p)
   (layout-views :initform nil :initarg layout-views :accessor layout-views-of)
   (subview-position-fn :initform #'view-layout-position-subviews  :initarg :subview-position-fn 
                        :accessor subview-position-fn-of))
  (:default-initargs :link-bounds-x? nil :link-bounds-y? nil
    :batches nil :subview-constructor nil
    :box-views? nil :nsubviews :prompt
    )
  (:documentation "A compound view for laying out subviews. ~
                   The user supplies views, or information to ~
                   construct views, and a  position for each view. ~
                   The position may be a function which when applied to ~
                   each view yields its position."))
 

(defmethod construct-sub-views ((self view-layout) &rest keyword-pairs 
                                &key subview-constructor batches)
  (setq subview-constructor
        (or subview-constructor
            (if batches #'batch-sub-views
                #'default-layout-sub-views)))
  (apply subview-constructor self  keyword-pairs))


(defmethod init-position-subviews ((self view-layout) &rest keyword-pairs &key )
  (let ((fn (subview-position-fn-of self)))
  (if (and fn (null (subview-locns-of self)))
      (apply fn self :allow-other-keys t keyword-pairs))))


  



(defmethod var-info ((self view) var-args)
  (setq var-args
        (or var-args
            (let ((d (cond ((or (typep self 'd-view) (typep self 'bar-chart)
                                (typep self 'pairs-layout)
                                (typep self '1d-layout))
                            self)
                           ((typep self 'plot)
                            (interior-view-of self))
                           ((typep self 'plot)
                            (interior-view-of self))
                           (t nil))))
              (if (null d)
                (setq d (car (descendant-views-of-type self 'd-view  ))))
                        
              
              (cond 
               ((null d) nil)
               ((typep d '2d-view)
                (list :x (x-variate-of d) :y (y-variate-of d)
                      :x-function (x-func-of d) :y-function (y-func-of d)
                      :x-transform (x-transform-of d) :y-transform (y-transform-of d)))
               ((typep d '1d-view)
                (list :var (variate-of d) :function (func-of d) 
                      :transform (transform-of d)))
               ((typep d '3d-view)
                (list :x (x-variate-of d) :y (y-variate-of d)
                      :z (z-variate-of d)
                      :x-function (x-func-of d) :y-function (y-func-of d)
                      :z-function (z-func-of d)
                      :x-transform (x-transform-of d) :y-transform (y-transform-of d)
                      :z-transform (z-transform-of d)))
               ((or (typep d 'pairs-layout) (typep d '1d-layout))
                (list :vars (vars-of d) :functions (funcs-of d)
                      :transforms (transforms-of d)))
               ((typep d 'd-view)
                (list :vars (vars-of d) :functions (funcs-of d)
                      :transforms (transforms-of d)))
               ((typep d 'bar-chart)
                (list :batches (if (functionp (batches-of d)) (batches-of d))
                      :by (by-vars-of d)))
               (t nil)))))
  (unless (getf  var-args :case-view)
  (let* ((c (cond ((typep self 'one-per-case-mixin)
                   self)
                  ((and (typep self 'plot) 
                        (typep (interior-view-of self) 'one-per-case-mixin))
                   (interior-view-of self))
                  (t nil)))
         cv)
    (if (null c)
      (setq c (car (descendant-views-of-type self 'one-per-case-mixin))))
                 
    (setq cv (if c (car (sub-views-of c))))
    
    (if cv
      (setq var-args 
            (append var-args
                    (list :case-view (class-name (class-of cv))))))))
    var-args)


  
          
                    

(defun repeat-list(list n)
  (if (and list (numberp n))
    (loop while (< (length list) n)
          do (setq list (append list list))
          finally (return (subseq list 0 n)))
    list))


(defmethod default-layout-sub-views ((self view-layout) 
                                     &key nsubviews subviews 
                                     subview-type 
                                     default-subview-type
                                     (common-vars? nil) (common-case-views? nil) 
                                     ordered-case-views? case-views-from
                                     (subview-superclass '(or simple-view compound-view)))
  
  (let* ((vo (viewed-object-of self))
         
         (case-args (list :case-views-from case-views-from 
                          :ordered-case-views? ordered-case-views?))
         smooth-info
         brush-info
         (sub-list (nconc case-args (list :data vo)))
         var-args)
    (setq case-views-from nil)
    (labels ((local-make-view(arg &optional (type default-subview-type)) 
               (let ((v
                      (if (view-p arg)
                        arg
                        (apply #'view 
                               (append (if (listp type)
                                         type
                                         (list :type type))
                                       
                                       (subview-arg-list arg default-subview-type)
                                       sub-list var-args)))))
                 (if common-vars?
                   (setq var-args (var-info v var-args)))
                 (when (and (null smooth-info) (typep v 'smooth-mixin))
                   (setq smooth-info (list :smooth-par (smooth-par-of v)))
                   (setq var-args (nconc smooth-info var-args)))
                 (when (and (null brush-info) (typep v 'brushable-view-mixin))
                   (setq brush-info (list :brush (brush-of v)))
                   (setq var-args (nconc brush-info var-args)))
                 (when (and common-case-views? (null case-views-from) )
                   (if  (typep v 'one-per-case-mixin)
                     (progn (setf (getf case-args :case-views-from) v)
                            (setf (getf case-args :ordered-case-views?) t))
                     (if  (and (typep v 'plot) (typep (interior-view-of v) 'one-per-case-mixin))
                       (progn  (setf (getf case-args :case-views-from) v)
                               (setf (getf case-args :ordered-case-views?) t)))))
                 
                 
                 
                 v))
             (local-make-views (arg types)
               (mapcar #'(lambda(type) (local-make-view arg type)) types)))
      
      
      (if (and (null nsubviews) (numberp subviews))
        (rotatef nsubviews subviews))
      (when (and (eq nsubviews :prompt) (numberp subviews))
        (setf nsubviews subviews)
        (setf subviews NIL))
      
      (setq subviews
            (cond ((and subviews (listp subviews))
                   (repeat-list subviews nsubviews))
                  ((and (numberp nsubviews) (> nsubviews 0) (null subview-type) (null default-subview-type)
                        (typep self 'grid-layout))
                   (let ((vt
                    (choose-view-from-menu :prompt-string  "Select layout view" 
                                              :superclass subview-superclass))) 
                    (make-list nsubviews :initial-element vt)))
                  ((and (numberp nsubviews) (> nsubviews 0) (null subview-type) (null default-subview-type))
                   (repeat-list 
                    (choose-views-from-menu :prompt-string  "Select layout views" 
                                            :nmax nsubviews
                                            :superclass subview-superclass) 
                    nsubviews))
                  ((numberp nsubviews) (make-list nsubviews))
                  ((and (null subviews) (eql nsubviews :prompt)  (null subview-type) (null default-subview-type))
                   (choose-views-from-menu :superclass subview-superclass
                                           :prompt-string  "Select layout views" ))
                  (t nil)))
      
      (let ((layout-status
             (loop for s in subviews collect
                   (cond ((legal-view-construct-p s)
                          :legal)
                         ((and (listp s) (every #'legal-view-construct-p s))
                          :layer)
                         (t (quail-error "Illegal subview argument ~A" s))))))
        
        
        (if subview-type
          (loop with subview-types = 
                (if (legal-view-construct-p subview-type) (list subview-type)
                    subview-type)
                for s in subviews 
                for status in layout-status
                for new = 
                (ecase status
                  (:legal (local-make-views s subview-types))
                  (:layer (loop for si in s 
                                append (local-make-views si subview-types))))
                collect new into lviews
                append new into sviews 
                finally 
                (setf (subviews-of self) sviews)
                (setf (layout-views-of self) lviews))
          
          (loop for s in subviews 
                for status in layout-status
                for new = 
                (ecase status
                  (:legal (list (local-make-view s)))
                  (:layer (mapcar #'local-make-view s)))
                collect new into lviews
                append new into sviews 
                finally 
                (setf (subviews-of self) sviews)
                (setf (layout-views-of self) lviews)))))) )




                                      
                                   

(defmethod view-layout-position-subviews ((self view-layout)
                                   &key  positions bounding-region)
 ;(flet ((plistp(arg)   ; 04SEP2023 and next 4 lines
 ;         (and (listp arg)
 ;              (evenp (length arg))
 ;              (loop for key in arg by #'cddr
 ;                    always (keywordp key)))))
  
  (constrain-bounds self)
  (let ((subs (layout-views-of self))
        sub-regions)
    
    
    (when subs
      (setf (positions-of self)
            (loop for i from 0 below (length subs)
                  collect (if (and positions (listp positions)) (elt positions i)
                              positions)))
      (loop for layer-list in subs
            for pos in  (positions-of self)
            do
            (loop with s = (car layer-list)
                  repeat (length layer-list) do
                  (push 
                   (cond ((region-p pos)
                          pos)
                         ((and pos (listp pos))
                          (apply #'make-region pos))
                         ((functionp pos) 
                          (funcall pos s) )
                         ((and pos (symbolp pos))
                          (funcall (get-function pos) s))
                         (t (bounding-region-of s)))
                   sub-regions)))
      (setf (sub-view-locns-of self) (nreverse sub-regions))
      (if (and (positions-of self) (null bounding-region))
        (set-bounding-region self :region (compute-containing-bounding-region self)))
      ))
  ;) ; 04SEP2023
 )



(defmethod use-x-axis-p ((self view-layout))
  t)

(defmethod use-y-axis-p ((self view-layout))
  t)




(defmethod compute-containing-bounding-region ((self view-layout) )

  (or
   (loop for svl in (sub-view-locns-of self)
        minimize (left-of svl) into left
        maximize (right-of svl) into right
        minimize (bottom-of svl) into bottom
        maximize (top-of svl) into top
        finally (return (make-region left right bottom top)))
   (make-region)))


         


(defmethod constrain-bounds ((self view-layout) &key draw? (link-bounds-x? :ignore)
                             (link-bounds-y? :ignore))
  (unless (eq link-bounds-x? :ignore)
    (setf (link-bounds-x-p self) link-bounds-x?))
  (unless (eq link-bounds-y? :ignore)
    (setf (link-bounds-y-p self) link-bounds-y?))
  (if (link-bounds-x-p self)
    (let* ((x-views (loop for v in (subviews-of self)
                              when (linkable-bounds-x-p v) collect v))
           (d-views (loop for v in x-views 
                          when (and (typep v 'd-view) (use-x-axis-p v)) 
                          collect v)))
      (link-view-bounds x-views :x)
      (set-view-extents x-views :x :recompute? nil :region (if d-views (maximize-x-extents d-views)))))
  (if (link-bounds-y-p self)
    (let* ((y-views (loop for v in (subviews-of self)
                              when (linkable-bounds-y-p v) collect v))
           (d-views (loop for v in y-views 
                          when (and (typep v 'd-view) (use-y-axis-p v)) 
                          collect v)))
      (link-view-bounds y-views :y)
      (set-view-extents y-views :y :recompute? nil :region (if d-views (maximize-y-extents d-views)))))
  (when (or (link-bounds-x-p self) (link-bounds-y-p self))
    (remap-to-viewports self :erase? draw? :draw? draw?)))



(defmethod delete-subview :before ((self view-layout) view)
  (setf (layout-views-of self)
  (loop for views in (layout-views-of self)
        for del = (delete view views)
        when del
        collect del)))


(defmethod add-to-named-slot  ((self view-layout) old new)
  
  (setf (layout-views-of self)
        (loop for views in (layout-views-of self)
              collect (if (member old views)
                        (append views (list new))
                        views))))

(defmethod add-subview :after ((self view-layout) subview new-region)
  (declare (ignore  new-region))
  (when (legal-subview-p self subview)
    (push (list subview) (layout-views-of self))))



(defmethod layer-subview :after ((self view-layout) view 
                          on  
                          &rest arg )
  (declare (ignore  arg))
  (add-to-named-slot self on view))


(defmethod dview-subview ((self view-layout))
  (or (car (subviews-of-type self 'd-view))
      (car (descendant-views-of-type self 'd-view))))

(defmethod menu-properties ((self view-layout) 
                            (slot-name (eql 'middle-menu)))
  (let ((dv (dview-subview self)))
    (if (typep dv 'data-menu-item-mixin)
      (acons :dim (ndims dv)
             (copy-tree (menu-properties dv 'middle-menu ))))))


(defmethod get-menu-items ((self view-layout) (slot-name (eql 'middle-menu)))
  (let ((v-items (var-menu-items self))
        (c-items (case-menu-items self))
        items)
    
    (setq items 
          (append
           (if v-items
             `(("Variables" nil "" :sub-items  ,v-items)))
           (if c-items
             `(("Cases" nil "" :sub-items  ,c-items)))))
    (setq items 
              (append items 
                      `(("Point symbols" nil "" :sub-items  ,(ps-subview-menu-items)))))
    
    (cons '("-" nil) items)))
  

(defmethod var-menu-items ((self view-layout) &key target)
  (let ((dv (dview-subview self)))
    (if (typep dv 'data-menu-item-mixin)
      (var-menu-items dv :method 'change-variable
                              :target target))))

(defmethod case-menu-items ((self view-layout) &key target)
  (let ((dv (dview-subview self)))
    (if (typep dv 'data-menu-item-mixin)
     (case-menu-items dv 
                       :target target))))


(defmethod change-variable ((self view-layout) &rest args
                            &key which (draw? t))
  ;; changes variable its function or transform and redraws plot
  
  (let (from-view)
  (if (null which)
    (setq from-view (car (intersection *selected-views* (subviews-of-type self 'd-view)))))
  
  (if (typep from-view 'd-view)
    (setq which (car (vars-of from-view))))
  
  (if (null which)
    (quail-error "Please select variate to change"))
  
  (if (null from-view)
    (setq from-view (loop for v in (subviews-of-type self 'd-view)
                          thereis 
                          (and (eq-variate-exprs (car (vars-of v)) which)
                               v))))
    (when (and from-view (has-variates-p from-view))
      (apply #'change-variable from-view :draw? draw?  :which which
                       args))))

  


