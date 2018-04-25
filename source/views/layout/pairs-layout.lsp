;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               pairs-layout.lisp
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
;;;----------------------------------------------------------------------------------


(in-package :views)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(pairs-layout add-diagonal-views pairs-sub-views init-label-texts)))



(defclass pairs-layout (grid-layout) 
  ((middle-menu :allocation :class :initform nil))
  (:default-initargs :box-views? t :pairs-view '2d-point-cloud :diag-view 'label
     :gap-x 0.05 :gap-y 0.05
    :link-bounds-x?  :by-col :link-bounds-y?  :by-row
    :initform-fn #'get-data-inits :min-nvars 2
    :subview-constructor #'pairs-sub-views
    ))


(defgeneric add-diagonal-views (pairs-layout &rest arg &key type &allow-other-keys) 
  (:documentation "Adds views of given type on diagonal"))

(defgeneric pairs-sub-views (pairs-layout &key &allow-other-keys))

(defmethod construct-sub-views :after ((self pairs-layout) &key)
  ;;Catherine, here's the hack that ensures that the layout-format is correct.
  (setf (layout-format-of self) (list (nrows-of self) (ncols-of self)))
  (init-label-texts self))

(defmethod init-label-texts ((self pairs-layout))
  (loop with n = (length (layout-views-of self))
        with nrows = (nrows-of self)
        with jx with jy with pair-y with pair-x
        for i upfrom 0
        for views in (layout-views-of self) 
        for label = (loop for v in views thereis (and (typep v 'label) v))
        when (and label (string-equal "" (get-text label)) )do
        
        (setq jy
              (if (= i (- n 1)) (- i 1) (+ i 1)))
        (setq jx
              (if (= i (- n 1)) (- i nrows) (+ i nrows)))
        
        (setq pair-y
              (if (> jy 0) (nth 0 (nth jy (layout-views-of self)))))
        (setq pair-x
              (if (> jx 0) (nth 0 (nth jx (layout-views-of self)))))
        (setf (text-of label)
              (if pair-y
                (progn 
                  (text-link  pair-y label :y)
                  (or (coord-string-y pair-y) "VAR"))
                "VAR"))
        (if pair-x (text-link  pair-x label :x))))

(defmethod prompt-for-pairs-view((self pairs-layout))
  (choose-views-from-menu 
                    :prompt-string "Select pair view(s)"
                    :superclass '2d-view))


(defmethod pairs-sub-views ((self pairs-layout) 
                              ;; &rest keyword-pairs
                               &key (cases #'list-cases) (value-fn #'value-of) labels coords
                               vars functions transforms 
                                ordered-case-views? (common-case-views? t)
                                case-views-from  pairs-view diag-view)
  (unless common-case-views?
    (setq case-views-from nil))
  
  (let* ((n (length vars))
         (fns (or functions (make-list n)))
         (trans (or transforms (make-list n )))
         (pair-args 
          (loop for y in vars for fy in fns for afy in trans
            append
            (loop for x in vars for fx in fns for afx in trans 
                  unless (eql x y) 
                  collect 
                  (list 
                   :x x  :x-function fx  :x-transform afx 
                   :y y :y-function fy :y-transform afy))))
         (diag-args (loop 
            for v in vars for f in fns for af in trans
             for l in (or (and (listp labels) labels)
                                  (make-list n :initial-element labels))
                  
            collect (list :var v :function f :transform af  :text (or l ""))))
         (add-args (list :data (viewed-object-of self)
                         :cases cases
                         :value-fn value-fn
                              :ordered-case-views? ordered-case-views?
                          :case-views-from case-views-from ))
        pair-layout  diag-layout mem)
    
    
    
    (cond ((eql pairs-view :prompt)
               (setf pairs-view (prompt-for-pairs-view self)))
              ((or (null pairs-view) (eql pairs-view :default))
               (setf pairs-view '2d-point-cloud))
              ((and (listp pairs-view) (setq mem (member :default pairs-view)))
               (setf (car mem) '2d-point-cloud)))
    
    (if (legal-view-construct-p  pairs-view)
             (loop with subview-arg = (append
                                    (subview-arg-list pairs-view '2d-point-cloud)
                                    add-args)
                   with c-coords with first-pc = nil
                   for arg in pair-args
                   for i upfrom 0  
                   for y = (truncate i (- n 1))
                   for x = (mod (+ i 1 (truncate i n)) n)
                   for new-coords = (if coords
                                      (mapcar #'list (elt coords x) (elt coords y))
                                      (if (and c-coords (>= i (- n 1)))
                                      (mapcar #'list (elt c-coords x) (elt c-coords y)))) 
                                      
                   for new-view =  (apply #'view :coords new-coords
                                          (append arg subview-arg))
                   collect (list new-view) into lviews
                   do
                   (when (and common-case-views? (null first-pc) (typep new-view 'one-per-case-mixin))
                     (setq first-pc new-view)
                     (setf (getf add-args :case-views-from) first-pc)
                     (setf (getf add-args :ordered-case-views?) t))

                   (when (and (null coords) (typep new-view 'd-view))
                   (if (= i 0)
                     (setq c-coords (list (y-coords-of new-view) (x-coords-of new-view)))
                     (if (< i ( - n 1))
                       (setq c-coords (append c-coords (list (x-coords-of new-view)))))))
                   
                  finally (setf pair-layout lviews))

             (loop with c-coords with new-view
                   for arg in pair-args
                   for i upfrom 0  
                   for y = (truncate i (- n 1))
                   for x = (mod (+ i 1 (truncate i n)) n)
                   for new-coords = (if coords
                                      (mapcar #'list (elt coords x) (elt coords y))
                                      (if (and c-coords (>= i (- n 1)))
                                      (mapcar #'list (elt c-coords x) (elt c-coords y))))
             
                   for new-views =
                   (loop for pairs-v in pairs-view
                         for subview-arg = (append
                                            (subview-arg-list pairs-v '2d-point-cloud)
                                            add-args)
                         for the-view = (apply #'view :coords new-coords
                                         (append arg subview-arg))
                         collect  the-view
                         when (and common-case-views? (null case-views-from) 
                                   (typep the-view '2d-point-cloud)) do
                     (setf case-views-from new-view)
                     (setf (getf add-args :ordered-case-views?) t)  ;; added 25-3-97
                     (setf (getf add-args :case-views-from) new-view))
                    collect new-views into lviews
                    do
                    (setq new-view (loop for v in new-views thereis 
                                         (and (typep v '2d-view) v)))
                     (when (and (null coords) (typep new-view 'd-view))
                    (if (= i 0)
                      (setq c-coords 
                            (if (and new-view (typep new-view 'd-view))
                              (list (y-coords-of new-view) (x-coords-of new-view))
                              (list nil nil)))
                      (if (< i ( - n 1))
                        (setq c-coords (append c-coords
                                             (if (and new-view (typep new-view 'd-view))
                                               (list (x-coords-of new-view))
                                               (list nil)))))))
                   finally (setf  pair-layout lviews)))

    (if (legal-view-construct-p  diag-view)
             (loop with subview-arg = (append
                                    (subview-arg-list diag-view 'label)
                                    add-args)
                   for arg in diag-args
                   for new-view =  (apply #'view 
                          (append arg subview-arg))
                   collect (list new-view) into lviews
                  finally (setf diag-layout lviews))

             (loop for arg in diag-args
                   for new-views =
                   (loop for diag-v in diag-view
                         for subview-arg = (append
                                            (subview-arg-list diag-v 'label)
                                            add-args)
                             collect (apply #'view 
                                        (append arg subview-arg)) )
                   collect new-views into lviews
                   finally (setf diag-layout lviews)))
    
    (flet ((order-views (diags pairs)
             (loop with k = (length diags)
                   with answer
                   for i from 0 below (* k k)
                   do
                   (if (zerop (mod i (+ k 1)))
                     (progn
                       (push (car diags) answer)
                       (setf diags (cdr diags)))
                     (progn
                       (push (car pairs) answer)
                       (setf pairs (cdr pairs))))
                   finally (return (nreverse answer)))))
       (setf (layout-views-of self)
            (order-views diag-layout pair-layout))
      (setf (subviews-of self)
            (apply  #'append (layout-views-of self)))
  
      
      )))



(defmethod change-variable ((self pairs-layout) 
                            &key var function transform
                            x x-function x-transform
                            y y-function y-transform 
                            which  (draw? t))
  ;; changes variable its function or transform and redraws plot
  
  
  (if (null which)
    (setq which (car (or (intersection *selected-views* (subviews-of-type self 'label))
                         (intersection *selected-views* (subviews-of-type self 'd-view))))))
  (if (typep which 'd-view)
    (setq which (car (vars-of which))))
  
  (if (null which)
    (quail-error "Please click on subview to select old variate"))
  
  (let ((subs2 (subviews-of-type self '2d-view))
        pcx pcy)
    
    (if (typep which 'label)
      (setq pcx 
            (loop for p in subs2 
                  thereis (and (member which (car (text-links-of p)))  p))
            pcy 
            (loop for p in subs2 
                  thereis (and (member which (second (text-links-of p)))  p)))
      
      (setq pcx 
            (loop for p in subs2 
                  thereis (and (eq-variate-exprs (x-variate-of p) which) p))
            pcy 
            (loop for p in subs2 
                  thereis (and (eq-variate-exprs (y-variate-of p) which) p))))
    
    (when pcx
      (setq var (or var x y))
      (setq function (or function x-function y-function))
      (setq transform (or transform x-transform y-transform))
      (if draw? (erase-view self))
      (change-variable pcx :draw? nil :x var :x-function function :x-transform transform )
      (change-variable pcy :draw? nil :y var :y-function function :y-transform transform  )
      (when draw?
        (loop for sv in (subviews-of self) do
              (remap-to-viewports sv :erase? nil :draw? nil))
        (draw-view self))
      )))


(defmethod change-case-status ((self pairs-layout) cases status
                               &key (rescale? t) (draw? t ))
  (if (eq cases :selected)
    (setq cases
          (loop for v in *selected-views*
                append (list-viewed-elements v))))
  (when cases
    (if draw? (erase-view self))
    (loop for v in (subviews-of-type self 'd-view)
          do
          (new-case-status v (if (eq cases t) (cases-of v) cases)
                           status :rescale? rescale?  :draw? nil ))
    (if draw? (draw-view self))))


(defmethod activate-cases ((self pairs-layout)  cases  &rest args &key )
  (apply #'change-case-status self cases t args))

(defmethod deactivate-cases ((self pairs-layout)  cases  &rest args &key )
  (apply #'change-case-status self cases :ignore args))

(defmethod activate-all-cases ((self pairs-layout)    &rest args &key )
  (apply #'change-case-status self t t args))




(defmethod add-diagonal-views ((self pairs-layout) 
                               &key type link-bounds-x?
                               link-bounds-y?)
  (let* ((pcs (loop for row in (col-format self)
                    collect
                    (loop for (r) in row
                          until (typep r '2d-view)
                          finally (return (if (typep r '2d-view) r)))))
         new-views
         )
    
    (erase-view self)
    (loop for (l) in (get-view-diag self)
          for pc in pcs
          for h = (if pc (view :type type
                               :data (dataset-of pc)
                               :cases (cases-of pc)
                               :var (x-variate-of pc)
                               :function (x-func-of pc)
                               :transform (x-transform-of pc)
                               :variates (variates-of pc)
                               :coords (x-coords-of pc)
                               :linkable-bounds-x? link-bounds-x?
                               :linkable-bounds-y? link-bounds-y?
                               :draw? nil))
          when h
          do
          (setq new-views (append new-views (list h)))
          (layer-subview self  h  l :clip-region nil :draw? nil :ignore-y? t))
 
    
    
    (loop for v in (subviews-of self) do
          (remap-to-viewports v :erase? nil :draw? nil))
    (draw-view self)))



(defmethod get-view-diag ((self pairs-layout))
  (let ((ncols (ncols-of self))) 
    (loop with subs = (layout-views-of self)
          for i from 0 below (length subs) by (+ 1 ncols) 
          collect (elt subs i))))





(defmethod nrows-of ((self pairs-layout))
  (let ((lf (layout-format-of self)))
    (unless (listp lf)
      (setq lf (list nil nil)))
  (or (car lf)
      (let* ((n (length (layout-views-of self)))
             (ncols (cadr lf))
             (nrows 
              (if (eql ncols 0)
                0
                (cond ((and n ncols) (ceiling (/ n ncols)))
                      ((<= n 1) n)
                      (t (ceiling (sqrt n)))))))
        (setf (layout-format-of self) (list nrows ncols))
        nrows))))

(defmethod ncols-of ((self pairs-layout))
  (let ((lf (layout-format-of self)))
    (unless (listp lf)
      (setq lf (list nil nil)))
  (or (second lf)
      (let* ((n (length (layout-views-of self)))
             (nrows (car lf))
             (ncols
              (if (eql nrows 0) 
                0
                (cond ((and n nrows) (ceiling (/ n nrows)))
                      ((<= n 1) n)
                      (t (ceiling (sqrt n)))))))
         (setf (layout-format-of self) (list nrows ncols))
        ncols))))

    
    


(defmethod vars-of ((self pairs-layout))
  (cons (loop for a in (second (layout-views-of self))
                            until (typep a '2d-view)
                            finally (if (typep a '2d-view)
                                      (return (y-variate-of a))))
                      
        (loop   for v in (cdr (layout-views-of self))
                for i from 0 below (- (nrows-of self) 1)
                collect (loop for a in v
                            until (typep a '2d-view)
                            finally (if (typep a '2d-view)
                                      (return (x-variate-of a)))))))


(defmethod funcs-of ((self pairs-layout))
  (cons (loop for a in (second (layout-views-of self))
                            until (typep a '2d-view)
                            finally (if (typep a '2d-view)
                                      (return (y-func-of a))))
                      
        (loop   for v in (cdr (layout-views-of self))
                for i from 0 below (- (nrows-of self) 1)
                collect (loop for a in v
                            until (typep a '2d-view)
                            finally (if (typep a '2d-view)
                                      (return (x-func-of a)))))))


(defmethod transforms-of ((self pairs-layout))
  (cons (loop for a in (second (layout-views-of self))
                            until (typep a '2d-view)
                            finally (if (typep a '2d-view)
                                      (return (y-transform-of a))))
                      
        (loop   for v in (cdr (layout-views-of self))
                for i from 0 below (- (nrows-of self) 1)
                collect (loop for a in v
                            until (typep a '2d-view)
                            finally (if (typep a '2d-view)
                                      (return (x-func-of a)))))))


(defmethod row-views ((self pairs-layout))
  (loop for row in (row-format self)
        collect
        (loop for v in (reduce #'append row)
              when (or
                    (typep v '2d-view)
                    (and (typep v '1d-view) (eql (orientation-of v) :vertical)))
              collect v)))
             


(defmethod col-views ((self pairs-layout))
  (loop for col in (col-format self)
        collect
        (loop for v in (reduce #'append col)
              when (or
                    (typep v '2d-view)
                    (and (typep v '1d-view) (eql (orientation-of v) :horizontal))) collect v)))
        

      
      
  
