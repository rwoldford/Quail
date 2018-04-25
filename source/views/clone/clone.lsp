;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               clone.lisp
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
;;;     C.B. Hurley 1993 George Washington University
;;;     
;;;
;;;
;;;------------------------------------------------


(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(view-clone-list clone-view-fn
          clone-view-fn-prompt subview-keys-of)))

(defgeneric view-clone-list (view)
  (:documentation "Returns an argument list to be passed to clones"))

(defgeneric subview-clone-list (view)
  (:documentation "Returns an argument list to clone subviews"))

(defgeneric toplevel-clone-list (view)
  (:documentation "Toplevel part of argument list for cloning"))

(defgeneric subview-keys-of (view)
  (:documentation "Returns a list of keywords. ~
                   When constructing a view, if a keyword in this list~
                   appears more than once in the view construction arglist~
                   the values are appended."))

(defgeneric clone-view-fn (view &optional new-name)
  (:documentation "Returns an function which will construct a clone of view"))


(defun clone-view-fn-prompt(view &key viewport)
  (let* ((pos (prompt-position view viewport))
        (name 
         (wb:prompt-user :type 'symbol
                         :read-type :read
                         :prompt-string "Name of function?"
                         :left (2d-position-x pos)
                         :top (2d-position-y pos))))
    (clone-view-fn view name)))



(defmethod clone-view-fn ((self view) &optional new-name )
  (let* ((clone-list (view-clone-list self)))
    (if new-name
      (apply #'make-view-constructor-with-doc new-name (class-name (class-of self))
             clone-list)
      (apply #'make-view-constructor-fn (class-name (class-of self)) clone-list))))

(defmethod view-clone-list ((self view))
  (append (toplevel-clone-list self)
          (subview-clone-list self)))

(defmethod toplevel-clone-list ((self view)))

(defmethod subview-clone-list ((self view)))


(defmethod toplevel-clone-list :around ((self view))
  (append
   (list  :type (class-name (class-of self)))
   (unless (linkable-bounds-x-p self)
     (list :linkable-bounds-x? nil))
   (unless (linkable-bounds-y-p self)
     (list :linkable-bounds-y? nil))
   (call-next-method)
   (loop for (a . b) in (drawing-style-of self) 
         collect a collect b)))




(defmethod subview-keys-of ((self view)))



(defmethod subview-keys-of ((self one-per-case-mixin))
  (list :case-view))


  

(defmethod subview-clone-list  ((self one-per-case-mixin))
  (let* ((sub (first (subviews-of self))))
    (if sub (list :case-view (view-clone-list sub)))))

(defmethod subview-clone-list  ((self display-list))
  (let* ((sub (first (subviews-of self))))
    (if sub (list :item-type (view-clone-list sub)))))

(defmethod toplevel-clone-list ((self orientation-mixin )) 
  (append
   (list :orientation (orientation-of self))
   (call-next-method)))

(defmethod toplevel-clone-list ((self justification-mixin )) 
  (append
   (list :justification (justification-of self))
   (call-next-method)))

(defmethod toplevel-clone-list  ((self position-key-mixin))
  (append (default-positions-of self)
          (call-next-method) ))


(defmethod toplevel-clone-list  ((self boxed-view-mixin))
  (append (list :box? (box-p self))
          (call-next-method) ))

(defmethod toplevel-clone-list  ((self boxed-subview-mixin))
  (append (list :box-views? (box-views-p self))
          (call-next-method) ))

(defmethod toplevel-clone-list  ((self display-list))
  (append (list :display-list-border (display-list-border-of self))
          (call-next-method) ))


(defmethod subview-keys-of ((self boxplot-view))
  (list :case-view :boxes))

(defmethod subview-clone-list  ((self boxplot-view))
  (let* ((subs (subviews-of self))
         (box (first subs))
         (point (fifth subs))
         clone-args)
    (if box 
      (setq clone-args (list :boxes (view-clone-list box))))
    (if point 
      (setq clone-args 
            (append clone-args (list :case-view (view-clone-list point)))))
    clone-args))

(defmethod toplevel-clone-list  ((self bar))
  (append (list :orientation (orientation-of self)
        :collect-styles? (collect-styles-p self))
          (call-next-method)))

(defmethod toplevel-clone-list  ((self function-view))
  (append
   (list :function (function-of self)
         :nlines (nlines-of self)
         :domain (domain-of self))
   (call-next-method)))

(defmethod toplevel-clone-list ((self label ))
  (let ((tx (text-of self)))
    (if (functionp tx)
      (append
       (list  :text (text-of self))
       (call-next-method))
      (call-next-method)
      ) ))

(defmethod toplevel-clone-list  ((self line))
  (append
   (list :slope (slope-of self)
         :intercept (intercept-of self))
   (call-next-method)))

(defmethod toplevel-clone-list  ((self fitted-line))
  (append
   (list :fit-fn (fit-fn-of self))
   (call-next-method))
  )

(defmethod toplevel-clone-list  ((self flip-mixin))
  (append
   (list :flip-x? (flip-x-p self) :flip-y? (flip-y-p self))
   (call-next-method)))

(defmethod toplevel-clone-list  ((self edge-lines-mixin))
  (append
   (list :lines-to (lines-to self) )
   (call-next-method)))




(defmethod toplevel-clone-list  ((self smooth-mixin))
  (append
   (list :fit-fn (fit-fn-of self) :smooth-par (smooth-par-of self))
   (call-next-method))
  )

(defmethod toplevel-clone-list  ((self moving-dview-mixin))
  (append
   (list :coords-scale-method (coords-scale-method-of self) )
   (call-next-method))
  )

(defmethod toplevel-clone-list  ((self rotating-cloud))
  (append
   (list :draw-axis?  (draw-axis-p  self) :draw-label?  (draw-label-p  self) )
   (call-next-method))
  )

(defmethod toplevel-clone-list  ((self histogram-view))
  (append
   (list :histogram-scale (histogram-scale-of self) )
   (call-next-method))
  )


(defmethod toplevel-clone-list  ((self axis))
  (append
   (list :ntics (ntics-of self) 
         :internal-tics? (internal-tics-p self))
   (call-next-method)))

(defmethod toplevel-clone-list  ((self point-symbol))
  (append
   (list :label (label-of self) )
   (call-next-method)))

(defmethod toplevel-clone-list  ((self view-with-size))
  (append
   (list :size (view-size-of self) )
   (call-next-method)))
        
(defmethod toplevel-clone-list  ((self plot))
  (let ((iv (interior-view-of self))
        (ans (call-next-method)))
    (when iv
      (if (compute-applicable-methods #'link-bounds-x-p  (list iv ))
        (setq ans (append (list :link-bounds-x? (link-bounds-x-p iv)) ans )))
      (if (compute-applicable-methods #'link-bounds-y-p  (list iv ))
        (setq ans (append (list :link-bounds-y? (link-bounds-y-p iv)) ans ))))
    ans))
        
  

(defmethod subview-keys-of ((self histogram-view))
  (list :bars))

(defmethod subview-clone-list  ((self histogram-view))
  (let ((bar (first (subviews-of self))))
    (if bar 
      (list :bars (view-clone-list bar))
      )))

(defmethod subview-keys-of ((self plot))
  (list :title :left-view :right-view :bottom-view :top-view
        :left-label :right-label :bottom-label :top-label
        :interior-view))

(defmethod subview-clone-list  ((self plot))
  (flet ((get-clone-list(v)
           (if v (view-clone-list v))))
    (let ((iv (interior-views-of self)) (ti (title-of self))
           (lv (left-views-of self)) (rv (right-views-of self))
           (bv (bottom-views-of self)) (tv (top-views-of self))
           (ll (left-label-of self)) (rl (right-label-of self))
           (bl (bottom-label-of self)) (tl (top-label-of self)))
       (loop for sub in (list ti iv lv rv bv tv ll rl bl tl)
             for key in (list :title :interior-view 
                              :left-view :right-view :bottom-view  :top-view
                              :left-label  :right-label :bottom-label :top-label )
             nconc (list key (if (listp sub) 
                                 (mapcar #'get-clone-list sub)
                                 (get-clone-list sub)))))))



(defmethod subview-clone-list  ((self grid-plot))
  (flet ((get-clone-list(v)
           (if v (view-clone-list v)))
         (non-null-elt(v)
           (find  t v :key #'(lambda(x) (not (null x))))))
    
    (append
      (let ((iv (interior-views-of self)) (ti (title-of self))
           (lv (non-null-elt (left-views-of self)))
            (rv (non-null-elt (right-views-of self)))
           (bv (non-null-elt (bottom-views-of self)))
           (tv (non-null-elt (top-views-of self)))
           (ll (left-label-of self)) (rl (right-label-of self))
           (bl (bottom-label-of self)) (tl (top-label-of self)))
       (loop for sub in (list ti iv lv rv bv tv ll rl bl tl)
             for key in (list :title :interior-view 
                              :left-view :right-view :bottom-view  :top-view
                              :left-label  :right-label :bottom-label :top-label )
             nconc (list key (if (listp sub) 
                                 (mapcar #'get-clone-list sub)
                                 (get-clone-list sub))))))))

(defmethod subview-keys-of ((self view-layout))
  '(:subviews))

(defmethod subview-clone-list  ((self view-layout))
  (flet ((get-clone-list(v)
           (if v (view-clone-list v))))
    `(:subviews
       ,(loop for sub in (layout-views-of self)
               collect (mapcar #'get-clone-list sub)))))
       


(defmethod toplevel-clone-list  ((self view-layout)) 
  (append
   `( 
    :positions ,(positions-of self)
    :link-bounds-x? ,(link-bounds-x-p self)
    :link-bounds-y? ,(link-bounds-y-p self))
   (call-next-method)))


(defmethod toplevel-clone-list  ((self table-layout)) 
  (append
   `( 
    :scaled-dimension ,(scaled-dimension-of self)
    :justification ,(justification-of self)
     )
   (call-next-method)))
      
(defmethod subview-clone-list  ((self grid-layout))
  (flet ((get-clone-list(v)
           (if v (view-clone-list v))))
    `(:subviews
       ,(loop for sub in (layout-views-of self)
               collect (mapcar #'get-clone-list sub))
         )))

(defmethod subview-clone-list  ((self batch-layout))
  (flet ((get-clone-list(v)
           (if v (view-clone-list v))))
    `(:subviews
       ,(mapcar #'get-clone-list (car (layout-views-of self))))))

(defmethod subview-clone-list  ((self 1d-layout))
  (flet ((get-clone-list(v)
           (if v (view-clone-list v))))
    `(:subviews
       ,(mapcar #'get-clone-list (car (layout-views-of self))))))

(defmethod toplevel-clone-list  ((self grid-layout)) 
  (call-next-method))

(defmethod subview-keys-of ((self pairs-layout))
  (list :diag-view :pairs-view))

(defmethod subview-clone-list  ((self pairs-layout))
  (flet ((get-clone-list(v)
           (if v (view-clone-list v))))
    (let ((pair (second (layout-views-of self)))
          (diag (first (layout-views-of self))))
      
      `(:pairs-view
        ,(mapcar #'get-clone-list pair)
        :diag-view ,(mapcar #'get-clone-list diag)))))
        



(defmethod subview-clone-list  ((self view-layers))
  (flet ((get-clone-list(v)
           (if v (view-clone-list v))))
    `(:subviews
       ,(loop for sub in (subviews-of self)
               collect (get-clone-list sub))
       :link-bounds-x? ,(link-bounds-x-p self)
       :link-bounds-y? ,(link-bounds-y-p self))))















      



 

            





