;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               draw-style-mixins.lisp
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


(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(draw-style-mixin  single-draw-style-mixin multiple-draw-style-mixin
           construct-new-style-p style-keys-of
           drawing-style-of 
           set-drawing-style set-highlight-style
           style-proportions all-highlight? any-highlight?)))

;;;----------------------------------------------------------------------------------
(defclass draw-style-mixin () 
  ((style-keys :initform nil :allocation :class :accessor style-keys-of)
   (drawing-style 
    :initarg :drawing-style
    :initform nil
    :documentation  "an alist specifying how view is to be drawn")
   ))

(defclass single-draw-style-mixin (draw-style-mixin) 
  ())

(defclass multiple-draw-style-mixin (draw-style-mixin) 
  ((drawing-style 
    :initarg :drawing-style
    :initform nil
    :accessor drawing-styles-of)))


(defgeneric drawing-style-of (draw-style-mixin &key &allow-other-keys)
  (:documentation "Returns drawing style of self as an alist "))


(defgeneric set-drawing-style (draw-style-mixin 
                              &rest style-pairs &key &allow-other-keys)
  (:documentation "Sets draw-styles according to style pairs and redraws "))
 
(defgeneric set-highlight-style (view hi-val &rest args &key &allow-other-keys))




(defgeneric style-proportions (multiple-draw-style-mixin style-keys &optional args)
  (:documentation "Returns the proportions of self drawn with ~
                  each combination of values for styles in style-keys"))

(defgeneric default-drawing-style (draw-style-mixin 
                                  &key &allow-other-keys)
  (:documentation "Returns a default style for view"))

(defgeneric init-drawing-style (draw-style-mixin 
                                  &key &allow-other-keys)
  (:documentation "Called at view initialization to construct the drawing style"))

(defgeneric check-styles (draw-style-mixin style-keys)
                                 
  (:documentation "Checks that view has correct styles and adds if necessary"))

(defgeneric construct-new-style-p (draw-style-mixin)
  (:documentation "Tests if the drawing style needs to be reconstructed"))
                    
;;;----------------------------------------------------------------------------------

(defmethod initialize-instance :after ((self draw-style-mixin) 
                                       &rest keyword-pairs
                                       &key  check-style? )
  (let ((style-keys (style-keys-of self)))
    (if (construct-new-style-p self)
      (apply #'init-drawing-style self (find-pairs keyword-pairs  style-keys))
      (if check-style? (check-styles self (find-pairs keyword-pairs  style-keys) )))))


(defmethod default-drawing-style ((self draw-style-mixin)  &rest keyword-pairs 
                                  &key (highlight? nil) (invisible? nil))
  (apply #'make-drawing-style 
         :highlight? highlight?  
         :invisible? invisible? keyword-pairs))
         
  
(defmethod drawing-style-of ((self draw-style-mixin) &key)
  (slot-value self 'drawing-style))

(defmethod (setf drawing-style-of) (new-style (self draw-style-mixin) )
  (setf (slot-value self 'drawing-style) new-style))


(defmethod init-drawing-style ((self single-draw-style-mixin) 
                                   &rest keyword-pairs)
 (setf (drawing-style-of self) 
        (apply #'default-drawing-style self  keyword-pairs)))

(defmethod construct-new-style-p ((self single-draw-style-mixin)) 
  (not (drawing-style-p (drawing-style-of self))))


(defmethod check-styles ((self single-draw-style-mixin) style-keys)
  (let ((ds (drawing-style-of self)))
    (loop for style in style-keys by #'cddr
          for val in (cdr style-keys) by #'cddr 
          unless (has-draw-style-p ds style) do
          (add-style ds style val))))


(defmethod drawing-styles-of ((self single-draw-style-mixin))
  (list (drawing-style-of self)))
  
(defmethod has-draw-style-p ((self single-draw-style-mixin) slot-name)
  (has-draw-style-p (drawing-style-of self) slot-name))
  
(defmethod element-draw-style ((self single-draw-style-mixin)
                           viewed-obj-elt test )
  (when (member viewed-obj-elt (list-viewed-elements self) :test test )
      (drawing-style-of self)))


(defmethod draw-style ((self single-draw-style-mixin) slot-name &key)
  (draw-style (drawing-style-of self) slot-name))

(defmethod set-draw-style :around
           ((self single-draw-style-mixin)
            (slot-name (eql :color))
            (val (eql :prompt)) &key)
  (set-draw-style (drawing-style-of self) :color (wb::prompt-user-for-color)))

(defmethod set-draw-style ((self single-draw-style-mixin) slot-name val &key)
  (set-draw-style (drawing-style-of self)  slot-name val))


(defmethod copy-style-values ((view-1 single-draw-style-mixin)
                        (view-2 single-draw-style-mixin))

  (copy-style-values (drawing-style-of view-1)
                       (drawing-style-of view-2)))

(defmethod viewed-elt-test ((self draw-style-mixin))
  *default-data-eq*)



#|
(defmethod set-drawing-style ((self single-draw-style-mixin) 
                              &key element styles test from elements)
                              
  ;; accepts a single viewed object
  
  (when (or (null element)
            ;;(and from (member self (linked-views-of from)))
            (and from
            (member element (list-viewed-elements self) 
                    :test (or test (viewed-elt-test self))))
            (and from
            (subsetp elements (list-viewed-elements self) 
                    :test (or test (viewed-elt-test self)))))
            
    (loop for style in styles by #'cddr
          for val in (cdr styles) by #'cddr do
          (set-draw-style self style val ))
    :set-style))

(defmethod set-highlight-style ((self single-draw-style-mixin) hi-val
                              &key element  test from not-from)
                              
  ;; accepts a single viewed object
  (when (or (and (null element)    (null not-from))
            ;;(and from (member self (linked-views-of from)) )
            from
            (member element (list-viewed-elements self) 
                    :test (or test (viewed-elt-test self)))
            
            (and not-from 
                 (not (if (typep not-from 'view) 
                        (or (eq not-from self)
                            (member self (linked-views-of not-from)))
                        (some #'(lambda(v) (or (eq v self) (member self (linked-views-of v)) ))
                              not-from)))))
            
    (set-draw-style self :highlight? hi-val )
    :set-style))

|#


(defmethod set-drawing-style ((self single-draw-style-mixin) 
                              &key element styles test from elements)
  
  (declare (ignore from))
  (when (or (and (null element)  (null elements))
            (and elements (intersection elements (list-viewed-elements self) 
                                   :test (or test (viewed-elt-test self))))
            
            (and element (member element (list-viewed-elements self) 
                                 :test (or test (viewed-elt-test self)))))
    
    (loop for style in styles by #'cddr
          for val in (cdr styles) by #'cddr do
          (set-draw-style self style val ))
    :set-style))


(defmethod set-highlight-style ((self single-draw-style-mixin) hi-val
                                &key element elements test from not-from)
  (declare (ignore from))                      
  (when (or (and (null element)  (null elements)  (null not-from))
            (and elements (intersection elements (list-viewed-elements self) 
                                   :test (or test (viewed-elt-test self))))
            (and element (member element (list-viewed-elements self) 
                                 :test (or test (viewed-elt-test self))))
            
            (and not-from 
                 (not (if (typep not-from 'view) 
                        (or (eq not-from self)
                            (member self (linked-views-of not-from)))
                        (some #'(lambda(v) (or (eq v self) (member self (linked-views-of v)) ))
                              not-from)))))
    
    (set-draw-style self :highlight? hi-val )
    :set-style))

;;--------------------------------------------------------------------------------

(defmethod construct-new-style-p ((self multiple-draw-style-mixin))
  (let ((ds (drawing-styles-of self)) )
    (if (and ds (listp ds) (every #'drawing-style-p ds))
      (let ((list-vo (list-viewed-elements self)))
        (not (= (length ds) (if list-vo (length list-vo) 1))))
     t)))

(defmethod has-draw-style-p ((self multiple-draw-style-mixin) slot-name)
  (loop for ds in (drawing-styles-of self)
        thereis (has-draw-style-p ds slot-name)))


(defmethod summary-drawing-style ((self multiple-draw-style-mixin))
 ;; could be done on a majority basis
  (let* ((all-styles (drawing-styles-of self))
         (sty (copy-drawing-style (car all-styles)))
         (h-val
          (loop for s in all-styles thereis (draw-style s :highlight?)))
         (i-val
          (loop for s in all-styles thereis (draw-style s :invisible?))))
  (set-draw-style sty :highlight? h-val)
  (set-draw-style sty :invisible? i-val)
    sty))

(defmethod drawing-style-of ((self multiple-draw-style-mixin) 
                             &key element
                             (test (viewed-elt-test self)))

;; gets style corresponding to element
  ;; if the element does not have a corresponding
  ;; style use single (summary) drawing style

  (or (element-draw-style self element test) 
      (summary-drawing-style self)))
  

(defmethod init-drawing-style ((self multiple-draw-style-mixin) 
                               &rest keyword-pairs)
                               
  ;; one draw style per element of viewed-elements
  ;; where draw-styles and element are matched by position

  (setf (drawing-styles-of self)
        (loop with list-vo = (list-viewed-elements self)
              repeat (if list-vo (length list-vo) 1)
              collect (apply #'default-drawing-style self keyword-pairs))))

(defmethod check-styles ((self multiple-draw-style-mixin) style-keys)
  (loop for ds in (drawing-styles-of self) do
        (loop for style in style-keys by #'cddr
              for val in (cdr style-keys) by #'cddr 
              unless (has-draw-style-p ds style) do
              (add-style ds style val))))


(defmethod element-draw-style ((self multiple-draw-style-mixin)
                           viewed-obj-elt test )
  (let ((p (if viewed-obj-elt
             (position viewed-obj-elt 
                     (list-viewed-elements self) :test test))))
    (if p
      (elt (drawing-styles-of self) p))))

(defmethod element-draw-styles ((self multiple-draw-style-mixin)
                           viewed-obj-elt test )
  (if viewed-obj-elt
    (loop for ve in (list-viewed-elements self)
          for d in (drawing-styles-of self)
          when (funcall test ve viewed-obj-elt)
          collect d)))
  


(defmethod draw-style ((self multiple-draw-style-mixin) slot-name 
                       &key element) 
  ;; gets style corresponding to element
  ;; if the element does not have a corresponding
  ;; style use single (summary) drawing style

  (draw-style (drawing-style-of self :element element) 
              slot-name))


(defmethod draw-style ((self multiple-draw-style-mixin) ( slot-name (eql :invisible?))
                       &key element)
  ;; multi-style view draw method should handle invisible
  (if (null element)
    nil
    (call-next-method)))


(defmethod set-draw-style ((self multiple-draw-style-mixin) slot-name val 
                           &key element 
                           (test (viewed-elt-test self)))
  ;; sets style corresponding to element
  ;; if the element is nil set all styles
  
  (if (null element)
    (loop for ds in (drawing-styles-of self) do
          (set-draw-style ds  slot-name val))
    
    (let ((ds-vo (element-draw-style self element test)))
      (if ds-vo (set-draw-style ds-vo  slot-name val)))))

(defmethod set-draw-style :around
           ((self multiple-draw-style-mixin)
            (slot-name (eql :color))
            (val (eql :prompt))
            &key element 
            (test (viewed-elt-test self)))
  (let ((new-colour (wb::prompt-user-for-color)))
    (set-draw-style self
                    :color new-colour
                    :element element
                    :test test)))
    



#|

(defmethod set-drawing-style ((self multiple-draw-style-mixin) 
                              &key element styles from
                              (test (viewed-elt-test self)))
  
  (let ((changes? nil))
   (cond ((and (null element) (null from))
          (setq changes? :set-style)
         (loop for style in styles by #'cddr
               for val in (cdr styles) by #'cddr do
                (set-draw-style self style val )))
        ((and element (not from))
         (loop for ds-vo in (element-draw-styles self element test)
               do
               (setq changes? :set-style)
               (loop for style in styles by #'cddr
                   for val in (cdr styles) by #'cddr do
                   (set-draw-style ds-vo style val ))))
         (from
         (loop with from-elts = (or element (list-viewed-elements from))
                 for elt in (list-viewed-elements self)
               for ds-vo in (drawing-styles-of self)
               when (member-if #'(lambda(x) (funcall test x  elt)) from-elts)
                 do
                 (setq changes? :set-style)
               (loop for style in styles by #'cddr
                   for val in (cdr styles) by #'cddr do
                   (set-draw-style ds-vo style val )))
         (when (and (null changes?) (null element))
           (setq changes? :set-style)
           (loop for style in styles by #'cddr
                 for val in (cdr styles) by #'cddr do
                 (set-draw-style self style val ))))
        (t nil))
   changes?))



          
  



(defmethod set-highlight-style ((self multiple-draw-style-mixin) hi-val
                              &key element from not-from 
                              (test (viewed-elt-test self)))
  ;; accepts a single viewed object
  (let ((changes? nil))
  (cond ((and (null element) (null from) (null not-from) )
         (setq changes? :set-style)
         (set-draw-style self :highlight? hi-val ))
        ((and element (not from))
         (loop for ds-vo in (element-draw-styles self element test)
               do  (setq changes? :set-style)
               (set-draw-style ds-vo :highlight? hi-val )))
         
        (from
         (loop with from-elts = (or element (list-viewed-elements from))
                for elt in (list-viewed-elements self)
               for ds-vo in (drawing-styles-of self)
               when (member-if #'(lambda(x) (funcall test x  elt)) from-elts)
               do (setq changes? :set-style)
               (set-draw-style ds-vo :highlight? hi-val ))
         (when (and (null changes?) (null element))
           (setq changes? :set-style)
           (set-draw-style self :highlight? hi-val ))
         )
        
        ((and not-from (typep not-from 'view) (not (eq not-from self)))
         (if (not (member self (linked-views-of not-from)))
           (progn 
             (setq changes? :set-style)
             (set-draw-style self :highlight? hi-val ))
           (loop with from-elts = (list-viewed-elements not-from)
                 with not-all? = nil
                 with test = (link-table-test-of (link-table-of self))
                 for element in (list-viewed-elements self)
                 for ds-vo in (drawing-styles-of self)
                 if (member-if #'(lambda(x) (funcall test x  element)) from-elts)
                 do (setq not-all? t)
                 else collect ds-vo into change-ds
                 finally (when not-all?
                           (setq changes? :set-style) 
                           (loop for d in change-ds do
                                 (set-draw-style d :highlight? hi-val ))))))

        ((and not-from (listp not-from) (not (member self not-from)))
         (if (not (some #'(lambda(x) (member self (linked-views-of x))) not-from))
           (progn 
             (setq changes? :set-style)
             (set-draw-style self :highlight? hi-val ))
         (loop with from-elts = (loop for v in not-from append (list-viewed-elements v))
               with not-all? = nil
                 with test = (link-table-test-of (link-table-of self))
               
               for element in (list-viewed-elements self)
               for ds-vo in (drawing-styles-of self)
               if (member-if #'(lambda(x) (funcall test x  element)) from-elts)
               do (setq not-all? t)
               else collect ds-vo into change-ds
                finally (when not-all?
                           (setq changes? :set-style) 
                           (loop for d in change-ds do
                                 (set-draw-style d :highlight? hi-val ))))))

        (t nil))
  changes?))

|#

(defmethod set-drawing-style ((self multiple-draw-style-mixin) 
                              &key element styles from elements highlit?
                              (test (viewed-elt-test self)))
  
   (let ((changes? nil))
     (cond ((and (null element) (null from) (null elements))
            (setq changes? :set-style)
            (if (or (eq t highlit?) (and highlit? (any-highlight? self)))
              (loop for d in (drawing-styles-of self)
                    when (draw-style d :highlight?) do
                    (loop for style in styles by #'cddr
                          for val in (cdr styles) by #'cddr do
                          (set-draw-style d style val )))
              (loop for style in styles by #'cddr
                    for val in (cdr styles) by #'cddr do
                    (set-draw-style self style val ))))
        
        ((or elements from)
         (loop with from-elts = (or elements (list-viewed-elements from))
               for elt in (list-viewed-elements self)
               for ds-vo in (drawing-styles-of self)
               when (member-if #'(lambda(x) (funcall test x  elt)) from-elts)
               do
               (setq changes? :set-style)
               (loop for style in styles by #'cddr
                     for val in (cdr styles) by #'cddr do
                     (set-draw-style ds-vo style val )))
         (when (and (null changes?) (null elements))
           (setq changes? :set-style)
           (loop for style in styles by #'cddr
                 for val in (cdr styles) by #'cddr do
                 (set-draw-style self style val ))))
        (element
         (loop for ds-vo in (element-draw-styles self element test)
               do
               (setq changes? :set-style)
               (loop for style in styles by #'cddr
                   for val in (cdr styles) by #'cddr do
                   (set-draw-style ds-vo style val ))))
        (t nil))
   changes?))


(defmethod set-highlight-style ((self multiple-draw-style-mixin) hi-val
                              &key element from not-from elements
                              (test (viewed-elt-test self)))
  ;; accepts a single viewed object
  (let ((changes? nil))
  (cond ((and (null element) (null from) (null not-from) (null elements) )
         (setq changes? :set-style)
         (set-draw-style self :highlight? hi-val ))
        
         
        ((or elements from)
         (loop with from-elts = (or elements (list-viewed-elements from))
                for elt in (list-viewed-elements self)
               for ds-vo in (drawing-styles-of self)
               when (member-if #'(lambda(x) (funcall test x  elt)) from-elts)
               do (setq changes? :set-style)
               (set-draw-style ds-vo :highlight? hi-val ))
         (when (and (null changes?) (null elements))
           (setq changes? :set-style)
           (set-draw-style self :highlight? hi-val ))
         )
        (element
         (loop for ds-vo in (element-draw-styles self element test)
               do  (setq changes? :set-style)
               (set-draw-style ds-vo :highlight? hi-val )))
        
        ((and not-from (typep not-from 'view) (not (eq not-from self)))
         (if (not (member self (linked-views-of not-from)))
           (progn 
             (setq changes? :set-style)
             (set-draw-style self :highlight? hi-val ))
           (loop with from-elts = (list-viewed-elements not-from)
                 with not-all? = nil
                 with test = (link-table-test-of (link-table-of self))
                 for element in (list-viewed-elements self)
                 for ds-vo in (drawing-styles-of self)
                 if (member-if #'(lambda(x) (funcall test x  element)) from-elts)
                 do (setq not-all? t)
                 else collect ds-vo into change-ds
                 finally (when not-all?
                           (setq changes? :set-style) 
                           (loop for d in change-ds do
                                 (set-draw-style d :highlight? hi-val ))))))

        ((and not-from (listp not-from) (not (member self not-from)))
         (if (not (some #'(lambda(x) (member self (linked-views-of x))) not-from))
           (progn 
             (setq changes? :set-style)
             (set-draw-style self :highlight? hi-val ))
         (loop with from-elts = (loop for v in not-from append (list-viewed-elements v))
               with not-all? = nil
                 with test = (link-table-test-of (link-table-of self))
               
               for element in (list-viewed-elements self)
               for ds-vo in (drawing-styles-of self)
               if (member-if #'(lambda(x) (funcall test x  element)) from-elts)
               do (setq not-all? t)
               else collect ds-vo into change-ds
                finally (when not-all?
                           (setq changes? :set-style) 
                           (loop for d in change-ds do
                                 (set-draw-style d :highlight? hi-val ))))))

        (t nil))
  changes?))

(defmethod copy-style-values ((view-1 multiple-draw-style-mixin)
                        (view-2 multiple-draw-style-mixin))
 (loop for ds1 in (drawing-styles-of view-1)
       for ds2 in (drawing-styles-of view-2) do
  (copy-style-values ds1 ds2)))

;;-------------------------------------------------------------------------------



(defmethod set-drawing-style :around ((self draw-style-mixin) 
                                      &rest style-pairs &key (draw? t) (erase? :default) 
                                       styles)
  (let* ((style-args (or styles (find-styles self style-pairs)))
         (hilite-only? (and ;;(null element) (null from)
                            (= (length style-args) 2)  
                            (eq :highlight? (car style-args))))
         result)
      
      
      (when (and (eq erase? :default)
                 (or (member :width  style-args) (member :size style-args)))
        (erase-view self)
        (setq erase? nil))
      (setq result
            (apply #'call-next-method self :styles style-args style-pairs))
      
      (when (and result style-args draw?)
        (unless (or  hilite-only?  (null style-args))
        (if erase?  (erase-view self)))
        (if hilite-only?
          (let ((hi-val (second style-args))) 
            (update-highlight self hi-val))
          (let ((pos (position :invisible? style-args)))
            (unless (and pos (draw-style self :invisible? ))
              (if (viewports-of self) (optional-cache (draw-view self )))
              ) )))))


(defmethod set-highlight-style :around ((self draw-style-mixin) hi-val
                                   &rest args   &key (draw? t) )
      
        (if (and (apply #'call-next-method self hi-val args) 
               draw?)
          (update-highlight self hi-val)))










(defmethod update-highlight  ((self draw-style-mixin) style-val)
  
  (if (null style-val)   ; bug in macl case, it ignores nil clause?
    (optional-cache (downlight-view self))
    (case style-val
      (:toggle
       (if (draw-style self :highlight?)
         (optional-cache (highlight-view self))
         (optional-cache (downlight-view self))))
      (t (optional-cache (highlight-view self))))))

(defmethod find-styles ((self draw-style-mixin) style-pairs)
  (loop with result for s in style-pairs by #'cddr
        for val in (cdr style-pairs) by #'cddr
        when (has-draw-style-p self s) do
        (push val result) (push s result)
        finally (return result)))




;;--------------------------------------------------------------------------------
;; extracting proportions from mutiple-draw-style-mixin for different
;; style combinations



(defmethod style-values ((self multiple-draw-style-mixin) style)
  ;; extracts all the values for style in drawing-style of self

   (loop with stl-vals
         for ds in (drawing-styles-of self) do
         (pushnew (draw-style ds style) stl-vals)
         finally (return stl-vals)))

(defmethod order-style-values ((self multiple-draw-style-mixin) style)
   (order-style-values (style-values self style) style))

(defmethod order-style-values ( (vals list) style)
  (declare (ignore style))
  (if (and (= 2 (length vals)) (member nil vals) (member t vals))
    (list t nil)
    vals))


(defmethod order-style-values ( (vals list) (style (eql :color) ))
  (if (or (some #'wb:colorp vals) (every #'null vals))
    (wb:order-colors (remove-duplicates vals :test #'wb:eq-colors))
    (wb:order-patterns (remove-duplicates vals :test #'wb:eq-patterns))))
  

(defun sty-cross-prod (a b)
  ;; a and b are (possibly null) lists)
  ;; return the set cross product
  (cond ((or (null a) (null b))
         (setq a (or a b))
         (loop for ai in a 
               collect (if (and ai (listp ai))
                         ai (list ai))))
        (t
         (loop for ai in a 
               for ai-list = (if (and ai (listp ai))
                               ai (list ai))
               nconc
               (loop for bi in b 
                     collect
                     (if (and bi (listp bi))
                       (append ai-list bi)
                       (append ai-list (list bi))))))))
                       
                    

(defun styles-match-p (draw-stl keys vals)
;; returns t if draw-stl has key-value pairs eq to
;; the ith keys -- vals, for each i

  (loop for k in keys
        for v in vals
        always (eq (draw-style draw-stl k) v)))

(defmethod style-proportions ((self multiple-draw-style-mixin) style-keys &optional weights)
;; returns the proportions of self drawn with
;; combination of styles

  (if (null style-keys)
    (list '(t) '(1.0))
    (let (style-combinations style-counts)
      (unless (listp style-keys)
        (setq style-keys (list style-keys)))
      (loop for sk in style-keys 
            for sk-vals = (order-style-values self sk) do
            (setq style-combinations
                  (sty-cross-prod style-combinations sk-vals)))

      (setq style-counts (make-list (length style-combinations) 
                                    :initial-element 0))
      (loop for d in (drawing-styles-of self) 
            for i upfrom 0 
            do
            (loop for style-vals in style-combinations 
                  for s-count on style-counts do
                  (if (styles-match-p d style-keys style-vals)
                    (incf (car s-count) (if weights (nth i weights) 1)))))
      (loop with n = (reduce #'+ style-counts)
            for c in style-counts for sc in style-combinations
            unless (zerop c) collect (/ c n) into sty-counts
            and collect sc into sty-combinations
            finally (return (list sty-combinations sty-counts))))))


(defmethod style-proportions-in-order ((self multiple-draw-style-mixin) style-keys &optional weights)
  (flet ((test(s1 s2)
           (loop for k in style-keys
                 always (eq (draw-style s1 k) (draw-style s2 k)))))
    (let* ((styles (drawing-styles-of self))
           (n (if weights (reduce #'+ weights) (length styles))))
      (loop with combs = (list (first styles))
            with counts = (if weights (list (car weights)) (list 1))
            for s in (cdr styles)
            for i upfrom 1
            for merge? = (test s (first combs))
            unless merge?
            do (push s combs) (push (if weights (nth i weights) 1) counts)
            when merge? do (incf (car counts) (if weights (nth i weights) 1))
          finally (return  (list (loop for comb in  (reverse combs) collect
                  (loop for k in style-keys collect
                        (draw-style comb k)))
            (mapcar #'(lambda(x) (/ x n)) (reverse counts))))))))


   

(defmethod update-instance-for-different-class 
           :after ((old single-draw-style-mixin) 
                   (new multiple-draw-style-mixin) &rest initargs)
  
  (declare (ignore initargs))
  (if (slot-boundp old 'drawing-style)
  (unless (typep old 'multiple-draw-style-mixin)
    (let ((style-args (loop for (a . b) in (drawing-style-of old)
                            collect a collect b)))
      (setf (drawing-style-of new)
            (loop with list-vo = (list-viewed-elements old)
                  repeat (if list-vo (length list-vo) 1)
                  collect (apply #'default-drawing-style new style-args)))))))



(defmethod update-instance-for-different-class 
           :after ((old multiple-draw-style-mixin) 
                   (new single-draw-style-mixin) &rest initargs)
  
  (declare (ignore initargs))
  (if (slot-boundp old 'drawing-style)
  (unless (typep new 'multiple-draw-style-mixin)
    (setf (drawing-style-of new) (drawing-style-of old)))
  ))


(defmethod update-instance-for-different-class 
           :around ((old draw-style-mixin) 
                    (new draw-style-mixin) &rest initargs)
  (declare (ignore initargs)) 
  (call-next-method) 
  (if (slot-boundp new 'drawing-style)
    (let* ((style-keys (style-keys-of new))
           view style-defaults)
      (unless
        (loop for key in style-keys
              always (has-draw-style-p new key))
        (setq view (view :type (class-name (class-of new))))
        (setq style-defaults (loop for (a . b) in (drawing-style-of view)
                                   collect a collect b))
        (check-styles new (find-pairs style-defaults style-keys))))))




(defgeneric any-highlight? (view)
  (:documentation "Returns t if any part of the view is highlit"))

(defmethod any-highlight? ((self single-draw-style-mixin))
  (draw-style self :highlight?))

(defmethod any-highlight? ((self multiple-draw-style-mixin))
  (loop for d in (drawing-styles-of self)
        thereis (draw-style d :highlight?)))


(defgeneric all-highlight? (view)
  (:documentation "Returns t if all parts of the view are highlit"))

(defmethod all-highlight? ((self single-draw-style-mixin))
  (draw-style self :highlight?))

(defmethod all-highlight? ((self multiple-draw-style-mixin))
  (loop for d in (drawing-styles-of self)
        always (draw-style d :highlight?)))


(defmethod highlight-value ((self single-draw-style-mixin) &optional toggle)
  (if toggle
    (not (draw-style self :highlight?))
    (draw-style self :highlight?)
    ))

(defmethod highlight-value ((self multiple-draw-style-mixin) &optional toggle)
  (if toggle
  (loop for d in (drawing-styles-of self) collect
        (not (draw-style d :highlight?)))
   (loop for d in (drawing-styles-of self) collect
        (draw-style d :highlight?))))





(defgeneric set-highlight-style-on(self &rest args  &key &allow-other-keys )
  )










      


#|
(defgeneric highlit-viewed-elements(self &key status)
  )

(defmethod highlit-viewed-elements ((self single-draw-style-mixin) 
                                  &key (status t ))
  (if (eq (draw-style self :highlight?) status)
    (list-viewed-elements self)))


(defmethod highlit-viewed-elements ((self multiple-draw-style-mixin) 
                               &key (status t ) )
  (loop for d in (drawing-styles-of self)
        for v in (list-viewed-elements self)
        when (eq status (draw-style d :highlight?))
        collect v))
|#
(defun active-viewports-p(view)
  (loop for v in (viewports-of view)
        thereis (active-viewport-p v)))