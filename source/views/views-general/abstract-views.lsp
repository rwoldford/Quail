;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               abstract-views.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(simple-view compound-view list-viewed-elements)))
;;;----------------------------------------------------------------------------------


(defclass simple-view (viewed-elements-mixin view) 
  ((right-menu :allocation :class :initform nil)
    ))


(defmethod description-string ((self simple-view))
  (let* ((vo (viewed-object-of self))
        (name (dataset-name vo)))
    (if (and name (not (equal name "NIL")))
    (format nil "~A viewing ~A" self name)
    (format nil "~A" self))))



(defmethod get-menu-items :around ((self simple-view) 
                                   (slot-name (eql 'right-menu)))
  
  (let ((result (call-next-method))
        (layers (append
                 (loop for (string sym) in 
                       (view-menu-list '(and simple-view (not d-view )))
                       collect 
                       `(,string (layer-view ,sym :viewport)))
                 `(("Selected" (layer-selected-view :viewport ))))))
    
    (add-menu-items self   result `(("Paste" nil  "" :sub-items ,layers)))))





(defclass compound-view (view)
  ((subview-position-region 
    :initform nil 
    :accessor subview-position-region-of 
    :documentation "Region where subviews are positioned.")
   (subviews 
    :initform nil 
    ;;:initarg :subviews
    :accessor subviews-of 
    :documentation "Subviews in a list")

   (sub-view-locns 
    :initform nil 
   ;; :initarg :subview-locns 
    :accessor sub-view-locns-of 
    :documentation "Subviews in a list")

   (visible-subs?
    :initform nil
    :accessor visible-subs-p
    :documentation "keeps track of which subs are currently inside self (usually all")))




(defmethod default-drawing-style :around ((self simple-view) &rest keyword-pairs  
                                          &key (color nil))

  (apply #'call-next-method self :color color keyword-pairs ))

(defmethod place-subview ((self simple-view)
                          (subview view)
                          locn )
  (declare (ignore locn))
  (quail-error 
     "~A is a simple-view and cannot have subviews, ~A is ignored"))

(defmethod (setf subviews-of) (new-value (self simple-view)  )
  (unless (null new-value)
    (quail-cerror 
     "~A is a simple-view and cannot have subviews, ~A is ignored"
     self new-value)))

(defmethod delete-subview ((self simple-view) subview)
    (quail-cerror 
     "~A is a simple-view and has no subviews, ~A is ignored"
     self subview))

(defmethod legal-subview-p ((self simple-view) subview)
  (declare (ignore subview ))
  nil)


;;;================================================================================



(defmethod subview-position-region ((self compound-view ))
  (or (subview-position-region-of self)
      (bounding-region-of self)))


(defmethod recompute-visible-subs ((self compound-view ))
  (setf (visible-subs-p self)  (visible-subviews-p self)))
  


(defmethod set-bounding-region :around ((self compound-view ) &rest ignore )
  (declare (ignore ignore)) 
  (call-next-method)
   (recompute-visible-subs self ))

(defmethod init-position-subviews ((self compound-view ) &key)
  (loop with br = (bounding-region-of self)
        for sv in (subviews-of self) do
         (place-subview self sv br)))


(defmethod init-position-subviews :around ((self compound-view ) &key)
  (if (subview-position-region-of self)
    (setf (subview-position-region-of self) 
          (make-region (bounding-region-of self))))
  (call-next-method)
   (recompute-visible-subs self ))


#|
(defmethod some-subviews((self compound-view) &key highlit?)
  (cond ((null highlit?)
         (loop for sv in (subviews-of self) 
               for inside? in (visible-subs-p self)
               when inside?
               collect sv))
        ((eq t highlit?)
         (loop for sv in (subviews-of self) 
               for inside? in (visible-subs-p self)
               when (and inside?
                         (any-highlight? sv))
               collect sv))
        (t (or (some-subviews self :highlit? t)
               (some-subviews self :highlit? nil)))))
|#

(defun has-highlit-subviews?(view)
  (or (any-highlight? view)
  (loop for v in (subviews-of view)
        thereis (has-highlit-subviews? v))))





(defmethod some-subviews((self compound-view) &key highlit?)
  (cond ((null highlit?)
         (loop for sv in (subviews-of self) 
               for inside? in (visible-subs-p self)
               when inside?
               collect sv))
        ((eq t highlit?)
         (loop for sv in (subviews-of self) 
               for inside? in (visible-subs-p self)
               when (and inside?
                         (has-highlit-subviews? sv))
               collect sv))
        (t (or (some-subviews self :highlit? t)
               (some-subviews self :highlit? nil)))))


(defmethod delete-subview ((self compound-view) subview)
  
  ;; remove subview and its location from self
  
  (with-accessors ((sub-views subviews-of)
                   (sub-view-locns sub-view-locns-of))
    self
    (loop for sv in  sub-views
          for svl in sub-view-locns
          until (eql sv subview)
          finally
          (when (eql sv subview)
            (setf sub-views (delete subview sub-views))
            (setf sub-view-locns (delete svl sub-view-locns))))))

(defmethod add-subview :after ((self compound-view ) subview new-region)
  (declare (ignore  new-region))
  (when (legal-subview-p self subview)
    (recompute-visible-subs self )))

(defmethod delete-subview :after ((self compound-view ) subview )
  
  (let* ((views (list-subviews self :test 
                               #'(lambda(v) (typep v 'linkable-bounds-mixin))))
         (px (find subview views :key #'link-bounds-x-of :test #'member))
         (py (find subview views :key #'link-bounds-y-of :test #'member))
         )
    (if px
      (unlink-view-bounds (link-bounds-x-of px) subview :x))
    (if py
      (unlink-view-bounds (link-bounds-y-of py) subview :y)))

  (recompute-visible-subs self ))


(defmethod layer-subview :after ((self compound-view) view  on &rest arg)
  
  (declare (ignore arg view on)) 
  (recompute-visible-subs self ))

(defmethod styles-to-subs ((self compound-view) ) nil)


(defmethod set-drawing-style :around  ((self compound-view) 
                                       &rest key-args
                                       &key  highlit? element styles)
  
  (let ((styles-to-subs (styles-to-subs self))
        (style-pairs (or styles key-args))
        my-styles other-styles)

    (loop for style in style-pairs by #'cddr
          for val in (cdr style-pairs) by #'cddr 
          unless (or (eq style :highlit?) 
                     (eq style :element))
          do
         ;; (if (and (eq style :color)
         ;;          (eq val :prompt))
         ;;   (setf val (wb::prompt-user-for-color)))
          (if (eq val :prompt)
            (setf val (prompt-for-style style)))

          (if (has-draw-style-p self style)
            (if (member style styles-to-subs)
              (progn
                (set-draw-style self style val)
                (push (draw-style self style) other-styles) 
                (push style other-styles))
              (progn
                (push val my-styles) 
                (push style my-styles)))
            
            (progn
              (push val other-styles) 
              (push style other-styles))))

    (if my-styles
      (apply #'call-next-method self :element element :styles my-styles key-args))
    (if other-styles
       (with-update-style-cache 
         (loop with sv-args+ = (append other-styles 
                                    (list :element element :highlit? highlit?))
            for v in (some-subviews self :highlit? highlit?)
            do
            (apply #'set-drawing-style v sv-args+ ))))))




(defmethod set-highlight-style :around  ((self compound-view) hi-val
                                         &rest key-args
                                         &key  highlit? element)
  (if (member :highlight? (styles-to-subs self))
    (progn 
      (set-draw-style self :highlight? hi-val)
      (setq hi-val (draw-style self :highlight?))
      (with-update-style-cache  
        (loop with sv-args+ = (list :element element)
          for v in (some-subviews self :highlit? highlit?)
          do
          (apply #'set-highlight-style v hi-val sv-args+ ))))
  
  (apply #'call-next-method self hi-val :element element  key-args)))

;;;================================================================================



(defmethod delete-all-subviews ((self compound-view))
  (setf (subviews-of self) nil)
  (setf (sub-view-locns-of self) nil))




