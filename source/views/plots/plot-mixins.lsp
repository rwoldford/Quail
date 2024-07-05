;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               plot-mixins.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '( title-of 
            position-key-mixin titled-view-mixin margined-view-mixin
            set-position-value *default-title-dims* *default-margin-size*)))
            

(defvar *default-title-dims* '(:width 0.9 :height 0.1))
(defvar *default-margin-size* 0.01)

(defclass position-key-mixin ()
  ((position-keys :initform nil :allocation :class)
  (default-positions
     :initform nil
     :accessor default-positions-of)
   )
  (:documentation "Mixin has a slot for storing parameters for positioning subviews."))


(defclass titled-view-mixin (position-key-mixin)
  ((title :initarg :title 
          :accessor title-of 
          :initform t)
   (position-keys :initform '(:title-height :title-width) :allocation :class)
   (right-menu :allocation :class :initform nil)
   )
  (:default-initargs 
    :title-height (getf *default-title-dims* :height)
    :title-width (getf *default-title-dims* :width)
    :title-font nil :title-text #'dataset-name)
                     
  (:documentation "Mixin for view with a title "))


(defclass margined-view-mixin (position-key-mixin)
  ((right-menu :allocation :class :initform nil)
   (position-keys :allocation :class 
                  :initform '(:left-margin-size :right-margin-size
                              :bottom-margin-size :top-margin-size) ))
  (:default-initargs 
    :left-margin-size  *default-margin-size*
    :right-margin-size *default-margin-size*
    :bottom-margin-size *default-margin-size* :top-margin-size *default-margin-size*)
  (:documentation "Mixin for view with margins.~
                   Should be included in precedence list before any other view~
                   which adjusts the subview-position-region."))

(defmethod initialize-instance :around ((self margined-view-mixin)
                                        &rest initargs
                                        &key no-margins?
                                      &allow-other-keys)
  (if no-margins?
    (apply #'call-next-method self :left-margin-size 0 :right-margin-size 0
           :top-margin-size 0 :bottom-margin-size 0 initargs)
    (call-next-method)))


(defgeneric set-position-value (position-key-mixin  parameter &key  &allow-other-keys)
  (:documentation "Sets the position parameter"))












;;;----------------------------------------------------------------------------------




(defmethod initialize-instance :before ((self position-key-mixin)  
                                       &rest keyword-pairs
                                       &key)
  
  (setf (default-positions-of self) 
        (loop for key in (slot-value self 'position-keys)
              for val = (getf keyword-pairs key :no-value)
              unless (eql val :no-value)
              nconc (list key val))))




(defmethod reposition-view :around ((self position-key-mixin)   
                                    &rest keyword-pairs
                                    &key 
                                    (default-positions (default-positions-of self )))
  ;; extra arg default-positions
  (call-next-method self :default-positions 
         (append keyword-pairs default-positions)))



(defmethod set-position-value ((self position-key-mixin)  parameter &key  value (recompute? t))
  (let* ((pars (default-positions-of self))
         (old (getf pars parameter)))
    
      (unless (numberp value)
        (setq value
              (if (and  (numberp old) (not (zerop old)))
                (case value
                  (:larger (* 1.5 old))
                  (:smaller (* 0.67 old))
                  (t (wb:prompt-user :result-type 'number :read-type :eval
                                     :prompt-string
                                     (format nil "Change ~A from ~S" parameter old))))
                (wb:prompt-user :result-type 'number :read-type :eval
                                :prompt-string (format nil "Change ~A from ~S" parameter old)))))
      (setf (getf pars parameter) value)
      (if recompute? (reposition-view self))))



(defmethod position-parameter-menu-list  ((self position-key-mixin))
  (flet ((key-list (k)
           `(("larger"  (set-position-value ,k :value :larger ))
             ("smaller"  (set-position-value ,k :value :smaller ))
             ("prompt" (set-position-value ,k )) )))

    (loop for k in (default-positions-of self) by #'cddr 
          collect (list (string-downcase (princ-to-string k))
                        nil "" :sub-items (key-list k)))))


(defmethod get-menu-items :around ((self position-key-mixin)  (slot-name (eql 'right-menu)))
  (let ((result (call-next-method))
        (size-list (position-parameter-menu-list self)))
    (add-menu-items self result
                    `(( "Positions" nil "" :sub-items ,size-list) ))))
                


(defmethod subview-position-region-of :before ((self margined-view-mixin))
  (if (null (slot-value self 'subview-position-region))
    (setf (slot-value self 'subview-position-region) 
          (make-region (bounding-region-of self)))))


(defmethod init-position-subviews :before ((self margined-view-mixin) 
                                           &key left-margin-size right-margin-size
                                           bottom-margin-size top-margin-size
                                           )
  (let* ((interior-region (subview-position-region-of self))
         (br (bounding-region-of self))
         (w (width-of br ))
         (h (height-of br )))
    (incf (left-of interior-region) (max 0 (* w left-margin-size )))
    (incf (bottom-of interior-region) (max 0 (* h bottom-margin-size)))
    (decf (right-of interior-region) (max 0 (* w right-margin-size)))
    (decf (top-of interior-region) (max 0 (* h top-margin-size)))
    (valid-region-check interior-region)
    ))


(defmethod view-title-string ((self titled-view-mixin))
  "TITLE"
  )


(defmethod construct-sub-views :before ((self titled-view-mixin )  
                                        &key title-font title-text subviews-all?)
   (with-accessors ( (title title-of)
                   (viewed-obj viewed-object-of))
                  self
    (if (and (null title) subviews-all?) (setq title t))
    (setf title
          (cond ((null title) nil)
                ((legal-label-construct-p title)
                 (view-from-arg  
                  title 
                  (list :font title-font  :data viewed-obj :text title-text)
                  'label #'sublabel-arg-list))
                ((and (listp title) (legal-label-construct-p (car title)))
                 (view-from-arg 
                  (car title)
                  (list :font title-font  :data viewed-obj :text title-text)
                  'label #'sublabel-arg-list))))
      (if (and title (typep title 'label))
        (unless (or (stringp (text-of title))
                    (and (functionp (text-of title)) (funcall (text-of title) viewed-obj)))
          (setf (text-of title) (view-title-string self))))))
              



(defmethod subview-position-region-of :before ((self titled-view-mixin))
  (if (null (slot-value self 'subview-position-region))
    (setf (slot-value self 'subview-position-region) 
          (make-region (bounding-region-of self)))))


(defmethod init-position-subviews :before ((self titled-view-mixin) 
                                           &key title-height title-width )
  
  (let ((interior-region (subview-position-region-of  self))
        (br (bounding-region-of self))
        (title (title-of self))) 
    (when title 
      (place-subview self title
                     (sub-region interior-region :n
                                 :width (* (width-of br) title-width)
                                 :height (* (height-of br) title-height)
                                 :remains interior-region))
      )
    (valid-region-check interior-region)
    ))

