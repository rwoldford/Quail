;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               bordered-view-mixin.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '( 
            bordered-view-mixin default-margin-text
            interior-view-of interior-views-of
            left-view-of left-label-of left-views-of left-labels-of 
           right-view-of right-label-of right-views-of right-labels-of
           bottom-view-of bottom-label-of bottom-views-of bottom-labels-of
           top-view-of top-label-of top-views-of top-labels-of
           *default-border-view-sizes* *default-border-label-sizes*
           *default-xy-ratio*)))

(defvar *default-border-view-sizes*
  '(:left 0.1 :right 0.1 :bottom 0.07 :top 0.07))

(defvar *default-border-label-sizes*
  '(:left 0.07 :right 0.07 :bottom 0.07 :top 0.07))

(defvar *default-xy-ratio* nil)


(defclass bordered-view-mixin (position-key-mixin)
  ((right-menu :allocation :class :initform nil)
   (position-keys :allocation :class 
                  :initform '(:left-view-size :right-view-size 
                              :bottom-view-size :top-view-size
                              :bottom-label-size :top-label-size
                              :left-label-size :right-label-size
                              :xy-ratio) )

   (interior-view :initarg :interior-view :initarg :interior-views
                  :accessor interior-views-of :initform nil)
   (left-view :initarg :left-view :initarg :left-views  
              :accessor left-views-of :initform nil)
   (left-label :initarg :left-label :initarg :left-labels 
               :accessor left-labels-of :initform nil)
   (right-view :initarg :right-view :initarg :right-views 
               :accessor right-views-of :initform nil)
   (right-label :initarg :right-label :initarg :right-labels 
                :accessor right-labels-of :initform nil)
   (bottom-view :initarg :bottom-view :initarg :bottom-views
                :accessor bottom-views-of :initform nil)
   (bottom-label :initarg :bottom-label :initarg :bottom-labels 
                 :accessor bottom-labels-of :initform nil)
   (top-view :initarg :top-view :initarg :top-views
             :accessor top-views-of :initform nil)
   (top-label :initarg :top-label :initarg :top-labels
              :accessor top-labels-of :initform nil))
  
  (:default-initargs 
    :no-labels? nil :no-margin-views? nil
    :label-font nil
    :left-view-size (getf *default-border-view-sizes* :left)
    :right-view-size (getf *default-border-view-sizes* :right)
    :bottom-view-size (getf *default-border-view-sizes* :bottom)
    :top-view-size (getf *default-border-view-sizes* :top)
    :bottom-label-size (getf *default-border-label-sizes* :bottom)
    :top-label-size (getf *default-border-label-sizes* :top)
    :left-label-size (getf *default-border-label-sizes* :left)
    :right-label-size (getf *default-border-label-sizes* :right)
    :xy-ratio *default-xy-ratio*)
                    
  (:documentation "Provides slots for interior-view,~
                   left, right bottom and top margin views~
                   and left, right bottom and top label views"))



(defgeneric construct-margin-labels (bordered-view-mixin &key  &allow-other-keys))

(defgeneric construct-margin-views (bordered-view-mixin &key  &allow-other-keys))

(defgeneric construct-interior-view (bordered-view-mixin &key  &allow-other-keys))

(defgeneric compute-interior-region (bordered-view-mixin &key  &allow-other-keys))

(defgeneric init-position-margin-labels (bordered-view-mixin outer-space inner-space
                                                             &key  &allow-other-keys))

(defgeneric init-position-margin-views (bordered-view-mixin outer-space inner-space
                                                            &key  &allow-other-keys))

(defgeneric init-position-interior-view (bordered-view-mixin space &key  &allow-other-keys))

(defgeneric init-position-interior-views (bordered-view-mixin space &key  &allow-other-keys))



(defgeneric default-margin-text (bordered-view-mixin axis))

(defmethod construct-sub-views  ((self bordered-view-mixin) 
                                &rest keyword-pairs &key all-subviews? no-labels?
                                no-margin-views?)
  
  (if all-subviews?
    (with-accessors ((ll  left-labels-of) (rl right-labels-of)
                     (tl top-labels-of) (bl bottom-labels-of)
                     (lv  left-views-of) (rv right-views-of)
                     (tv top-views-of) (bv bottom-views-of))
                    self
      (multiple-value-setq (tl bl rl ll lv rv bv tv)
        (values-list 
         (loop for v in (list tl bl rl ll lv rv bv tv)
              collect (or v t))))))
                  
 
    (apply #'construct-interior-view self  keyword-pairs )
   
    (if no-margin-views? 
      (progn
        (setf (left-views-of self) nil)
        (setf (right-views-of self) nil)
        (setf (bottom-views-of self) nil)
        (setf (top-views-of self) nil))
      (apply #'construct-margin-views self keyword-pairs ))
     (if no-labels? 
      (progn
        (setf (left-labels-of self) nil)
        (setf (right-labels-of self) nil)
        (setf (bottom-labels-of self) nil)
        (setf (top-labels-of self) nil))
      (apply #'construct-margin-labels self  keyword-pairs))
    )


(defmethod init-label-text ((self bordered-view-mixin) 
                            (margin T) label)
  (if (typep label 'label)
    (let* ((iv  (interior-view-of self))
           (lab (text-of label)))
      (if (and  (not (equal lab "")) (typep iv 'd-view) )
        (if (equal lab (coord-string-x iv))
          (text-link iv label :x)
          (if (equal lab (coord-string-y iv)) (text-link iv label :y)))))))







(defmethod construct-margin-views ((self bordered-view-mixin) 
                                   &rest keyword-pairs )
  (declare (ignore keyword-pairs)) )

(defmethod construct-interior-view ((self bordered-view-mixin) 
                                   &rest keyword-pairs )
  (declare (ignore keyword-pairs)))

(defmethod construct-margin-labels ((self bordered-view-mixin) 
                                   &rest keyword-pairs )
  (declare (ignore keyword-pairs)))

(defmethod init-position-margin-views ((self bordered-view-mixin) 
                                       outer-space inner-space
                                       &rest keyword-pairs )
  (declare (ignore keyword-pairs outer-space inner-space)) )



(defmethod init-position-margin-labels ((self bordered-view-mixin)
                                        outer-space inner-space
                                        &rest keyword-pairs )
  (declare (ignore keyword-pairs outer-space inner-space)))

(defmethod init-position-interior-views ((self bordered-view-mixin)
                                        space
                                        &rest keyword-pairs )
  (declare (ignore keyword-pairs space)))

(defmethod init-position-interior-view ((self bordered-view-mixin)
                                        space
                                        &rest keyword-pairs )
  (apply #'init-position-interior-views self space keyword-pairs ))


(defmethod init-position-subviews ((self bordered-view-mixin)
                                    &rest keyword-pairs)
  
  (let ((outer-space (subview-position-region self))
        (inner-space (apply #'compute-interior-region self keyword-pairs)))
    (apply #'init-position-interior-views self inner-space keyword-pairs)
    (apply #'init-position-margin-labels self outer-space inner-space keyword-pairs)
    (apply #'init-position-margin-views self outer-space inner-space keyword-pairs)
    (setf (subview-position-region-of self) inner-space)
    
    ))
   

(defmethod interior-view-of ((self bordered-view-mixin))
  (let ((v (slot-value self 'interior-view)))
    (if (listp v) (car v) v)))

(defmethod left-view-of ((self bordered-view-mixin))
  (let ((v (slot-value self 'left-view)))
    (if (listp v) (car v) v)))

(defmethod right-view-of ((self bordered-view-mixin))
  (let ((v (slot-value self 'right-view)))
    (if (listp v) (car v) v)))

(defmethod bottom-view-of ((self bordered-view-mixin))
  (let ((v (slot-value self 'bottom-view)))
    (if (listp v) (car v) v)))

(defmethod top-view-of ((self bordered-view-mixin))
  (let ((v (slot-value self 'top-view)))
    (if (listp v) (car v) v)))


(defmethod left-label-of ((self bordered-view-mixin))
  (let ((v (slot-value self 'left-label)))
    (if (listp v) (car v) v)))

(defmethod right-label-of ((self bordered-view-mixin))
  (let ((v (slot-value self 'right-label)))
    (if (listp v) (car v) v)))

(defmethod bottom-label-of ((self bordered-view-mixin))
  (let ((v (slot-value self 'bottom-label)))
    (car v)))

(defmethod top-label-of ((self bordered-view-mixin))
  (let ((v (slot-value self 'top-label)))
    (if (listp v) (car v) v)))


(defmethod (setf interior-view-of) (view (self bordered-view-mixin))
  (setf (slot-value self 'interior-view) (list view)))

(defmethod (setf left-view-of) (view (self bordered-view-mixin))
  (setf (slot-value self 'left-view) (list view)))
    

(defmethod (setf right-view-of) (view (self bordered-view-mixin))
  (setf (slot-value self 'right-view) (list view)))

(defmethod (setf bottom-view-of) (view (self bordered-view-mixin))
  (setf (slot-value self 'bottom-view) (list view)))

(defmethod (setf top-view-of) (view (self bordered-view-mixin))
  (setf (slot-value self 'top-view) (list view)))

(defmethod (setf left-label-of) (view (self bordered-view-mixin))
  (setf (slot-value self 'left-label) (list view)))


(defmethod (setf right-label-of) (view (self bordered-view-mixin))
  (setf (slot-value self 'right-label) (list view)))

(defmethod (setf bottom-label-of) (view (self bordered-view-mixin))
  (setf (slot-value self 'bottom-label) (list view)))



(defmethod (setf top-label-of) (view (self bordered-view-mixin))
  (setf (slot-value self 'top-label) (list view)))























    
