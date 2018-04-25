;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               xy-layout.lisp
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
;;;     C.B. Hurley 1995 George Washington University
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(xy-layout xy-sub-views)))



(defclass xy-layout (grid-layout) 
  ((middle-menu :allocation :class :initform nil))
  (:default-initargs :box-views? t :subview-superclass '2d-view
     :gap-x 0 :gap-y 0
    :link-bounds-x? :by-col :link-bounds-y? :by-row
    :initform-fn #'get-data-inits-xy 
    :subview-constructor #'xy-sub-views :common-case-views? :default
    ))


(defgeneric xy-sub-views (view-layout &key &allow-other-keys))

(defmethod construct-sub-views :after ((self xy-layout) &key x-vars y-vars)
  (setf (layout-format-of self) (list (length y-vars) (length x-vars))))




(defmethod xy-sub-views ((self xy-layout) &rest keyword-pairs
                               &key subview-type  subview-superclass 
                               subviews common-case-views?
                               (cases #'list-cases) (value-fn #'value-of)
                               x-vars y-vars x-functions x-transforms 
                               y-functions y-transforms 
                                )
  (setq x-functions (or x-functions (make-list (length x-vars))))
  (setq y-functions (or y-functions (make-list (length y-vars))))
  (setq x-transforms (or x-transforms (make-list (length x-vars))))
  (setq y-transforms (or y-transforms (make-list (length y-vars))))
  
  (if (eql common-case-views? :default)
      (setq common-case-views? (not subviews)))

  (labels ((set-subview-type()
             (or subview-type
                 (setq subview-type (choose-views-from-menu :prompt-string  "Select xy view(s)"
                                                            :superclass subview-superclass))))
           (add-var(s x-list y-list)
             
             (let ((vlist (append x-list y-list (list :cases cases :value-fn value-fn))))
               (cond ((typep s 'view) s)
                     ((and (listp s) (getf s :type))
                      (append s vlist))
                     ((listp s)
                        (set-subview-type)
                        (append s vlist))
                     (t (append (list :type s) vlist))
                     ))))
    
    (unless (or subviews  subview-type)
      (set-subview-type))
    (when (and x-vars y-vars)
      (cond
       ((and subviews (= (length subviews) (* (length x-vars) (length y-vars))))
        (setq subviews
              (loop with i = 0
                    for y in y-vars
                    for yf in y-functions
                    for yt in y-transforms
                    for y-list = (list :y y :y-function yf :y-transform yt)
                    append
                    (loop for x in x-vars
                          for xf in x-functions
                          for xt in x-transforms
                          for s = (nth i subviews)
                          for x-list = (list :x x :x-function xf :x-transform xt)
                          collect 
                          (cond ((legal-view-construct-p s)
                                 (add-var s x-list y-list))
                                ((listp s)
                                 (loop for si in s 
                                       collect (add-var si x-list y-list)))
                                (t
                                 (add-var nil x-list y-list)))
                          do (incf i))))
        )
       (t 
        
        
        (setq subviews
              (loop for y in y-vars
                    for yf in y-functions
                    for yt in y-transforms
                    for y-list = (list :y y :y-function yf :y-transform yt :cases cases)
                    append
                    (loop for x in x-vars
                          for xf in x-functions
                          for xt in x-transforms
                          collect (append (list   :x x :x-function xf :x-transform xt) y-list )))) ))) )
    (apply #'default-layout-sub-views self :subviews subviews :nsubviews nil
           :subview-type subview-type
                        
         :common-case-views? common-case-views? keyword-pairs))
  
  










    
    



