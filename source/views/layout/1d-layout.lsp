;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               1d-layout.lisp
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
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(1d-layout 1d-sub-views )))


(defgeneric 1d-sub-views (view-layout &key &allow-other-keys))


(defclass 1d-layout (grid-layout) 
  ()
  (:default-initargs  :subview-superclass '1d-view
    :link-bounds-x? t :link-bounds-y? t :format :row
    :initform-fn #'get-data-inits :common-vars? nil
    :subview-constructor #'1d-sub-views :common-case-views? :default))
    

(defmethod coord-string-x ((self 1d-layout))
  (cond ((= 1 (ncols-of self))
         (list
          (or (loop for vi in (subviews-of self)
                    thereis (and (typep vi '1d-view) (free-coord-string vi)))
              "")))
           ((= 1 (nrows-of self))
            (loop for views in (col-format self)
                  for vf = (flatten-views views)
                  collect
                  (or (loop for vi in vf 
                            thereis (coord-string vi))
                      "")))
           (t (call-next-method))))
             
  

(defmethod coord-string-y ((self 1d-layout))
  (cond ((= 1 (nrows-of self))
         (list
          (or (loop for vi in (subviews-of self)
                    thereis (and (typep vi '1d-view) (free-coord-string vi)))
              "")))
           ((= 1 (ncols-of self))
            (loop for views in (row-format self)
                  for vf = (flatten-views views)
                  collect
                  (or (loop for vi in vf 
                            thereis (coord-string vi))
                      "")))
           (t (call-next-method))))







(defmethod 1d-sub-views ((self view-layout) 
                         &rest keyword-pairs
                         &key subview-type subview-superclass 
                         subviews common-case-views?
                         (cases #'list-cases)   vars functions transforms (value-fn #'value-of)
                         format orientation)
  (let* ((n (length vars))
         (fns (or functions (make-list n :initial-element nil)))
         (trans (or transforms (make-list n )))
          (dir (or orientation 
                  (if (eq format :row) :vertical :horizontal))))
    (if (eql common-case-views? :default)
      (setq common-case-views? (not subviews)))
    (labels ((set-subview-type()
               (or subview-type
                   (setq subview-type (choose-views-from-menu :prompt-string  "Select 1d views" 
                                                              :superclass subview-superclass))))
             (add-var(s v f tr)
               
               (let ((vlist (list :var v :function f :transform tr
                                  :cases cases :value-fn value-fn
                                  :orientation dir))) 
                 (cond ((typep s 'view) s)
                       ((and (listp s) (getf s :type))
                        (append s vlist))
                       ((listp s)
                        (set-subview-type)
                        (append s vlist))
                       (t (append (list :type s) vlist))))))
      (unless (or subviews  subview-type)
        (set-subview-type))
      
      (when vars
        (cond 
         ((and subviews (= (length subviews) (length vars)))
          (setq subviews 
                (loop for v in vars
                      for f in fns
                      for tr in trans
                      for s in subviews
                      collect
                      (cond ((legal-view-construct-p s)
                             (add-var s v f tr))
                            ((listp s)
                             (loop for si in s 
                                   collect (add-var si v f tr)))
                            (t
                             (add-var nil v f tr)))))
          )
         
         (t
          (setq subviews
                (loop for v in vars
                for f in fns
                for tr in trans collect
                (list :var v :function f :transform tr 
                      :cases cases 
                      :orientation dir)))))) ))
    (apply #'default-layout-sub-views self :subviews subviews :nsubviews nil
         :subview-type subview-type 
         :common-case-views? common-case-views? keyword-pairs)
  
  )


(defmethod vars-of ((self 1d-layout))
  (loop  for v in (layout-views-of self)
         for vi = (loop for vi in v 
                       thereis (and (typep vi '1d-view) vi))
         collect (if vi (variate-of vi))))



(defmethod funcs-of ((self 1d-layout))
  (loop  for v in (layout-views-of self)
         for vi = (loop for vi in v 
                       thereis (and (typep vi '1d-view) vi))
         collect (if vi (func-of vi))))


(defmethod transforms-of ((self 1d-layout))
  (loop  for v in (layout-views-of self)
         for vi = (loop for vi in v 
                       thereis (and (typep vi '1d-view) vi))
         collect (if vi (transform-of vi))))

