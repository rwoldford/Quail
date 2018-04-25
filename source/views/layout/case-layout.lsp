;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               case-layout.lisp
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
;;;     C.B. Hurley 1996 
;;;
;;;
;;;----------------------------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               case-layout.lisp
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


(eval-when (:compile-toplevel :load-toplevel :execute) (export '(case-layout  )))


(defclass case-layout (grid-layout) 
  ()
  (:default-initargs  :subview-type '1d-point-cloud :default-subview-type '1d-point-cloud
    :link-bounds-x? t :link-bounds-y? t :format :col
    :initform-fn #'get-data-inits-1 :common-vars? t :common-case-views? nil
    :subview-constructor #'case-layout-sub-views))

(defgeneric case-layout-sub-views (view-layout &key &allow-other-keys))

(defmethod case-layout-sub-views ((self view-layout) 
                         &rest keyword-pairs
                         &key subview-type subview-superclass 
                         subviews data (labels #'identifier-name)
                         (cases #'list-cases)   (common-case-views? t) (common-vars? nil)
                          format orientation)
  (let* ( (dir (or orientation 
                  (if (eq format :row) :vertical :horizontal)))
         var-args)
    (setq cases (cases-of self))
    
   (setq var-args
         (loop with var-keys = (list :vars :functions :transforms 
                                     :var :function :transform 
                                     :x :x-function :x-transform 
                                      :y :y-function :y-transform 
                                       :z :z-function :z-transform )
               for key in keyword-pairs by #'cddr
               for val in (cdr keyword-pairs) by #'cddr
               when (member key var-keys)
               collect key and collect val ))

   (setq var-args (append  (list :orientation dir :data data) var-args))
        
   (labels ((set-subview-type()
              (or subview-type
                  (setq subview-type (choose-views-from-menu :prompt-string  "Select 1d views" 
                                                             :superclass subview-superclass))))
            (add-arg( arg s)
              (cond ((null s)
                      (append  var-args arg))
                     ((typep s 'view) s)
                    ((and (listp s) (getf s :type))
                     (append s var-args arg))
                    ((listp s)
                     (append s var-args arg (list  :type (set-subview-type) )))
                    (t (cons s  (append var-args var-args ))))))
      (unless (or subviews  subview-type)
        (set-subview-type))
      
      (when cases
        (setq subviews
              (cond 
               ((and subviews (listp subviews))
                (if (legal-view-construct-p subviews)
                  (setq subviews (list subviews)))
                (loop for s in cases
                      for l in (if (listp labels) labels
                                  (make-list (length  cases) :initial-element labels))
                      for list-s = `(:cases  ,(list s) :labels ,(list l))
                    
                      collect
                      (loop
                        for sub in subviews
                        for legal =  (legal-view-construct-p sub)
                        collect
                        (cond (legal (add-arg  list-s sub))
                              (t nil)))))
               
               (t
                (loop for s in cases
                       for l in (if (listp labels) labels
                                  (make-list (length  cases) :initial-element labels))
                      for list-s = `(:cases  ,(list s) :labels ,(list l))
                 
                      collect (add-arg  list-s nil))))))
    (apply #'default-layout-sub-views self :subviews subviews :nsubviews nil
         :subview-type subview-type 
         :common-case-views? common-case-views?  :common-vars? common-vars? 
          keyword-pairs))
  
  ))

(defmethod margin-string-left ((self case-layout))
  (if (and (> (nrows-of self) 1 ) (<= (ncols-of self) 2 ))
    (loop for row in (row-format self)
          for s = (caar row)
          for s1 = (car (subviews-of s))
          collect (if (typep s1 'point-symbol)
                    (label-of s1)
                    (identifier-name (viewed-object-of s1))))
    (if (= 1 (nrows-of self))
      (coord-strings (car (subviews-of self))))))

(defmethod margin-string-right ((self case-layout))
  (if (and (> (nrows-of self) 1 ) (<= (ncols-of self) 2 ))
    (loop for row in (row-format self)
          for s = (caar (last row))
          for s1 = (car (subviews-of s))
          collect (if (typep s1 'point-symbol)
                    (label-of s1)
                    (identifier-name (viewed-object-of s1))))))


(defmethod margin-string-bottom ((self case-layout))
  (if (and (> (ncols-of self) 1 ) (<= (nrows-of self) 2 ))
    (loop for col in (col-format self)
          for s = (caar col)
          for s1 = (car (subviews-of s))
          collect (if (typep s1 'point-symbol)
                    (label-of s1)
                    (identifier-name (viewed-object-of s1))))
    (if (= 1 (ncols-of self))
      (coord-strings (car (subviews-of self))))))


(defmethod margin-string-top ((self case-layout))
  (if (and (> (ncols-of self) 1 ) (<= (nrows-of self) 2 ))
    (loop for col in (col-format self)
          for s = (caar (last col))
          for s1 = (car (subviews-of s))
          collect (if (typep s1 'point-symbol)
                    (label-of s1)
                    (identifier-name (viewed-object-of s1))))))


(defmethod list-case-views ((self case-layout))
  (loop for (s) in (layout-views-of self)
        collect (car (subviews-of s))))

