;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               group-plot.lisp
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
;;;     C.B. Hurley 1994 George Washington University
;;;     

(in-package :vw)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(prompt-plot-by)))





(defun prompt-plot-by(&key data views dim overlay? plot-fn)
  "Prompts the user for inputs to construct a group plot~
   of dimension dim (if provided)."
  
  (unless (numberp dim) (setq dim nil))
  (unless views (setq views (top-views)))
  (setq data (or data (selected-dataset views) (choose-dataset)))
    
  (let* (byvars menu-list cases subject-views vars
                batches batch-args colors highlits?
                )
    (multiple-value-setq (cases subject-views) 
      (list-plot-cases views :highlit? t))
    
    (setq highlits? (and cases t))
    (if (null cases) 
      (multiple-value-setq (cases subject-views) 
        (list-plot-cases views)))
    
    
    
    (if (default-group-variable data)
      (setq byvars (list (default-group-variable data))))
    
    (unless byvars
      (setq menu-list (variable-menu-items data))
      (when views
        (setq menu-list (cons (list "Color" :view-color) menu-list))
         
       ;; (if (every  #'(lambda(s) (dataset-with-info-p s))
        ;;            cases)
        ;;  (setq menu-list (cons (list "Cases" :cases) menu-list)))
        )
      
      
      (setq byvars 
            (choose-some-variables nil 1 "Choose one or more by variables"  menu-list)))
    
    
    (cond ((and views (member :view-color byvars))
           (multiple-value-setq (batches colors)
             (batch-plot-cases views :highlit? highlits?))
           (multiple-value-setq (data batches)
             (prompt-plot-batches data batches))
           
           (setq batch-args (list :margin-string-left (mapcar #'color-to-string (car colors))
                                  :data data 
                                  :batches batches)))
          
          ((and views (member :cases byvars))
           
           (setq batches (mapcar #'viewed-object-of subject-views))
           (setq batch-args (list :data data 
                                  :batches batches)))
          
          
          (t 
           
           (multiple-value-setq (data cases vars)
             (prompt-plot-args data cases vars))
           (setq batch-args (list :cases cases 
                                  :data data 
                                  :by byvars))))
    (unless (and *AUTO-SHARE-CASE-VIEWS?* 
                 (= (length cases) (length subject-views)))
      (setq subject-views nil)) 
    (if plot-fn
      (setq batch-args 
            (append (list :subview-type plot-fn) batch-args)))
    
    (cond 
     ((= dim 0)
      (apply #'table-plot batch-args))
     ((and (= dim 1) overlay?))
     ((= dim 1)
      (apply #'batch-plot :box-views? nil
             :subviews `(( :case-views-from ,subject-views
                                            :ordered-case-views? nil))
             :vars vars batch-args))
     
     ((= dim 2) 
      (apply #'batch-plot 
             :subview-superclass '2d-view
             :subviews `((:case-views-from ,subject-views
                                           :ordered-case-views? nil))
             :vars vars batch-args))
     
     
     ((>= dim 3)
      (if (eq plot-fn 'pairs-layout)
        (if vars
        (apply #'batch-plot 
             :subview-type plot-fn :vars vars
             :link-bounds-y? :by-block-row :link-bounds-x? :by-col
             :subviews `(( :case-views-from ,subject-views
                                            :ordered-case-views? nil :title nil))
             batch-args)
        (apply #'batch-plot 
             :subview-type plot-fn 
             :link-bounds-y? :by-block-row :link-bounds-x? :by-col
             :subviews `(( :case-views-from ,subject-views
                                            :ordered-case-views? nil :title nil))
             batch-args))
        (if vars
      (apply #'batch-plot 
             :subview-type plot-fn :vars vars
              :subviews `(( :case-views-from ,subject-views
                                            :ordered-case-views? nil :title nil))
             batch-args)
       (apply #'batch-plot 
             :subview-type plot-fn
              :subviews `(( :case-views-from ,subject-views
                                            :ordered-case-views? nil :title nil))
             batch-args)))))))
