;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               prompt-plot.lisp
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
(eval-when (:compile-toplevel :load-toplevel :execute) (export '( prompt-plot  default-group-variable 
          prompt-variable-display prompt-case-display prompt-case-display-by
           )))

(defgeneric default-group-variable(obj)
  )
  
(defmethod default-group-variable ((self t)) nil)

(defun prompt-plot(&key data views dim plot-fn)
 "Prompts the user for inputs to construct a single plot~
   of dimension dim."
  (let* (cases subject-views args  vars
                  (sel-plot (or views (top-views))))
    (setq data (or data (selected-dataset sel-plot) ))
    
    (multiple-value-setq (cases subject-views) 
      (list-plot-cases sel-plot :highlit? t))
    
    (if (null cases) 
      (multiple-value-setq (cases subject-views) 
        (list-plot-cases sel-plot)))
    (multiple-value-setq (data cases vars)
      (prompt-plot-args data cases vars))
    (if (null data)
      (setq data (choose-dataset)
            cases nil
            subject-views nil
            vars nil))
    
   
      (unless (and *AUTO-SHARE-CASE-VIEWS?* 
                 (= (length cases) (length subject-views)))
      (setq subject-views nil))
    (setq args (list :ordered-case-views? t
                     :case-views-from subject-views
                     :data data :cases cases
                     ))
    

       (if (and dim (listp dim))
      (apply #'xy-layout-plot :xvars (first dim) :yvars (second dim) args)
      (single-plot-by-dim dim
                          :dataset data :cases cases
                          :plot-fn plot-fn :vars vars
                           :args args)
      )))







(defun prompt-variable-display(&key data views)
  "Prompts the user for inputs to construct a variable display"
  ;(declare (ignore ignore)) ;25NOV2024
   (setq views (or views (top-views)))
    (setq data (or data (selected-dataset views) ))
    (let ((cases (or (list-plot-cases views  :highlit? t)
                       (list-plot-cases views))))
    (multiple-value-setq (data cases)
      (prompt-plot-args data cases nil))
    (if (null data)
      (setq data (choose-dataset)))
   (variate-display-list :data data :draw? t)))


(defun prompt-case-display(&key data views)
  "Prompts the user for inputs to construct a case display"
   (setq views (or views (top-views)))
    (setq data (or data (selected-dataset views) ))
    (let ((cases (or (list-plot-cases views  :highlit? t)
                       (list-plot-cases views))))
    (multiple-value-setq (data cases)
      (prompt-plot-args data cases nil))
    (if (null data)
      (setq data (choose-dataset)
            cases nil))
    (case-display-list :data data :draw? t
                       :cases cases
                       )))



(defun prompt-case-display-by(&key data views)
  "Prompts the user for inputs to construct~
   a case display by group."
   (let* ( menu-list byvar 
         cases)
    (setq views (or views (top-views)))
    (setq data (or data (selected-dataset views) ))
   
    (setq cases (or (list-plot-cases views  :highlit? t)
                       (list-plot-cases views)))
    (multiple-value-setq (data cases)
      (prompt-plot-args data cases nil))
    (if (null data)
      (setq data (choose-dataset)
            cases nil))
     (setq menu-list (variable-menu-items data))
    (setq byvar 
          (or (default-group-variable data)
              (choose-some-variables 
               data 1  "Choose group variable(s)" menu-list)))
    
     (batch-display-list 
     :data data :draw? t 
     :by byvar
     :cases cases)))
