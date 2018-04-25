;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               menus.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '( make-plot-menu view-menu-list select-view-menu-list )))






(defun make-plot-menu ()
  (let ((menu-items '(
                          ("Scatterplot" (scatterplot))
                          
                          ("Histogram" (histogram))
                          ("Boxplot" (boxplot))
                          ("ScatMatrix" (scat-mat))
                          ("3DPlot" (rotating-plot))
                          ("LinesPlot" (lines-plot))
                          )))
    (wb:make-menu :items menu-items 
                  :title "Plot" 
                  :menu-type :title
                  )
    (wb:menu (wb:make-menu :items menu-items :title "Plot" 
                           :menu-type :title
                           ) )))

(defun view-menu-list (view-symbol &optional fn)
  
  "Returns a menu item list of view classes inheriting from~
   class named by view-symbol"
  (if fn
    (loop for class-name in  (view-classes-with-spec view-symbol)
          collect  
          `(,(format nil "~@(~A~)" class-name) 
            (,fn ,class-name)))
    (loop for class-name in  (view-classes-with-spec view-symbol)
          collect  
          `(,(format nil "~@(~A~)" class-name) 
            ,class-name))))


(defgeneric select-view-menu-list(view-symbol)
  (:documentation "Returns a menu item list of SOME view classes inheriting from~
   class named by view-symbol"))

(defmethod select-view-menu-list(view-symbol)
  (view-menu-list view-symbol))

#|
(eval-when (eval load)
  (make-plot-menu))
|#
