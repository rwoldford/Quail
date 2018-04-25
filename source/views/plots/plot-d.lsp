;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               plot-d.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(3d-plot 2d-plot 1d-plot  d-plot )))
             
(defclass d-plot (plot) 
  ((middle-menu :allocation :class :initform nil))
  (:default-initargs 
    ;;:initform-fn #'get-data-inits 
    :initform-fn #'get-dataset-init
    :right-margin-size 0.01))
 


(defclass 3d-plot (plot ) 
  ((middle-menu :allocation :class :initform nil) )
  (:default-initargs 
    :initform-fn #'get-data-inits-3
    ;;:initform-fn #'get-dataset-init
    :default-interior 'rotating-cloud
    :right-margin-size *default-margin-size*
    :left-view nil :bottom-view nil
    :left-label nil :bottom-label nil))
   



(defclass 2d-plot (standard-plot)
  ((middle-menu :allocation :class :initform nil)
   (position-keys :allocation :class 
                  :initform '(:left-view-size :right-view-size 
                              :bottom-view-size :top-view-size
                              :bottom-label-size :top-label-size
                              :left-label-size :right-label-size 
                              :title-height :title-width
                              :left-margin-size :right-margin-size
                              :bottom-margin-size :top-margin-size
                              :gap-x :gap-y :xy-ratio) ))
  (:default-initargs 
     :default-interior '2d-point-cloud
    :initform-fn #'get-data-inits-2
    ;;:initform-fn #'get-dataset-init
    ))
                  

(defclass 1d-plot (standard-plot) 
  ((middle-menu :allocation :class :initform nil))
  (:default-initargs 
     :default-interior 'histogram-view
    :initform-fn #'get-data-inits-1
    ;;:initform-fn #'get-dataset-init
    ))
   
(defmethod prompt-for-plot-interior((self d-plot))
  (choose-view-from-menu 
                    :prompt-string "Select plot interior"
                    :superclass 'd-view))

(defmethod prompt-for-plot-interior((self 3d-plot))
  (choose-view-from-menu 
                    :prompt-string "Select plot interior"
                    :superclass '3d-view))

(defmethod prompt-for-plot-interior((self 2d-plot))
  (choose-views-from-menu 
                    :prompt-string "Select plot interior(s)"
                    :superclass '2d-view))

(defmethod prompt-for-plot-interior((self 1d-plot))
  (choose-views-from-menu 
                    :prompt-string "Select plot interior(s)"
                    :superclass '1d-view))

