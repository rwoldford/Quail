;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                              surface-plot.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is an addition to Window-basics for moving pointclouds
;;;  
;;;
;;;  Authors:
;;;      R.W. Oldford 1992
;;;      P. Poirier 1992
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(surface-plot)))


(defclass surface-plot (plot)  ;; kind-of rotating-plot?
  ((controls :initform nil :accessor controls-of :initarg :controls))
  ;; short-circuit general initialization for data scheme
  (:default-initargs :initform-fn NIL ))

;;; Potentially different initialization scheme
#| #'surface-data-init)
  )
(defun surface-data-init (&key  (data :prompt)
                           (x :prompt)
                           (y :prompt)
                           (surface-heights :prompt)
                           (surface-function :prompt)
                           &allow-other-keys)
  "Initializes the surface data for a surface-plot. ~
   (:key ~
   (:arg surface :prompt The surface object to be produced.)
   (:arg data :prompt A dataset used to choose the x and y coordinates of the surface.) ~
   (:arg x :prompt A 1-dimensional array of the x-coordinates of the surface. ~
                   Or if data is a dataset, this ) ~
   (:arg y :prompt A 1-dimensional array of the x-coordinates of the surface.) ~
   (:arg surface-heights :prompt A 1-dimensional array containing the height of the surface at ~
   every x y pair.  The number of elements in surface-heights is the ~
   product of the number of elements in x and y.  The elements ~
   themselves are arranged varying the elements of y fastest.)
   (:arg surface-function NIL If given a function which when called on each x y pair ~
   will return the corresponding value of surface-heights.) ~
   Keyword argument data could be a dataset and X and Y indices into the dataset.~
   Or x and y are 1-dimensional arrays which identify the x and y grid points ~
   of the surface. ~
   Surface-heights is a vectoand either X or Y are :prompt (the default),~
   the user is prompted to provide a dataset.~
   If X is :prompt the user is prompted for an X variable.~
   If Y is :prompt the user is prompted for a Y variable.~
   If no dataset has been specified, the make-dataset-from-vars function ~
   is used to build a dataset from X and Y, and X and Y becomes the first ~
   and second indices into the dataset obtained by applying the ~
   list-variates function to the dataset."
|#

(defun surface-plot (&rest keyword-pairs
                           &key
                           surface
                           data
                           x
                           y
                           surface-heights
                           surface-function
                           (draw? T))
  "Creates and returns a surface plot object suitable for use in ~
   surface view.  ~
   (:rest ~
   (:arg keyword-pairs NIL Additional optional initialization keyword arguments.)) ~
   (:key ~
   (:arg surface NIL The surface object to be produced.)
   (:arg data NIL A dataset used to choose the x and y coordinates of the surface.) ~
   (:arg x NIL A 1-dimensional array of the x-coordinates of the surface.) ~
   (:arg y NIL A 1-dimensional array of the x-coordinates of the surface.) ~
   (:arg surface-heights NIL A 1-dimensional array containing the height of the surface at ~
   every x y pair.  The number of elements in surface-heights is the ~
   product of the number of elements in x and y.  The elements ~
   themselves are arranged varying the elements of y fastest.)
   (:arg surface-function NIL If given a function which when called on each x y pair ~
   will return the corresponding value of surface-heights.) ~
   (:arg draw? T If non-NIL the surface plot is drawn in a window.) ~
   )"
  (unless surface
    (if (null x) 
      (if (null data)
        (setq x (choose-dataset "Enter the x-grid array."))
        (setq x (choose-variable data 1 "Choose X variable"))))
    (if (null y) 
      (if (null data)
        (setq y (choose-dataset "Enter the y-grid array."))
        (setq y (choose-variable data 1 "Choose Y variable"))))
    (setf surface
          (surface
           x y
           :surface-heights surface-heights
           :surface-function surface-function
           )))
  (let ((surface-plot 
         (apply #'make-surface-plot 'surface-plot
                :surface surface
                keyword-pairs)))
    (if draw?
      (draw-view surface-plot))
    surface-plot))


(eval-when (load eval)
  (setf (symbol-function 'vw::surface-plot) #'surface-plot))

(defun make-surface-plot (plot &rest keyword-pairs)
  (apply #'make-instance plot keyword-pairs))

(defmethod construct-sub-views ((self surface-plot) &rest keyword-pairs
                                &key color-table fast-color-table surface
                                )
    
  (setf (interior-view-of self)
        (apply #'make-instance 'surface-view
               :color-table color-table
               :fast-color-table fast-color-table
               :surface surface
               :viewed-object surface
               keyword-pairs))
    
    (setf (controls-of self)
          (rotate-controls :target
           (interior-view-of self)
           )))

(defmethod init-position-subviews ((self surface-plot)
                                   &key (panel-size 0.1))
  (let ((space-left (make-region (subview-position-region self))))
    (place-subview self (controls-of self)
                   (sub-region space-left :w
                               :width panel-size
                               :remains space-left))
    (place-subview self (interior-view-of self) space-left)))
