;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               interval-view.lisp
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
;;;
;;;
;;;----------------------------------------------------------------------------------

(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(interval-view )))


(defclass interval-view (justification-mixin oriented-line 1d-view simple-view)
  ((min-fn :initarg :min-fn 
           :initform #'(lambda(n) (apply #'min n))
           :accessor min-fn-of)
   (max-fn :initarg :max-fn 
           :initform #'(lambda(n) (apply #'max n)) 
           :accessor max-fn-of))
  (:default-initargs :justification :default
                     :orientation :horizontal)
  (:documentation "Draws a line for range of data"))





(defmethod compute-line-endpoints ((self interval-view) )
  (if (eq (justification-of self) :default)
    (if (eq (orientation-of self) :horizontal)
      (setf (justification-of self) :bottom)
      (setf (justification-of self) :right)))

  (let* ((br (bounding-region-of self))
         (xmin (left-of br))
         (xmax (right-of br))
         (ymin (bottom-of br))
         (ymax (top-of br))
         (plot-coords (active-members self (plot-coords-of self)))
         (pmin (funcall (min-fn-of self) plot-coords))
         (pmax (funcall (max-fn-of self) plot-coords)))
   
    
    (setf (lines-coords-of self)
          (ecase (orientation-of self) 
            (:horizontal 
             (ecase (justification-of self)
               (:top (list (list pmin ymax)
                           (list pmax ymax)))
               (:bottom (list (list pmin ymin)
                              (list pmax ymin)))
               (:center (list (list pmin (/ (+ ymin ymax) 2))
                              (list pmax (/ (+ ymin ymax) 2))))))
            (:vertical 
             (ecase (justification-of self)
               (:left (list (list xmin pmin)
                            (list  xmin pmax)))
               (:right (list (list xmax pmin)
                              (list  xmax pmax)))
               (:center (list (list (/ (+ xmin xmax) 2) pmin)
                              (list  (/ (+ xmin xmax) 2) pmax)))))))))

             




(defmethod new-variable :before  ((self interval-view) &key )
  (setf (lines-coords-of self) nil))



(defmethod new-case-status ((self interval-view)  cases status  &key )
  (declare (ignore cases status))
  (compute-line-endpoints self)
  )


 
