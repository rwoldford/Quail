;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               range-view.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(range-view )))


(defclass range-view (justification-mixin oriented-line 1d-view simple-view)
  ()
  (:default-initargs :justification :default
                     :orientation :horizontal)
  (:documentation "Draws a line for range of data"))


(defmethod initialize-instance :after  ((self range-view) &key orientation justification) 
  (if (eq justification :default)
    (if (eq orientation :horizontal)
      (setf (justification-of self) :bottom)
      (setf (justification-of self) :right))))


(defmethod compute-line-endpoints ((self range-view) )
  (let* ((br (bounding-region-of self))
         (xmin (left-of br))
         (xmax (right-of br))
         (ymin (bottom-of br))
         (ymax (top-of br))
         pmin pmax)
    (loop for x in (plot-coords-of self)
          for s in (case-status-of self)
          unless (invalid-status-p s) 
          minimize x into minx
          maximize x into maxx
          finally (setq pmin minx pmax maxx))
    
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
               (:bottom (list (list xmax pmin)
                              (list  xmax pmax)))
               (:center (list (list (/ (+ xmin xmax) 2) pmin)
                              (list  (/ (+ xmin xmax) 2) pmax)))))))))

             




(defmethod new-variable :before  ((self range-view) &key )
  (setf (lines-coords-of self) nil))



(defmethod new-case-status ((self range-view)  cases status  &key )
  (declare (ignore cases status))
  (compute-line-endpoints self)
  )


 
