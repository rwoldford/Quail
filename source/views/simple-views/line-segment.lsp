;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               line-segment.lisp
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
;;;     C.B. Hurley 1988-1992 George Washington University
;;;     R.W. Oldford 1988-1991
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------

(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '( line-segment 
            line-segment-wmenu 
           set-endpoints
            )))

(defclass line-segment (line-segment-mixin simple-view)
  ()
  (:default-initargs :menu? t
    :documentation "a line segment between two endpoints"))

(defclass line-segment-mixin (flip-mixin lines-mixin)
  ((lines-coords :initarg :endpoints
                 :initform '((0 0) (1 1))
                 :accessor lines-coords-of))
  )

(defclass line-segment-wmenu (line-segment)
  ((middle-menu :allocation :class :initform nil))
  (:documentation "a line segment between two endpoints,~
                   with a menu for changing the points"))



(defgeneric set-endpoints (line-segment-mixin &key )
  (:documentation "sets the end points of line"))

(defmethod initialize-instance :before  ((self line-segment) &key  menu?)
  (if (and menu? (eq (class-name (class-of self)) 'line-segment))
    (change-class self 'line-segment-wmenu)))

(defmethod compute-bounding-region ((self line-segment-mixin) )
  (let (  left right bottom top)
    (loop for (x y) in (lines-coords-of self)
          when x
          minimize x into minx
          maximize x into maxx
          minimize y into miny
          maximize y into maxy
          finally (setq left minx right maxx bottom miny top maxy))
    (when (= left right)
      (let ((delta (draw-style self :width)))
        (cond
         (delta
          (decf left delta)
          (incf right delta))
         (T
          (decf left)
          (incf right))
         )))
    (when (= bottom top)
      (let ((delta (draw-style self :width)))
        (cond
         (delta
          (decf bottom delta)
          (incf top delta))
         (T
          (decf bottom)
          (incf top))
         ))
      )
    (make-region left
                 right
                 bottom
                 top)))
    

(defmethod get-menu-items ((self line-segment-wmenu)
                           (slot-name (eql 'middle-menu)))
  '(("-" nil)
    ( "Endpoints"  (set-endpoints))
    ))

(defmethod set-endpoints ((self line-segment) &key endpoints (draw? t))
  (if draw? (erase-view self))
  (if (null endpoints)
    (setf endpoints 
          (list
           (wb::prompt-user :result-type 'list :read-type :read
                                 :prompt-string "Enter first point as list")
           (wb::prompt-user :result-type 'list :read-type :read
                                 :prompt-string "Enter second point as list"))))
  (setf (lines-coords-of self) endpoints)
  (if draw? (draw-view self)))
