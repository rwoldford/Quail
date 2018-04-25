;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               simple-lines.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(simple-lines )))



(defclass simple-lines (lines-mixin  2d-view simple-view)
  ((order-fn :initform #'< :initarg :order-fn
              :accessor order-fn-of))
  (:documentation "Draws line segments connecting coords ordered by order-fn"))


(defclass multi-style-simple-lines (multi-style-lines-mixin simple-lines)
  ()
  )





(defmethod initialize-instance :before  ((self simple-lines) &key multi-style?) 
  
  (if (and (eq (class-name (class-of self)) 'simple-lines) multi-style?)
    (change-class self 'multi-style-simple-lines)))
    

(defmethod new-variable :before ((self simple-lines) &key  )
  (setf (lines-coords-of self) nil))

(defmethod new-case-status  ((self simple-lines) cases status &key  )
  (declare (ignore cases status))
  (setf (lines-coords-of self) nil))

(defmethod lines-coords-of  ((self simple-lines))
  (declare (ignore args))
  
  (or (slot-value self 'lines-coords)
      (setf (lines-coords-of self) 
            (if (and (order-fn-of self) (plot-coords-of self))
              (let* ((fn (order-fn-of self))
                    (reorder-styles? (and (typep self 'multi-style-lines-mixin)
                                (= (length (viewed-elements-of self))
                                   (length (plot-coords-of self)))))
                    (c (if reorder-styles?
                         (mapcar #'list (plot-coords-of self) (drawing-styles-of self)
                               (viewed-elements-of self))
                         (mapcar #'list (plot-coords-of self)))))

                (flet ((test (a b)
                         (let ((all-nums-a (every #'numberp a))
                               (all-nums-b (every #'numberp b)))
                           (cond ((and all-nums-a all-nums-b)
                                  (funcall fn (car a) (car b)))
                                 (all-nums-a t)
                                 (t nil)))))
                  (setq c (sort  c #'test :key #'car)) 
                  (when reorder-styles?  
                    (setf (drawing-styles-of self) (mapcar #'second c))
                    (setf (viewed-elements-of self) (mapcar #'third c)))
                  (loop for (ci) in c
                        while  (every #'numberp ci)
                        collect ci)))
              (plot-coords-of self)))))

 


(defmethod selected-p ((self simple-lines) viewport location)
   (and (active-viewport-p viewport) (contains-p viewport location)))


