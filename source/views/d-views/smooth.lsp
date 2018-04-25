;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               smooth.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute)
(export '(smooth set-smooth-par simple-smooth smoothed-2d-point-cloud smooth run-fn)))

(defgeneric set-smooth-par (smooth-mixin &key)
  (:documentation "Sets smoothing parameter."))


(defclass smooth-mixin ()
  ((viewed-object 
     :initarg :viewed-object
    :initform nil 
    :accessor viewed-object-of 
    :documentation "Object being viewed")
   (fit-fn :initarg :fit-fn :initform #'running-median :accessor fit-fn-of)
   (smooth-par :initarg :smooth-par :initform 7 :accessor smooth-par-of)
   (smoothed-coords :initarg smoothed-coords :initform nil :accessor smoothed-coords-of)
   )
  
  (:documentation "Computes smoothes coords"))


(defclass smoothed-2d-point-cloud (smooth-mixin 2d-point-cloud ) 
  ((middle-menu :allocation :class :initform nil)
   ))



(defclass simple-smooth (smooth-mixin simple-lines) 
  ((middle-menu :allocation :class :initform nil)
   ))

(defclass smooth (smooth-mixin lines ) 
  ((middle-menu :allocation :class :initform nil)
   ))

  
(defmethod initialize-instance :before  ((self smooth) &key  (simple? t))
  (when (and simple? (eq (class-name (class-of self)) 'smooth))
    (change-class self 'simple-smooth)
    (setf (viewport-compute-method-of self) #'compute-lines-coords-for-viewport)))


(defmethod get-menu-items :around ((self smooth-mixin) (slot-name (eql 'middle-menu)))
  (add-menu-items self   (call-next-method)
  '(("-" nil)
    ("Smooth Fn"  (set-fit-fn ))
    ("Smooth Par"  (set-smooth-par ))
    )))

(defmethod set-fit-fn ((self smooth-mixin) &key new-fn  (draw? t))
  
  (if draw? (erase-view self))
  (if (null new-fn)
    (setf new-fn (wb::prompt-user :type t
                                  :read-type :read
                                  :prompt-string "Enter new smooth function")))
  (setf (fit-fn-of self) (get-function new-fn))
  (setf (smoothed-coords-of self) nil)
  (if (typep self 'lines-mixin)
    (setf (lines-coords-of self) nil))
  (if draw? (draw-view self)))

(defmethod set-smooth-par ((self smooth-mixin) &key new-par  (draw? t))
  
  (if draw? (erase-view self))
  (if (null new-par)
    (setf new-par (wb::prompt-user :type t
                                   :read-type :eval
                                   :prompt-string "Enter new smoothing parameter")))
  (setf (smooth-par-of self) new-par)
  (setf (smoothed-coords-of self) nil)
  (if (typep self 'lines-mixin)
    (setf (lines-coords-of self) nil))
  (if draw? (draw-view self)))


(defmethod new-case-status :before ((self smooth-mixin)  cases status  &key )
  (declare (ignore cases status))
  (setf (smoothed-coords-of self) nil)
  )

(defmethod new-variable :before  ((self smooth-mixin) &key )
  (setf (smoothed-coords-of self) nil))




(defmethod smooth-par-of ((self smooth-mixin))
  (or (slot-value self 'smooth-par)
      (setf (slot-value self 'smooth-par)
            (wb::prompt-user :type t
                             :read-type :eval
                             :prompt-string "Enter smoothing parameter"))))
 
    
(defmethod get-cache-coords ((self smooth-mixin))
  (or (smoothed-coords-of self)
      (setf (smoothed-coords-of self)
            (let* ((raw-coords (call-next-method))
                   (index+raw
                    (loop for s in (case-status-of self)
                          for i upfrom 0
                          for r in raw-coords
                          when (active-status-p s) collect (cons i r)))
                   (sorted-coords (sort (copy-list index+raw) #'< :key #'second))
                   (par (smooth-par-of self))
                   (smooth-y (funcall (fit-fn-of self) 
                                      (mapcar #'second sorted-coords) 
                                      (mapcar #'third sorted-coords)
                                      par)))
              (loop for y in smooth-y
                    for (i) in sorted-coords
                    do
                    (setf (cadr (nth i raw-coords )) y))
              raw-coords))))
    
        


(defun running-median (x y n)
  (let ((n2 (truncate n 2))
        (len (length x))
        (med (get-function 'median)))
        
    (loop for i from 0 below len
          for left = (max 0 (- i n2))
          for right = (min (+ i n2) (- len 1))
          for win = (subseq y left (+ 1 right))
          collect
          (funcall med win))))


(defun run-fn(fname)
  (let ((f (get-function fname)))
  #'(lambda(x y n)
       (let ((n2 (truncate n 2))
        (len (length x))
        )
        
    (loop for i from 0 below len
          for left = (max 0 (- i n2))
          for right = (min (+ i n2) (- len 1))
          for win = (subseq y left (+ 1 right))
          collect
          (funcall f win))))))


(defun runing-median(x y n)
 (let ((n2 (truncate n 2))
        (len (length x))
        (f (get-function 'median))
        )
        
    (loop for i from 0 below len
          for left = (max 0 (- i n2))
          for right = (min (+ i n2) (- len 1))
          for win = (subseq y left (+ 1 right))
          collect
          (funcall f win))))   