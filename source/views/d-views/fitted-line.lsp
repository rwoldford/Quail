;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               fitted-line.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(fitted-line 
          compute-slope&intercept set-fit-fn )))

(defgeneric compute-slope&intercept (fitted-line)
  (:documentation "Computes slope and intercept from coords and places in slots"))

(defgeneric set-fit-fn (fitted-line &key )
  (:documentation "sets the fitting function of line"))

(defclass fitted-line (line 2d-view simple-view)
  ((viewed-object 
     :initarg :viewed-object
    :initform nil 
    :accessor viewed-object-of 
    :documentation "Object being viewed")
   (fit-fn :initarg :fit-fn :initform #'sreg :accessor fit-fn-of)
   (middle-menu :allocation :class :initform nil))
  (:default-initargs :slope nil :intercept nil)
  (:documentation "Draws a line fit to coords using fit-fn"))



#|
(defmethod compute-slope&intercept ((self fitted-line))
  (let* ((plot-coords (active-members self (plot-coords-of self)))
         (xs (mapcar #'first plot-coords))
         (ys (mapcar #'second plot-coords)))
    
    (multiple-value-bind (intercept slope ) 
                         (values-list
                          (funcall (fit-fn-of self) xs ys))
      
      
      (setf (intercept-of self) intercept)
      (setf (slope-of self) slope))
    
    ;; sort cases by x
    (let ((vo-ac (active-members self (cases-of self)))
          (vo-inac (inactive-members self (cases-of self)))
          (status-ac (active-members self (case-status-of self)))
          (status-inac (inactive-members self (case-status-of self)))
          result)
      
      (setq result  (sort (mapcar #'list xs vo-ac status-ac)  #'< :key #'car ))
      (setf (cases-of self) (append (mapcar #'second result)
                                            vo-inac))
      (setf (case-status-of self) (append (mapcar #'third result)
                                          status-inac)))))
      
|#      


(defmethod compute-slope&intercept ((self fitted-line))
  (let* ((plot-coords (active-members self (plot-coords-of self)))
         (xs (mapcar #'first plot-coords))
         (ys (mapcar #'second plot-coords)))
    
    (multiple-value-bind (intercept slope ) 
                         (values-list
                          (funcall (fit-fn-of self) xs ys))
      
      
      (setf (intercept-of self) intercept)
      (setf (slope-of self) slope))))
    
    


(defmethod slope-of  ((self fitted-line))
  (unless (slot-value self 'slope)
    (compute-slope&intercept self))
  (slot-value self 'slope))

(defmethod intercept-of  ((self fitted-line))
  (unless (slot-value self 'intercept)
    (compute-slope&intercept self))
  (slot-value self 'intercept))


(defmethod new-variable ((self fitted-line) &key )
  (declare (ignorable self)) ;(declare (ignore self)) ; 29JUL2023
  (call-next-method))

(defmethod new-variable :before  ((self fitted-line) &key )
  (setf (slope-of self) nil)
  (setf (intercept-of self) nil))



(defmethod new-case-status ((self fitted-line)  cases status  &key )
  (declare (ignorable cases status)) ;(declare (ignore cases status)) ; 29JUL2023
  (compute-slope&intercept self)
  (compute-line-endpoints self)
  )


  
(defmethod get-menu-items ((self fitted-line) (slot-name (eql 'middle-menu)))
  '(("-" nil)
    ("Fit Fn"  (set-fit-fn ))
    ("Slope"  (set-line-slope ))
    ("Intercept"  (set-line-intercept )
     )))



(defmethod set-fit-fn ((self fitted-line) &key new-fn  (draw? t))
  
  (if draw? (erase-view self))
  (if (null new-fn)
    (setf new-fn (wb::prompt-user :result-type  t
                                  :read-type :read
                                  :prompt-string "Enter new line function")))
  (setf (fit-fn-of self) (get-function new-fn))
  (compute-slope&intercept self)
  (compute-line-endpoints self)
  (if draw? (draw-view self)))


