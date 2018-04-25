;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               d-view-mixins.lisp
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
;;;     C.B. Hurley 1992 George Washington University
;;;   
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views )

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(data-extract-mixin ncases data-menu-item-mixin var-menu-items
           case-menu-items )))
           
(defclass data-extract-mixin()
  ((variates :initarg :variates :initform #'list-variates)
   (dataset :initarg :data :initform nil)
   (cases :initarg :subjects :initarg :cases  :initform #'list-cases  ))
  (:default-initargs :labels #'identifier-name
    :value-fn #'value-of
    ))

(defclass data-menu-item-mixin()
  ())

(defgeneric var-menu-items (data-menu-item-mixin &key &allow-other-keys)
  )

(defmethod var-menu-items ((self data-menu-item-mixin) &key target)
  (declare (ignore target)))



(defgeneric case-menu-items (data-menu-item-mixin &key &allow-other-keys)
  )

(defmethod case-menu-items ((self data-menu-item-mixin) &key target)
  (declare (ignore target)))
  

#|
(defun construct-viewed-object-list(data cases)
  "Returns a list of the viewed-objects.~
  These are typically cases."
  (cond 
        
        ((functionp cases) (funcall cases data))
        ((list-of-datasets-p cases) cases)
        ((null data) nil)
        ((dataset-p data) (list-cases data))
         (t (quail-error "Invalid subject argument ~A" cases))))
|#


(defmethod dataset-of ((self data-extract-mixin)) 
  (or (slot-value self 'dataset)
      (let ((v (viewed-object-of self)))
        (if (dataset-p v)
          (setf (slot-value self 'dataset) v)))))

(defmethod variates-of ((self data-extract-mixin)) 
  (let ((v (slot-value self 'variates))
        c ans
        ) 
    (cond ((listp v) v)
          ((setq ans (funcall v (dataset-of self)))
           (setf (slot-value self 'variates) ans))
          ((and (setq c (car (cases-of self)))
                (dataset-p c)
                (typep v 'standard-generic-function)
                (compute-applicable-methods  v  (list c)))
                (setf (slot-value self 'variates) (funcall v c)))
          (t (setf (slot-value self 'variates) nil)))))


(defmethod cases-of ((self data-extract-mixin))
  (let ((c (slot-value self 'cases)))
    (if (and  c (listp c) (not (eql (car c) :by)))
      c
      (let ((d (dataset-of self)))
        (setf (slot-value self 'cases) 
              (cond 
               ((functionp c) (funcall c d))
               ((and (listp c) (eql (car c) :by))
                (funcall (data-subsets-fn (cdr c) ) 
                    d))
                
                ((null d) nil)
               ((dataset-p d) (list-cases d))
               (t nil)))))))
          
(defmethod variate-names-of ((self data-extract-mixin))
  "Returns a list of variate-names."
  (variate-names (variates-of self) ))


(defun variate-names (vars)
  ;; returns a list of variate-names
  (loop for v in vars
        collect (cond ((numberp v)
                       (format nil "var ~D" v))
                      ((typep v '(OR STRING CHARACTER SYMBOL))
                              (string v))
                      (t (format nil "~A" v)))))



