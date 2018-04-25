;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               mlevel-dataset.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(mlevel-case-object mlevel-dataset-class nest-datasets )))

;; An mlevel dataset class is a dataset, but it has multilevel cases.
;; So, the cases are mlevel-case-objects

(defclass mlevel-case-object  (simple-case-object)
  ((sub-cases :initform NIL :initarg :sub-cases :accessor sub-cases-of)
   (parent-case :initform NIL :initarg :parent-case :accessor parent-case-of))
  (:documentation
   "A  structure representing a multilevel case."))



(defmethod list-variates ((self mlevel-case-object))
  (let ((p (parent-case-of self)))
    (if p
      (append (case-vars-of self))
              (list-variates p))
      (case-vars-of self)))
  




#|
(defmethod case-val ((c mlevel-case-object)
                             (v number) &key (default v))
  (let ((case-vals (case-data-of c)))
    (if (or (not (integerp v)) (>= v (qk::number-of-elements case-vals)))
      (if (eq default :error)
          (quail-error "~A has no value for ~A" c v)
          default)
      (qk::eref case-vals v))))

(defmethod case-val ((self mlevel-case-object)
                             v &key (default v))
  (let ((p (parent-case-of self))
         i)
    (cond ((and p (setq i (position v (case-vars-of p) :test #'eq-identifiers)))
           (case-val p i :default v))
          (p (case-val p v :default v))
          (t (if (eq default :error)
          (quail-error "~A has no value for ~A" self v)
          default)))))
 

|#




(defmethod value-of ((self mlevel-case-object)
                     var &key (default :error))
  (let* ((case-vals (case-data-of self))
         (cv (case-vars-of self))
         (v (position var cv :test #'eq-variates)))
    (if (or (not (integerp v))  (>= v (qk::number-of-elements case-vals)))
    (let* ((p (parent-case-of self))
           (ans (if p
                  (value-of p var :default :handle-error-here))))
      (if (eq ans :handle-error-here)
        default
        ans))
    
    (qk::eref case-vals v))))
 

(defmethod ancestor-data-p((a mlevel-case-object) (b mlevel-case-object))
  (or (eq a (parent-case-of b))
      (eq a (parent-dataset-of b))
      (ancestor-data-p a (parent-case-of b))))

#|


(defmethod contains-case-p((a mlevel-case-object) (b mlevel-case-object) 
                           &key (order? nil))
  (if order?
    (or (eq-dataset a b)
        (ancestor-data-p a b))
    (or (eq-dataset a b)
        (ancestor-data-p a b)
        (ancestor-data-p b a))))


(defmethod contains-data-p((a mlevel-case-object) (b mlevel-case-object)
                           &key (order? nil))
  (or (eq-dataset a b)
      (contains-case-p a b :order? order?)))      
   
|#
(defclass mlevel-dataset-class(dataset-class)
  ())

;; should be able to get the value of a level1 case from a level2 case
;; augment value-of
;; and list-variates.

(defun nest-datasets(dataset sub-dataset &key test nest-sizes)
  (change-class dataset 'mlevel-dataset-class)
  (setf (list-variates sub-dataset) (append (list-variates sub-dataset) (list-variates dataset)))
  (cond ((numberp nest-sizes)
         
  (loop for c in (list-cases dataset)
        for rest-sub-cases on  (list-cases sub-dataset) by #'(lambda(x) (nthcdr nest-sizes x))
        for sub-cases = (subseq rest-sub-cases 0 nest-sizes)
        do
        (change-class c 'mlevel-case-object)
        (setf (sub-cases-of c) sub-cases)
        (loop for s in sub-cases do
            (change-class s 'mlevel-case-object)  
            (setf (parent-case-of s) c))))
        ((functionp test)
         (loop with c = (list-cases dataset)
               for sub in (list-cases sub-dataset)
               for parent = (find sub c :test test)
                when parent do
               (change-class parent 'mlevel-case-object)
               (change-class sub 'mlevel-case-object)
               (setf (parent-case-of sub) parent)
               (push sub (sub-cases-of parent))))
        (t "fill this in")))





