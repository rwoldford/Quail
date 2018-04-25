;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                            simple-case-object.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright 1993 (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is part of the Views system, a toolkit for interactive statistical graphics.
;;;  
;;;
;;;  Authors:
;;;     C.B. Hurley 1993 George Washington University
;;;     R.W. Oldford 1993
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------

(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(simple-case-object case-data-of identifier-of make-case-object-class)))

(defclass simple-case-object ()
  ((identifier :initform NIL :initarg :identifier :accessor identifier-of)
   (parent-dataset :initform NIL :initarg :parent-dataset :accessor parent-dataset-of)
   (case-data :initform NIL :initarg :case-data :accessor case-data-of)
   (case-vars :initform NIL :initarg :case-vars :accessor case-vars-of))
  (:documentation
   "A simple structure representing a case."))

(defun make-case-object-class(class-name )
  (eval `(defclass ,class-name(simple-case-object) ()
    )))

(defmethod dataset-p ((thing simple-case-object))
  T)


(defmethod eq-dataset ((d1 simple-case-object) (d2 simple-case-object))
  (or (eq d1 d2)
      (eq-dataset (case-data-of d1) (case-data-of d2))))

(defmethod eq-dataset ((d1 simple-case-object) (d2 T))
  (eq-dataset (case-data-of d1) d2))

(defmethod eq-dataset (d1 (d2 simple-case-object))
  (eq-dataset (case-data-of d2) d1))






(defmethod list-cases ((c simple-case-object))
  (list c))



(defmethod list-variates ((c simple-case-object))
  (or (case-vars-of c)
  (list-variates (case-data-of c))))


(defmethod value-of ((c simple-case-object) (v fixnum) &key (default :error) (safe? T) )
  (if (null safe?)
    (qk::eref (case-data-of c) v)
  (let ((case-vals (case-data-of c)))
    (if (>= v (qk::number-of-elements case-vals))
      default
      (qk::eref case-vals v)))))

(defmethod value-of ((c simple-case-object) var &key (default :error) (safe? T))
  (if  (null safe?)
    (qk::eref (case-data-of c) (position var (case-vars-of c) :test #'eq-variates))
  (let* ((case-vals (case-data-of c))
         (ans1 (value-of case-vals var :default :value-of-failed)))
    (if (eq ans1 :value-of-failed)
      (let* ((cv (case-vars-of c))
             (v (position var cv :test #'eq-variates)))
        (if (or (not (integerp v)) (>= v (qk::number-of-elements case-vals)))
          default
          (qk::eref case-vals v)))
      ans1))))




(defmethod display ((c simple-case-object) &rest keyword-args &key  &allow-other-keys)
 (apply #'display (case-data-of c)  keyword-args))


