;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               data-ref-arrays.lisp
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;  DATASETS AS ref-arrayS
;;;
;;;  First index (row if a matrix) identifies the case.
;;;  Other dimensions used to identify variates.
;;;
;;;  ... rwo
;;;

(defmethod make-dataset-from-vars ((var quail-kernel:ref-array) &rest other-vars)
  ;; this doesn't do what I'd anticipate- create a 2d-array by glueing
  ;; together 2 or more 1d-arrays colwise    cbh 2/17/93
  ;; Works for me! .... rwo Feb 10 94
  (apply #'quail-kernel:cglue var other-vars ))

(defmethod make-data-subset ((d quail-kernel:ref-array) case-list &key) 
  (if case-list
    (let ((specs (if (typep (first case-list) 'quail-kernel::dimensioned-ref-object)
                   (qk::specs-of (first case-list)))))
      (setq specs (and specs (listp specs) (copy-list specs)))
     (if specs
       (progn
         (loop for c in (rest case-list)
              for c-specs = (qk::specs-of c)
              do
              (loop for i from 0 to (- (length specs) 1)
                    for a = (elt c-specs i)
                    do
                    (cond
                     ((eq (elt specs i) T) NIL)
                     ((eq a T) (setf (elt specs i) T))
                     (T (setf (elt specs i)
                              (append (elt specs i) a))))))
        (if (qk::direct-ref-p d)
          (apply #'qk::ref d (append specs (list :shape t)))
          (apply #'qk::ref (qk::ref-obj-of d) (append specs (list :shape t)))))
      (apply #'qk::rglue case-list)))))


(defmethod dataset-p ((d quail-kernel:ref-array))
  T)
 
(defmethod eq-dataset ((d1 quail-kernel:ref-array) (d2 quail-kernel:ref-array))
 ;; (quail-kernel:ref-eq d1 d2) changed this 6/16/97 cbh 
  (eq d1 d2))




(defmethod list-cases ((d quail-kernel:ref-array))
  (let ((dim (quail-kernel:dimensions-of d)))
    (if dim
      (loop for i from 0 below (first dim)
            collect (quail-kernel:ref d i))
      (list d)))
  )

(defmethod extract-case ((d quail-kernel:ref-array) (index integer))
  (quail-kernel:ref d index))


(defmethod list-variates ((d quail-kernel:ref-array))
  (let ((p (second (quail-kernel:dimensions-of d))))
    (if p
      (loop for i from 0 below p collect i)
      (list 0)
      )))




(defmethod identifier-of ((c quail-kernel:ref-array))
  NIL)
#|
(defmethod value-of ((c quail-kernel:ref-array) var &key)
  (quail-kernel:eref c var))
|#


#|

(defmethod case-val ((c quail-kernel:ref-array) (var number) &key (default var))
  (multiple-value-bind (result error?)
                       (ignore-errors (quail-kernel:eref c var))
    (if error?
      (if (eq default :error)
        (quail-error "~A has no value for ~A" c var)
        default)
      result)))
|#

(defmethod value-of ((c quail-kernel:ref-array) (var fixnum) &key (default :error))
  (multiple-value-bind (result error?)
                       (ignore-errors (quail-kernel:eref c var))
    (if error?
      default
      result)))