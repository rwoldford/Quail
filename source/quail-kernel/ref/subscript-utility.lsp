;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                           subscript-utility.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     M.E. Lewis 1992.
;;;     R.W. Oldford 1992.
;;;
;;;
;;;----------------------------------------------------------------------------

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '()))

(defun next-subscripts (subscripts dimensions order)
  "Returns the subscripts of the next element, ~
   in row or column major order."
  (ecase order
    (:row
     (reverse (next-subscripts (reverse subscripts)
                               (reverse dimensions) 
                               :col)))
    (:col
     (cond ((null subscripts) nil)
           (t
            (if (< (car subscripts) (- (car dimensions) 1))
              (cons (+ (car subscripts) 1) (cdr subscripts))
              (cons 0
                    (next-subscripts (cdr subscripts)
                                     (cdr dimensions)
                                     order))))))))

(defun prior-subscripts (subscripts dimensions order)
  "Returns the subscripts of the prior element, ~
   in row or column major order."
  (ecase order
    (:row
     (reverse (prior-subscripts (reverse subscripts)
                                (reverse dimensions)
                                :col)))
    (:col
     (cond ((null subscripts) nil)
           (t
            (if (> (car subscripts) 0)
              (cons (- (car subscripts) 1) (cdr subscripts))
              (cons (- (car dimensions) 1)
                    (prior-subscripts (cdr subscripts)
                                      (cdr dimensions)
                                      order))))))))

(defun index-to-subscripts (index dimensions order)
  "Returns the subscripts of the element at location index, ~
   counting in row or column major order."
  (labels ((f (index dimensions-product)
             (cond ((null dimensions-product) nil)
                   ((= (length dimensions-product) 1) (list index))
                   (t
                    (multiple-value-bind (quotient remainder)
                                         (floor index (car dimensions-product))
                      (cons quotient
                            (f remainder 
                               (cdr dimensions-product)))))))) 
    (ecase order
      (:col (reverse (index-to-subscripts index (reverse dimensions) :row)))
      (:row (cdr (f index (cum-prod dimensions)))))))

(defun cum-prod (seq)
  "Running cumulative products."
  (labels ((f (seq result)
             (cond ((null seq) result)
                   ((null result) (f (cdr seq) (list (car seq) 1)))
                   (t (f (cdr seq) (cons (* (car seq) (car result)) result))))))
    (f (reverse seq) nil)))


(defun expand-subscripts
       (subscripts f-slices &optional (counter 0))
  (cond ((and (null subscripts)
              (null f-slices))
         NIL)
        ((null f-slices) subscripts)
        ((null subscripts) (make-list (length f-slices) :initial-element T))
        ((= counter (car f-slices)) 
         (cons T
               (expand-subscripts subscripts (cdr f-slices) (+ counter 1))))
        (t
         (cons (car subscripts)
               (expand-subscripts (cdr subscripts)
                                  f-slices
                                  (+ counter 1))))))


