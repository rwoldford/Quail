;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                             binary-methods.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1994 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1994.
;;;
;;;
;;;--------------------------------------------------------------------------------
;;;

(in-package :quail)

;;;;;;;;;;;;;
;;;
;;;  dot-times-object

(defmethod dot-times-object ((x-inv inverse-matrix) (y-inv inverse-matrix))
  (let ((x (inverse-object-of x-inv))
        (y (inverse-object-of y-inv))
        )
    (cond
     ((or (eql x y-inv) (eql y x-inv))
      (identity-matrix (first (dimensions-of x-inv))))
     (T
      (inverse (dot-times-object y x))))))

(defmethod dot-times-object :around ((x matrix) (y-inv inverse-matrix))
  (if (eql x (inverse-object-of y-inv))
    (identity-matrix (first (dimensions-of x)))
    (call-next-method)))

(defmethod dot-times-object :around ((x-inv inverse-matrix) (y matrix))
  (if (eql y (inverse-object-of x-inv))
    (identity-matrix (first (dimensions-of y)))
    (call-next-method)))

(defmethod-multi dot-times-object
  ((x-inv inverse-matrix)
   (y (sequence array dimensioned-ref-object)))
  (solve (inverse-object-of x-inv) y))

(defmethod-multi dot-times-object
  ((y (sequence array dimensioned-ref-object))
   (x-inv inverse-matrix))
  (tp (solve (tp (inverse-object-of x-inv))
             (tp y))))


;;;;;
;;;  now for identity matrices
;;;

(defmethod-multi dot-times-object
  ((Ik identity-matrix)
   (y (sequence array dimensioned-ref-object)))
  (if (eq (first (dimensions-of y))
          (second (dimensions-of Ik)))
    y
    (call-next-method)))

(defmethod-multi dot-times-object
  ((y (array dimensioned-ref-object))
   (Ik identity-matrix))
  (if (eq (second (dimensions-of y))
          (first (dimensions-of Ik)))
    y
   (call-next-method)))
