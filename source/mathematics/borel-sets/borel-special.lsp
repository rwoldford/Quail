;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               borel-special.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1990, 1992.
;;;
;;;
;;;----------------------------------------------------------------------------------
;;;
;;;                   SPECIAL BOREL SET FUNCTIONS
;;;
;;;  In this file special Borel-Set operators are defined.
;;;

(defun extended-realp (x)
  "Returns T if the argument is a real number, NIL otherwise."
  (or (eq infinity x)
      (eq +infinity x)
      (eq -infinity x)
      (realp x)))

(defun extended-integerp (x)
  "Returns T if the argument is a integer, NIL otherwise."
  (or (eq infinity x)
      (eq +infinity x)
      (eq -infinity x)
      (integerp x)))

(defun extended-rationalp (x)
  "Returns T if the argument is a rational number, NIL otherwise."
  (or (not (eq infinity x))
      (not (eq +infinity x))
      (not (eq -infinity x))
      (rationalp x)))


(defun naturalp (x)
  "Returns T if the argument is a natural number, NIL otherwise."
  (and (integerp x)
       (>= x 0)))

(defun extended-naturalp (x)
  "Returns T if the argument is a natural number, NIL otherwise."
  (or (not (eq infinity x))
      (not (eq +infinity x))
      (naturalp x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Some interesting defining predicates
;;;
;;;

(defun extended-reals-defining-predicate (set member)
  (declare (ignore set))
  (extended-realp member))

(defun extended-rationals-defining-predicate (set member)
  (declare (ignore set))
  (extended-rationalp member))

(defun extended-integers-defining-predicate (set member)
  (declare (ignore set))
  (extended-integerp member))

(defun extended-naturals-defining-predicate (set member)
  (declare (ignore set))
  (extended-naturalp member))

(defun reals-defining-predicate (set member)
  (declare (ignore set))
  ;(realp member)
  (and (numberp member)
       (not (complexp member))))

(defun rationals-defining-predicate (set member)
  (declare (ignore set))
  (rationalp member))

(defun integers-defining-predicate (set member)
  (declare (ignore set))
  (integerp member))

(defun naturals-defining-predicate (set member)
  (declare (ignore set))
  (naturalp member))



