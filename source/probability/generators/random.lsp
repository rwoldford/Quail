;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               random.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1991.
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(random-number-generator rand seed-of max-pseudo-rand-of qrestart)))

(defclass random-number-generator (quail-object)
  ((seed :accessor seed-of
         :initarg :seed
         :initform NIL)
   (current-value :accessor current-value-of
                  :initform NIL)))


(defgeneric rand (generator &optional quantity)
  (:documentation
   "Return the next pseudo-random number from this generator.  ~
    If quantity is an integer greater then 1, then a vector containing ~
    that meny elements in the pseudo-random sequence is given."))



(defgeneric next (generator)
  (:documentation
   "Return the next pseudo-random number from this generator."))



(defgeneric qrestart (generator)
  (:documentation
   "Restarts the pseudo-random-number generator from its initial state."))

(defgeneric max-pseudo-rand-of (generator)
  (:documentation
   "Restarts the maximum pseudo-random-number that can be possibly generated ~
    by this generator."))

;;;----------------------------------------------------------------------------------
;;;
;;;  The methods
;;;
;;;----------------------------------------------------------------------------------

(defmethod rand ((generator random-number-generator) &optional (quantity 1))
  (if (not (numberp quantity))
    (quail-error "Quantity ~s is not a positive integer!"  quantity))
  (if (> quantity 1)
    (let ((result (array nan :dimensions (list quantity))))
      (dotimes (i quantity) 
        (setf (eref result i) (float (next generator))))
      result)
    (float (next generator))))

(defmethod next ((generator random-number-generator))
  (missing-method 'next generator))

(defmethod qrestart ((generator random-number-generator))
  (setf (current-value-of generator) (seed-of generator)))

(defmethod max-pseudo-rand-of ((generator random-number-generator))
  (missing-method 'max-pseudo-rand-of generator))

(defmethod initialize-instance :after
  ((generator random-number-generator) &key)
  (if (null (seed-of generator))
    (setf (seed-of generator)
          (mod (* (get-internal-real-time)
                  (get-universal-time))
               (max-pseudo-rand-of generator))))
  (qrestart generator))

