;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               factor.lisp                               
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(smallest-divisor dividesp factor coprimep)))

(defun smallest-divisor (n &key (from 2))
  "Returns the smallest divisor of the integer n that is ~
   greater than the value of the keyword argument from (default 2)."
  (declare (integer n)
           (optimize (speed 3)))
  (if (and (integerp n) (> n 0))
    (if (= n 1)
      1
      (if (or (not (integerp from))
              (< from 2))
        (quail-error "Keyword argument :from must be integer > 1: ~s" from)
        (find-divisor n from)))
    (quail-error "~s is not a postive integer!" n)))

(defun find-divisor (n test-divisor)
  (cond ((>= test-divisor ;;(* test-divisor test-divisor)
            n)
         n)
        ((dividesp test-divisor n) test-divisor)
        (t (find-divisor n (+ test-divisor 1)))))

(defun dividesp (a b)
  "Tests whether the integer a divides the integer b."
  (declare (type integer a b)
           (optimize (speed 3)))
  (= (mod b a) 0))


(defun factor (n)
  "Returns a list of the prime factors of the integer n."
  (declare (type integer n)
           (optimize (speed 3)))
  (cond ((= 0 n) '())
        ((= 1 n) '())
        ((< n 0) (let ((result (factor (abs n))))
                   (setf (first result) (- (first result)))
                   result))
        (t
         (let ((whats-left n)
               (next-value 2))
           (declare (type integer whats-left next-value))
           (loop while (> whats-left 1)
                 collect (let ()
                           (setf next-value
                                 (smallest-divisor whats-left :from next-value))
                           (setf whats-left (/ whats-left next-value))
                            next-value))))))


#| A clearer but slower version:

(defun factor (n)
  (cond ((not (integerp n))
         (quail-error "~s is not an integer!" n))
        ((= 0 n) '())
        ((= 1 n) '())
        ((< n 0) (let ((result (factor (abs n))))
                   (setf (first result) (- (first result)))
                   result))
        (t
         (let ((first-factor (smallest-divisor n)))
           (append (list first-factor)
                   (factor (/ n first-factor)))))))
|#

(defun coprimep (a b)
  (= (gcd a b) 1))
