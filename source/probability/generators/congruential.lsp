;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               congruential.lisp                               
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(linear-congruential-generator
          multiplicative-generator hull-dobel-gen lewis-goodman-miller-gen
          full-periodp
          )))



;;;---------------------------------------------------------------------------------------
;;;
;;; Standard linear congruential generator shell.  No smarts built in for efficiency.
;;; Best used for classroom demonstrations only.
;;;
;;;---------------------------------------------------------------------------------------


(defclass linear-congruential-generator
  (random-number-generator)
  ((multiplier :reader multiplier-of
               :initarg :multiplier)
   (modulus :reader modulus-of
            :initarg :modulus)
   (increment :reader increment-of
              :initarg :increment)))


(defmethod next ((generator linear-congruential-generator))
  (setf (current-value-of generator)
        (mod (+ (increment-of generator)
                (* (current-value-of generator)
                   (multiplier-of generator)))
             (modulus-of generator))))

(defmethod max-pseudo-rand-of ((generator linear-congruential-generator))
  (modulus-of generator))

              

(defmethod full-periodp ((g linear-congruential-generator))
  (let* ((c (increment-of g))
         (a (multiplier-of g))
         (m (modulus-of g)))
    (if (not (coprimep c m))
      (error "Modulus = ~s  Increment = ~s ... These must be relatively prime!"
             c m))
    (if (and (= (mod m 4) 0)
             (not (= 1 (mod a 4))))
      (error "Since 4 divides the modulus (~s), then the multiplier (~s) must be = 1 mod 4."
             m a))
    (loop for factor in (factor m) when (not (= 1 (mod a factor))) do
          (error "The multiplier (~s) is not = 1 mod ~s and ~s is a prime factor of the modulus ~s"
                 a factor factor m)
          finally (return t))))
      
    
 
;;;----------------------------------------------------------------------------------
;;;
;;; Now some classic generators taken from
;;; Ripley, B.D. (1983) "Computer Generation of Random Variables: A Tutorial"
;;;                     International Statistical Review, 51, pp. 301-319.
;;;
;;; The generators are named as they were identified by Ripley on page 307.
;;; Note all will be slow since the arithmetic is done with bignums in most
;;; CL implementations.
;;;
;;;----------------------------------------------------------------------------------


;;;----------------------------------------------------------------------------------
;;;
;;; The generator called hull-dobell
;;;
;;; modulus = 2^35 
;;; multiplier = 2^7 +1 
;;; increment = 1
;;;
;;; References:
;;;
;;; Hull, T.E. & A.R. Dobell (1964) "Mixed congruential random number generation for
;;;                                  binary machines" J. ACM 11, pp. 31-40.
;;;
;;; MacLaren, M.D. & G. Marsaglia (1965) "Uniform random number generators" J.ACM 12,
;;;                                  pp. 83-89.
;;;
;;;----------------------------------------------------------------------------------

(defclass hull-dobell-gen (linear-congruential-generator)
  ((modulus :reader modulus-of
            :initform 34359738368
            :allocation :class)
   (multiplier :reader multiplier-of
               :initform 129
               :allocation :class)
   (increment :reader increment-of
              :initform 1
              :allocation :class)))

;;;---------------------------------------------------------------------------------------
;;;
;;; Standard multiplicative generator shell.  No smarts built in for efficiency.
;;; Best used for classroom demonstrations only.
;;;
;;;---------------------------------------------------------------------------------------


(defclass multiplicative-generator
  (linear-congruential-generator)
  ((increment :reader increment-of
              :initform 0
              :allocation :class)))

;;;
;;; Make sure zero never gets to be a seed unless explicitly over-written
;;; via setf after initialization is complete.
;;;

(defmethod initialize-instance :after
  ((generator multiplicative-generator) &key)
  (let ((seed (seed-of generator)))
    (if (= seed 0)
      (setf (seed-of generator)
            (loop until (/= seed 0)
                  do
                  (setf seed
                        (mod (+ 1
                                (* 2
                                   (get-internal-real-time)
                                   (get-universal-time)))
                             (modulus-of generator)))
                  finally (return seed)))))
  (qrestart generator))

(defmethod next ((generator multiplicative-generator))
  (setf (current-value-of generator)
        (mod (* (current-value-of generator)
                (multiplier-of generator))
             (modulus-of generator))))


;;;----------------------------------------------------------------------------------
;;;
;;; The generator called lewis-goodman-miller after
;;; Lewis, P.A.P., Goodman, A.S. & J.M. Miller (1969) " A pseudo-random number
;;;     generator for the System/360" I.B.M. Systems Journal, 8, pp. 136-145.
;;;
;;; modulus = 2^31 - 1 = 2147483647
;;; multiplier = 7^5 = 16807
;;; increment = 0
;;;
;;; Also proposed as a portable standard multiplicative generator
;;;  from Park & Miller, 1988, CACM 31, pp. 1192-1201.
;;; We use Park & Miller's algorithm to ensure that no multiplication results
;;; are kept small.
;;;
;;;-----------------------------------------------------------------------------

(defclass lewis-goodman-miller-gen
  (multiplicative-generator)
  ((multiplier :reader multiplier-of
               :initform 16807
               :allocation :class)
   (modulus :reader modulus-of
            :initform 2147483647
            :allocation :class)))


(defmethod next ((generator lewis-goodman-miller-gen))
  (setf (current-value-of generator)
        (let* ((y (or (current-value-of generator)
                      (seed-of generator)))
               (gamma-of-y (- (* 16807
                                 (mod y 127773))
                              (* 2836 (floor y 127773)))))
          (if (> gamma-of-y 0)
            gamma-of-y
            (+ gamma-of-y 2147483647)))))

