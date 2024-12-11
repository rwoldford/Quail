;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               log-gamma.lisp                              
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
;;;
;;;-------------------------------------------------------------------------------
;;;
;;; This file contains the CL implementation of log-gamma function
;;;

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute)
   (export '(log-gamma gamma)))

;;;------------------------------------------------------------------------------
;;;
;;; LOG-GAMMA
;;;
;;;------------------------------------------------------------------------------

(proclaim '(sb-ext:maybe-inline log-gamma)) ;24NOV2024
(defun log-gamma (x)
   ;(declare (special pi)
 ;)
  "Returns the natural logarithm of the gamma function evaluated at x>0.  ~
   If 0<x<1, the reflection formula is used. Source: Numerical Recipes."
  
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  (cond
   ((not (numberp x)) (quail-error "~s is not a number!" x))
   ((<= x 0) (quail-error "Argument must be positive: ~s" x))
   ((< x 1) (-  (+ (log pi) (log (- 1 x)))                ; Reflection formula
                (+ (log (sin (* pi (- 1 x))))             ; Eqn (6.1.4) in NR
                   (log-gamma (- 2 x)))))
   (t
    (let* ((coefs (make-array 6
                    :element-type 'double-float
                    :initial-contents
                    '(76.18009173D0 -86.50532033D0 24.01409822D0
                      -1.231739516D0 0.120858003D-2 -0.536382D-5)))
          (stp 2.50662827465D0)
          (z (- x 1D0))
          (temp1 (+ z 5.5D0))
          (temp (- (* (+ z 0.5D0)
                      (log temp1))
                   temp1)))
      (+ temp
         (log (* stp
                 (+ 1D0
                    (loop for i from 0 to 5
                       sum (progn ()
                              (setf z (+ z 1D0))
                              (/ (aref coefs i) z)))))))))))

(defun gamma (x)
  "Returns the gamma function evaluated at x > 0."
  (exp (log-gamma x)))
