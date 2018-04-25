;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               discrete-dist.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1993 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Stat 440/840 students 1992.
;;;     R.W. Oldford 1993
;;;
;;;--------------------------------------------------------------------------------
;;; 

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) 
    (export '(discrete-dist)))

(defclass discrete-dist (prob-measure)
  ()
  (:documentation "A generic discrete probability distribution.")
  )
;;;----------------------------------------------------------------------------------
; General Methods for Discrete Distributions
; 
; 1) Parameter : p = the probability in finding quantile
;                value = the given value of finding in cdf-at and pdf-at
;                x = the variable in the given function 
;
; 2) cdf-at : using the summation to find the cdf-at if the function given is a pdf.
;
; 3) pdf-at :  f(x) = F(x) - F(x-1)
;
; 4) quantile-at : using the FIND-LIMITS to find the range of the quantile and
;                  then using the BISECTION to find out the quantile-at
;
;;;----------------------------------------------------------------------------------  

(defmethod cdf-at ((disc discrete-dist) (value number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  (loop for i from 0 to (floor value)
        sum (pdf-at disc i)))

;; return pdf-at value
(defmethod pdf-at ((disc discrete-dist) (value number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  (with-CL-functions (/ -)
    (- (cdf-at disc value) (cdf-at disc (- value 1)))))

;; return quantile-at value

(defmethod quantile-at ((disc discrete-dist) p &key (start NIL))
  (declare (ignore start)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (multiple-value-bind (l u) (find-limits disc p) 
    (bisection disc p :lower l :upper u)))
