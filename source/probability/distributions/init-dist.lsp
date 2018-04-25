;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               init-dist.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1993 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1993
;;;
;;;
;;;--------------------------------------------------------------------------------
;;;
;;;  Initialize a bunch of variables
;;;

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(*distributions*)))

(defvar *distributions*
  (list
       (setf *uniform* (make-instance 'uniform))
       
       (setf *gamma-dist* (make-instance 'gamma-dist :shape 0.5))
       
       (setf *exponential-dist* (make-instance 'exponential-dist))
       
       (setf *chi-squared* (make-instance 'chi-squared :df 1))
       
       (setf *student* (make-instance 'student :df 1))
       
       (setf *gaussian-dist* (make-instance 'gaussian-dist))
       
       (setf *cauchy* (make-instance 'cauchy-dist))
       
       (setf *binomial* (make-instance 'binomial-dist :upper-bound 1  :p 0.5))
       
       (setf *bernoulli* (make-instance 'bernoulli  :p 0.5))
       
       (setf *negative-binomial* (make-instance 'negative-binomial
                                   :lower-bound 1  :p 0.5))
       
       (setf *geometric* (make-instance 'geometric :p 0.5))
       
       (setf *hypergeometric* (make-instance
                                'hypergeometric :total-successes 10
                                :total-failures 5 :sample-size 7))
       
       (setf *discrete-uniform* (make-instance 'discrete-uniform))
       
       (setf *beta-dist* (make-instance 'beta-dist :shape1 1 :shape2 1))
       
       (setf *poisson* (make-instance 'poisson-dist :mean 1))
       
       (setf *K-dist* (make-instance 'K-dist :df 1))
       
       (setf *F-dist* (make-instance 'F-dist :df-num 1.0 :df-den 1.0))
       
       (setf *pareto* (make-instance 'pareto  :shape 1.0))
       
       (setf *weibull* (make-instance 'weibull  :shape 1.0))
       
       )
  "A collection of instances of selected distributions.")

  
