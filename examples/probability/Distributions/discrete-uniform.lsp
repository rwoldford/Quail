;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                     The discrete uniform distribution                           
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1995 Statistical Computing Laboratory, University Of Waterloo
;;;
;;;
;;;
;;;  Authors:
;;;      R.W. Oldford  1995.
;;;

(in-package :quail-user)

;;;
;;;  \subsubsection The discrete-uniform distribution
;;;
;;;  In this section, we consider the discrete-uniform distribution as represented in 
;;;  Quail.
;;;  The discrete-uniform can also be explored through the help system

(help 'discrete-uniform :topic)

;;;  The discrete-uniform is assigns equal probability to each of
;;;  a finite set of values.  The probabilities sum to one over the
;;;  set.
;;;  Without loss of generality, we take the set of values which the random
;;;  variable may take to be the values from lower-bound-of this distribution
;;;  increasing by 1 until the upper-bound-of it is reached.
;;;  The set of values include the end-points.
;;;
;;;  To be precise, when X follows a discrete Uniform distribution
;;;  from a to b, or X ~ UD(a,b), we have the following:
;;;    
;;;     1. Parameters: a = lower bound 
;;;                    b = upper bound
;;;                       where a,b must be integers and b > a.
;;;     2. Range: x =  a,a+1,...,b-1,b
;;;
;;;                       1
;;;     3. pdf: f(x) = -------
;;;                    (b-a+1)
;;;
;;;     4. cdf:          /  0                if x < a
;;;                     /
;;;                    /    x-a+1
;;;             F(x) =<    -------           if a <= x <= b
;;;                    \    b-a+1
;;;                     \  
;;;                      \  1                if x > b
;;;
;;;     5. mean: (b+a)/2
;;;
;;;     6. variance: ((b-a+1)^2 -1 )/12
;;;
;;;
;;;
;;;  An instance of a discrete-uniform from -5 to 5 is created as 

(setf discrete-uniform
      (make-instance 'discrete-uniform :lower-bound -5 :upper-bound 5))

(display discrete-uniform)

;;; The following functions will access the information

(lower-bound-of discrete-uniform)
(upper-bound-of discrete-uniform)

;;; The bounds can be changed but must be done so that the lower never exceeds the 
;;; upper.

(setf (lower-bound-of discrete-uniform) 0)
(setf (upper-bound-of discrete-uniform) 10)

(display discrete-uniform :from -5 :to 20 :new? T
         :title (format NIL "discrete-uniform(~s ~s)" 
                        (lower-bound-of discrete-uniform)
                        (upper-bound-of discrete-uniform)))

;;;
;;;  We can now have this distribution produce the usual calculations:
;;;
;;;   Probability density calculations:

(pdf-at discrete-uniform 3)
(pdf-at discrete-uniform (list 0.0 0.5 1 2 3 4 5 6 7 8 9 10 11))
(pdf-at discrete-uniform (array '(0 1 2 3 4 5) :dimensions '(2 3)))

;;;   Cumulative distribution calculations:

(cdf-at discrete-uniform 3)
(cdf-at discrete-uniform (list -1 0 .5 1 2 3 4 5 6 7 8 9 10 11))
(cdf-at discrete-uniform (array '(0 1 2 30 40 50) :dimensions '(2 3)))

;;;   Quantile (inverse cdf) calculations:

(quantile-at discrete-uniform .3)
(quantile-at discrete-uniform (list 0.0 .5 1))
(quantile-at discrete-uniform
             (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)))

;;;   Pseudo-random values

(random-value discrete-uniform)
(random-value discrete-uniform 10)

;;;  And to make sure these things look right
;;;  generate lots of data

(<- data (random-value discrete-uniform 1000))

;;;  draw the histogram

(<- hist
    (histogram :data data :var 0
                :nbins (+ (- (upper-bound-of discrete-uniform)
                             (lower-bound-of discrete-uniform))
                          2)
                :title
                (format NIL "~s indep UD(~s ~s)s"
                        (number-of-elements data)
                        (lower-bound-of discrete-uniform)
                        (upper-bound-of discrete-uniform))
                :histogram-scale :density))
 
;;; For the discrete-uniform distribution, wrapper functions for these calculations exist
;;; which reuse the same instance of a discrete-uniform distribution changing parameters
;;; as requested.
;;;
;;;   Probability density calculations:

(density-discrete-uniform 30 :from 1 :to 50)
(density-discrete-uniform (list 0 10 20 30) :from 1 :to 50)
(density-discrete-uniform (array '(0 20 30 50 70 100) :dimensions '(2 3))
                          :from 1 :to 50)

;;;   Cumulative distribution calculations:

(dist-discrete-uniform 30 :from 1 :to 50)
(dist-discrete-uniform (list 0 10 20 30) :from 1 :to 50)
(dist-discrete-uniform (array '(0 20 30 50 70 100) :dimensions '(2 3))
                       :from 1 :to 50)

;;;   Quantile (inverse cdf) calculations:

(quantile-discrete-uniform .3 :from 1 :to 50)
(quantile-discrete-uniform (list 0.0 .5 1) :from 1 :to 50)
(quantile-discrete-uniform (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3))
                           :from 1 :to 50)

;;;   Pseudo-random values

(random-discrete-uniform :from 1 :to 50)
(random-discrete-uniform :from 1 :to 50 :n 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  You can get to the stock discrete distributions by executing

   (edit-file "eg:Probability;Distributions;stock-disc.lsp")

;;;
;;;  or to the stock distributions discussion by executing

   (edit-file "eg:Probability;Distributions;stock.lsp")

;;; 
;;;  or to the overview by executing

   (edit-file "eg:Probability;Distributions;overview.lsp")
