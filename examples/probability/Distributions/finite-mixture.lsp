;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                     The finite mixture distribution                           
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1995 Statistical Computing Laboratory, University Of Waterloo
;;;
;;;
;;;  Authors:
;;;      R.W. Oldford  1995.
;;;

(in-package :quail-user)

;;;
;;;  \subsubsection The finite-mixture distribution
;;;
;;;  In this section, we consider the finite-mixture distribution as represented in 
;;;  Quail.
;;;  The finite-mixture can also be explored through the help system

(help 'finite-mixture :topic)

;;;  The finite-mixture is taken here to have density function f(x) and
;;;  distribution function F(x) defined as a linear combination of n separate
;;;  density functions f1, f2, ..., fn and n separate distribution functions
;;;  F1, F2, ..., Fn respectively where the coefficients in each case are
;;;  probabilities p1, p2, ...,pn that sum to one.  More precisely,
;;;
;;;   1. Parameters: p1, p2,...,pn denote the mixing probability 
;;;                                 where p1 + p2 +...+ pn = 1.
;;;                  f1, f2,...,fn denote the corresponding pdf.
;;;
;;;   2. pdf: f(x) = p1*f1(x) + p2*f2(x) + ... + pn*fn(x)
;;;
;;;   3. cdf: F(x) = p1*F1(x) + p2*F2(x) + ... + pn*Fn(x)
;;;
;;;   4. Range: Suppose X1,X2,...,Xn denote the sets of random variable
;;;              space for the corresponding pdf's. Then, the space for 
;;;              the mixture is UNION of Xi, for i from  1 to n.
;;;
;;;
;;;
;;;
;;;  An instance of a finite-mixture can be constructed in a few steps as
;;;  follows:


(setf f1 (make-instance 'gaussian-dist :location 10 :scale 1))
(setf f2 (make-instance 'gaussian-dist :location 0 :scale 1))
(setf f3 (make-instance 'gaussian-dist :location -10 :scale 3))

(setf finite-mixture
      (make-instance 'finite-mixture :distributions (list f1 f2 f3)
                     :mixing-probs (list .1 .8 .1)))

(display finite-mixture)

;;; The following functions will access the information

(lower-bound-of finite-mixture)
(upper-bound-of finite-mixture)


;;;
;;;  We can now have this distribution produce the usual calculations:
;;;
;;;   Probability density calculations:

(pdf-at finite-mixture 3)
(pdf-at finite-mixture (list 0.0 0.5 1 2 3 4 5 6 7 8 9 10 11))
(pdf-at finite-mixture (array '(0 1 2 3 4 5) :dimensions '(2 3)))

;;;   Cumulative distribution calculations:

(cdf-at finite-mixture 3)
(cdf-at finite-mixture (list -1 0 .5 1 2 3 4 5 6 7 8 9 10 11))
(cdf-at finite-mixture (array '(0 1 2 30 40 50) :dimensions '(2 3)))

;;;   Quantile (inverse cdf) calculations:

(quantile-at finite-mixture .3)
(quantile-at finite-mixture (list 0.0 .5 1))
(quantile-at finite-mixture
             (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)))

;;;   Pseudo-random values

(random-value finite-mixture)
(random-value finite-mixture 10)

;;;  And to make sure these things look right
;;;  generate lots of data

(<- data (random-value finite-mixture 1000))

;;;  draw the histogram

(<- hist
    (histogram :data data :var 0
                :title
                (format NIL "~s indep finite-mixtures"
                        (number-of-elements data))
                :histogram-scale :density))
 
;;; For the finite-mixture distribution, no wrapper functions for these calculations exist
;;; because the difference between two finite mixtures can be so great.
;;;

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
