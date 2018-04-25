;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                       The Poisson distribution                           
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
;;;  \subsubsection The Poisson distribution
;;;
;;;  In this section, we consider the Poisson distribution as represented in 
;;;  Quail.
;;;  The Poisson can also be explored through the help system

(help 'Poisson :topic)


;;;  The mean, m > 0 , parameterizes the standard Poisson distribution.
;;;
;;;  For any Poisson(m) random variable X, the probability that it takes on
;;;  a given value x is
;;;
;;;                 x
;;;                m  exp{-m}
;;;   Pr(X=x) =  -------------      for  x = 0, 1, 2, ...
;;;                    x!
;;;
;;;  and zero otherwise.
;;;
;;;
;;;  An instance of a Poisson with mean 10 is created as 

(setf Poisson (make-instance 'Poisson-dist :mean 10))

(display Poisson)

;;; The following functions will access the information

(mean Poisson)
(lower-bound-of Poisson)
(upper-bound-of Poisson)

;;; The mean can be changed

(setf (mean Poisson) 15)

(display Poisson :from 0 :to 30 :new? T
         :title (format NIL "Poisson(~s)" (mean Poisson)))

;;;
;;;  We can now have this distribution produce the usual calculations:
;;;
;;;   Probability density calculations:

(pdf-at Poisson 3)
(pdf-at Poisson (list 0.0 .5 1 2 3 4 5 6 7 8 9 10 11))
(pdf-at Poisson (array '(0 1 2 3 4 5) :dimensions '(2 3)))

;;;   Cumulative distribution calculations:

(cdf-at Poisson 3)
(cdf-at Poisson (list -1 0 .5 1 2 3 4 5 6 7 8 9 10 11))
(cdf-at Poisson (array '(0 1 2 30 40 50) :dimensions '(2 3)))

;;;   Quantile (inverse cdf) calculations:

(quantile-at Poisson .3)
(quantile-at Poisson (list 0.0 .5 1))
(quantile-at Poisson (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)))

;;;   Pseudo-random values

(random-value Poisson)
(random-value Poisson 10)

;;;  And to make sure these things look right
;;;  generate lots of data

(<- data (random-value Poisson 1000))

;;;  draw the histogram

(<- hist
    (histogram :data data :var 0
                :title
                (format NIL "Poisson(~s) sample" (mean Poisson))
                :histogram-scale :density))
 
;;; For the Poisson distribution, wrapper functions for these calculations exist
;;; which reuse the same instance of a Poisson distribution changing parameters
;;; as requested.
;;;
;;;   Probability density calculations:

(density-poisson 30 :mean 10)
(density-poisson (list 0 10 20 30) :mean 10)
(density-poisson (array '(0 20 30 50 70 100) :dimensions '(2 3))
                 :mean 10)

;;;   Cumulative distribution calculations:

(dist-poisson 30 :mean 10)
(dist-poisson (list 0 10 20 30) :mean 10)
(dist-poisson (array '(0 20 30 50 70 100) :dimensions '(2 3))
              :mean 10)

;;;   Quantile (inverse cdf) calculations:

(quantile-poisson .3 :mean 10)
(quantile-poisson (list 0.0 .5 1) :mean 10)
(quantile-poisson (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3))
                  :mean 10)

;;;   Pseudo-random values

(random-poisson :mean 10)
(random-poisson :mean 10 :n 10)

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
