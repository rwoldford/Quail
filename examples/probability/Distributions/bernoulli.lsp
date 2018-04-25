;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                       The bernoulli distribution                           
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
;;;  \subsubsection The bernoulli distribution
;;;
;;;  In this section, we consider the bernoulli distribution as represented in 
;;;  Quail.
;;;  The bernoulli can also be explored through the help system

(help 'bernoulli :topic)

;;;
;;;  The mathematical form of the distribution is as follows:
;;;
;;;                x       1 - x
;;;   Pr(X=x) =   p (1 - p)
;;;
;;;                   when  0 <= p <= 1 and x = 0, 1.
;;;
;;;  and zero otherwise.
;;;
;;;  An instance of a bernoulli with parameters p = .5 is created as 

(setf bernoulli (make-instance 'bernoulli :p 0.5))
(display bernoulli)

;;; The following functions will access the information
;;; which can be changed with setf.

(prob-success bernoulli)
(setf (prob-success bernoulli) .75)

(display bernoulli :new? T)

;;;
;;; And change it back.

(setf (prob-success bernoulli) .5)

;;; Because a bernoulli is a special kind of binomial distribution,
;;; it inherits methods from the binomial.

(number-of-trials bernoulli)
(lower-bound-of bernoulli)
(upper-bound-of bernoulli)



;;;
;;;  We can now have this distribution produce the usual calculations:
;;;
;;;   Probability density calculations:

(pdf-at bernoulli 3)
(pdf-at bernoulli (list 0.0 .5 1 2 3))
(pdf-at bernoulli (array '(0 1 2 2 1 0) :dimensions '(2 3)))

;;;   Cumulative distribution calculations:

(cdf-at bernoulli 3)
(cdf-at bernoulli (list -1 0 .5 1 2 3 4 5 6 7 8 9 10 11))
(cdf-at bernoulli (array '(0 1 2 3 4 5) :dimensions '(2 3)))

;;;   Quantile (inverse cdf) calculations:

(quantile-at bernoulli .2)
(quantile-at bernoulli (list 0.0 .5 1))
(quantile-at bernoulli (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)))

;;;   Pseudo-random values

(random-value bernoulli)
(random-value bernoulli 10)

;;;  And to make sure these things look right
;;;  generate lots of data

(<- data (random-value bernoulli 100))

;;;  draw the histogram

(<- hist (histogram :data data :var 0
                     :title "bernoulli(p = 0.5) sample"
                     :histogram-scale :density))

;;;  and overlay a view of the density function

(layer-view (interior-view-of hist)
            (function-view :function 
                           #'(lambda (x) (pdf-at bernoulli x))
                           :domain '(0 2)
                           :nlines 10))

;;; For the bernoulli distribution, wrapper functions for these calculations exist
;;; which reuse the same instance of a bernoulli distribution changing parameters
;;; as requested.
;;;
;;;   Probability density calculations:

(density-bernoulli 3 :p .5)
(density-bernoulli (list 0 .5 1 2 3)  :p .5)
(density-bernoulli (array '(0 1 3 5 7 10) :dimensions '(2 3)) :p .5)

;;;   Cumulative distribution calculations:

(dist-bernoulli 3 :p .5)
(dist-bernoulli (list 0 .5 1 2 3) :p .5)
(dist-bernoulli (array '(0 2 3 5 7 10) :dimensions '(2 3)) :p .5)

;;;   Quantile (inverse cdf) calculations:

(quantile-bernoulli .3 :p .5)
(quantile-bernoulli (list 0.0 .5 1) :p .5)
(quantile-bernoulli (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3))
                   :p .5)

;;;   Pseudo-random values

(random-bernoulli :p .5)
(random-bernoulli :p .5 :n 10)

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
