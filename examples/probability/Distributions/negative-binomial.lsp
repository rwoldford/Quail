;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                       The negative-binomial distribution                           
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
;;;  \subsubsection The negative-binomial distribution
;;;
;;;  In this section, we consider the negative-binomial distribution as represented in 
;;;  Quail.
;;;  The negative-binomial can also be explored through the help system

(help 'negative-binomial :topic)

;;;
;;;  The mathematical form of the distribution is as follows:
;;;
;;;                  (n-1)!     x       n - x
;;;   Pr(N=n) =  ------------- p (1 - p)
;;;              (x-1)! (n-x)!
;;;
;;;                   when  0 <= p <= 1 and n = x, x+1, x+2, ...
;;;
;;;  and zero otherwise.
;;;
;;;  An instance of a negative-binomial with parameters x = 10, p = .5 is created as 

(setf negative-binomial
      (make-instance 'negative-binomial
        :successes 10 :p 0.5))

(display negative-binomial)

;;; The following functions will access the information
;;; which can be changed with setf.

(number-of-successes negative-binomial)
(prob-success negative-binomial)

(setf (number-of-successes negative-binomial) 15)
(setf (prob-success negative-binomial) .75)

(display negative-binomial :from 9 :to 30 :new? T)

;;;
;;; And change them back.

(setf (number-of-successes negative-binomial) 10)
(setf (prob-success negative-binomial) .5)

;;;
;;;  We can now have this distribution produce the usual calculations:
;;;
;;;   Probability density calculations:

(pdf-at negative-binomial 3)
(pdf-at negative-binomial (list 0.0 .5 1 2 3 4 5 6 7 8 9 10 11))
(pdf-at negative-binomial (array '(0 10 20 30 40 50) :dimensions '(2 3)))

;;;   Cumulative distribution calculations:

(cdf-at negative-binomial 3)
(cdf-at negative-binomial (list -1 0 .5 1 2 3 4 5 6 7 8 9 10 11))
(cdf-at negative-binomial (array '(0 10 20 30 40 50) :dimensions '(2 3)))

;;;   Quantile (inverse cdf) calculations:

(quantile-at negative-binomial .3)
(quantile-at negative-binomial (list 0.0 .5 1))
(quantile-at negative-binomial (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)))

;;;   Pseudo-random values

(random-value negative-binomial)
(random-value negative-binomial 10)

;;;  And to make sure these things look right
;;;  generate lots of data

(<- data (random-value negative-binomial 100))

;;;  draw the histogram

(<- hist (histogram :data data :var 0
                     :title "negative-binomial(10, 0.5) sample"
                     :histogram-scale :density))

;;;  and overlay a view of the density function

(layer-view (interior-view-of hist)
            (function-view :function 
                           #'(lambda (x) (pdf-at negative-binomial x))
                           :domain '(10 45)
                           :nlines 10))

;;; For the negative-binomial distribution, wrapper functions for these calculations exist
;;; which reuse the same instance of a negative-binomial distribution changing parameters
;;; as requested.
;;;
;;;   Probability density calculations:

(density-negative-binomial 30 :successes 10  :p .5)
(density-negative-binomial (list 0 10 20 30) :successes 10  :p .5)
(density-negative-binomial (array '(0 20 30 50 70 100) :dimensions '(2 3))
                           :successes 10  :p .5)

;;;   Cumulative distribution calculations:

(dist-negative-binomial 30 :successes 10  :p .5)
(dist-negative-binomial (list 0 10 20 30) :successes 10  :p .5)
(dist-negative-binomial (array '(0 20 30 50 70 100) :dimensions '(2 3))
                        :successes 10  :p .5)

;;;   Quantile (inverse cdf) calculations:

(quantile-negative-binomial .3 :successes 10  :p .5)
(quantile-negative-binomial (list 0.0 .5 1) :successes 10  :p .5)
(quantile-negative-binomial (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3))
                            :successes 10  :p .5)

;;;   Pseudo-random values

(random-negative-binomial :successes 10  :p .5)
(random-negative-binomial :successes 10  :p .5 :n 10)

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
