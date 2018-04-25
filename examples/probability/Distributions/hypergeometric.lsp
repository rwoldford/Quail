;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                       The hypergeometric distribution                           
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
;;;  \subsubsection The hypergeometric distribution
;;;
;;;  In this section, we consider the hypergeometric distribution as represented in 
;;;  Quail.
;;;  The hypergeometric can also be explored through the help system

(help 'hypergeometric :topic)

;;;  To motivate the hypergeometric, suppose that we have a finite population
;;;  having N > 0 elements and that each element is one of two types -- success
;;;  or failure.  Further, the number of successes in the population is S and
;;;  so the number of failures is N-S.
;;;  
;;;  A sample of size n ( 0 < n <= N) is chosen (without replacement) from this
;;;  population.
;;;  The probability that x of these are successes is
;;;
;;;                / S \  / N-S \
;;;                \ x /  \ n-x /
;;;   Pr(X=x) =  -----------------    for x = 0, 1, 2, ..., min(n,S).
;;;                    / N \
;;;                    \ n /
;;;
;;;    and zero otherwise.
;;;
;;;  In Quail, the hypergeometric is parameterized by the number of successes,
;;;  the number of failures, and the sample size n.
;;;
;;;  An instance of a hypergeometric with 10 successes and 20 failures
;;;  and sample size 8 is created as 

(setf hypergeometric
      (make-instance 'hypergeometric
        :total-successes 10
        :total-failures 20
        :sample-size 8))

(display hypergeometric)

;;; The following functions will access the information

(total-successes-of hypergeometric)
(total-failures-of hypergeometric)
(population-size-of hypergeometric)
(sample-size-of hypergeometric)
(lower-bound-of hypergeometric)
(upper-bound-of hypergeometric)

;;; Some of these can be changed by setfs

(setf (total-successes-of hypergeometric) 15)
(setf (total-failures-of hypergeometric) 30)
(setf (sample-size-of hypergeometric) 16)

(population-size-of hypergeometric)

(display hypergeometric
         :from (lower-bound-of hypergeometric)
         :to (upper-bound-of hypergeometric)
         :new? T)


;;;
;;; And change them back.

(setf (sample-size-of hypergeometric) 8)
(setf (total-successes-of hypergeometric) 10)
(setf (total-failures-of hypergeometric) 20)
;;;
;;;  We can now have this distribution produce the usual calculations:
;;;
;;;   Probability density calculations:

(pdf-at hypergeometric 3)
(pdf-at hypergeometric (list 0.0 .5 1 2 3 4 5 6 7 8 9 10 11))
(pdf-at hypergeometric (array '(0 10 20 30 40 50) :dimensions '(2 3)))

;;;   Cumulative distribution calculations:

(cdf-at hypergeometric 3)
(cdf-at hypergeometric (list -1 0 .5 1 2 3 4 5 6 7 8 9 10 11))
(cdf-at hypergeometric (array '(0 10 20 30 40 50) :dimensions '(2 3)))

;;;   Quantile (inverse cdf) calculations:

(quantile-at hypergeometric .3)
(quantile-at hypergeometric (list 0.0 .5 1))
(quantile-at hypergeometric
             (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)))

;;;   Pseudo-random values

(random-value hypergeometric)
(random-value hypergeometric 10)

;;;  And to make sure these things look right
;;;  generate lots of data

(<- data (random-value hypergeometric 100))

;;;  draw the histogram

(<- hist (histogram :data data :var 0
                     :title "hypergeometric sample"
                     :histogram-scale :density))


;;; For the hypergeometric distribution, wrapper functions for these calculations exist
;;; which reuse the same instance of a hypergeometric distribution changing parameters
;;; as requested.
;;;
;;;   Probability density calculations:

(density-hypergeometric 3
                        :total-successes 10
                        :total-failures 20
                        :sample-size 8)

(density-hypergeometric (list 0 1 2 3)
                        :total-successes 10
                        :total-failures 20
                        :sample-size 8)

(density-hypergeometric (array '(0 2 3 5 7 10) :dimensions '(2 3))
                        :total-successes 10
                        :total-failures 20
                        :sample-size 8)

;;;   Cumulative distribution calculations:

(dist-hypergeometric 3
                     :total-successes 10
                     :total-failures 20
                     :sample-size 8)

(dist-hypergeometric (list 0 1 2 3)
                     :total-successes 10
                     :total-failures 20
                     :sample-size 8)

(dist-hypergeometric (array '(0 2 3 5 7 10) :dimensions '(2 3))
                     :total-successes 10
                     :total-failures 20
                     :sample-size 8)

;;;   Quantile (inverse cdf) calculations:

(quantile-hypergeometric .3
                         :total-successes 10
                         :total-failures 20
                         :sample-size 8)

(quantile-hypergeometric (list 0.0 .5 1)
                         :total-successes 10
                         :total-failures 20
                         :sample-size 8)

(quantile-hypergeometric (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3))
                         :total-successes 10
                         :total-failures 20
                         :sample-size 8)

;;;   Pseudo-random values

(random-hypergeometric :total-successes 10
                       :total-failures 20
                       :sample-size 8)

(random-hypergeometric :n 10
                       :total-successes 10
                       :total-failures 20
                       :sample-size 8)

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
