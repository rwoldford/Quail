;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                       The binomial distribution                           
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
;;;  \subsubsection The binomial distribution
;;;
;;;  In this section, we consider the binomial distribution as represented in 
;;;  Quail.
;;;  The binomial can also be explored through the help system

(help 'binomial :topic)

;;;
;;;  The mathematical form of the distribution is as follows:
;;;
;;;                   n!     x       n - x
;;;   Pr(X=x) =  ---------- p (1 - p)
;;;              x!  (n-x)!
;;;
;;;                   when  0 <= p <= 1 and x = 0, 1, ..., n
;;;
;;;  and zero otherwise.
;;;
;;;  An instance of a binomial with parameters n = 10, p = .5 is created as 

(setf binomial (make-instance 'binomial-dist :upper-bound 10 :p 0.5))
(display binomial)

;;; The following functions will access the information
;;; which can be changed with setf.

(number-of-trials binomial)
(prob-success binomial)

(setf (number-of-trials binomial) 15)
(setf (prob-success binomial) .75)

(display binomial :new? T)

;;;
;;; And change them back.

(setf (number-of-trials binomial) 10)
(setf (prob-success binomial) .5)

;;;
;;;  We can now have this distribution produce the usual calculations:
;;;
;;;   Probability density calculations:

(pdf-at binomial 3)
(pdf-at binomial (list 0.0 .5 1 2 3 4 5 6 7 8 9 10 11))
(pdf-at binomial (array '(0 1 2 3 4 5) :dimensions '(2 3)))

;;;   Cumulative distribution calculations:

(cdf-at binomial 3)
(cdf-at binomial (list -1 0 .5 1 2 3 4 5 6 7 8 9 10 11))
(cdf-at binomial (array '(0 1 2 3 4 5) :dimensions '(2 3)))

;;;   Quantile (inverse cdf) calculations:

(quantile-at binomial .3)
(quantile-at binomial (list 0.0 .5 1))
(quantile-at binomial (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)))

;;;   Pseudo-random values

(random-value binomial)
(random-value binomial 10)

;;;  And to make sure these things look right
;;;  generate lots of data

(<- data (random-value binomial 100))

;;;  draw the histogram

(<- hist (histogram :data data :var 0
                     :title "Binomial(10, 0.5) sample"
                     :histogram-scale :density))

;;;  and overlay a view of the density function

(layer-view (interior-view-of hist)
            (function-view :function 
                           #'(lambda (x) (pdf-at binomial x))
                           :domain '(0 10)
                           :nlines 10))

;;; For the binomial distribution, wrapper functions for these calculations exist
;;; which reuse the same instance of a binomial distribution changing parameters
;;; as requested.
;;;
;;;   Probability density calculations:

(density-binomial 3 :total 10  :p .5)
(density-binomial (list 0 .5 1 2 3) :total 10  :p .5)
(density-binomial (array '(0 2 3 5 7 10) :dimensions '(2 3)) :total 10  :p .5)

;;;   Cumulative distribution calculations:

(dist-binomial 3 :total 10  :p .5)
(dist-binomial (list 0 .5 1 2 3) :total 10  :p .5)
(dist-binomial (array '(0 2 3 5 7 10) :dimensions '(2 3)) :total 10  :p .5)

;;;   Quantile (inverse cdf) calculations:

(quantile-binomial .3 :total 10  :p .5)
(quantile-binomial (list 0.0 .5 1) :total 10  :p .5)
(quantile-binomial (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3))
                   :total 10  :p .5)

;;;   Pseudo-random values

(random-binomial :total 10  :p .5)
(random-binomial :total 10  :p .5 :n 10)

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
