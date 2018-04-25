;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                       The geometric distribution                           
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
;;;  \subsubsection The geometric distribution
;;;
;;;  In this section, we consider the geometric distribution as represented in 
;;;  Quail.
;;;  The geometric can also be explored through the help system

(help 'geometric :topic)

;;;
;;;  The mathematical form of the distribution is as follows:
;;;
;;;                        n - 1
;;;   Pr(N=n) =   p (1 - p)
;;;              
;;;
;;;                   when  0 <= p <= 1 and n = 1, 2, 3, ...
;;;
;;;  and zero otherwise.
;;;
;;;  An instance of a geometric with parameters x = 10, p = .5 is created as 

(setf geometric (make-instance 'geometric :p 0.5))

(display geometric)

;;; The following functions will access the information
;;; which can be changed with setf.

(prob-success geometric)
(setf (prob-success geometric) .25)

(display geometric :from 1 :to 15 :new? T)

;;; Because the geometric is a subclass of negative-binomial, we can ask for 

(number-of-successes geometric)

;;; but it cannot be set.

;;;
;;;  We can now have this distribution produce the usual calculations:
;;;
;;;   Probability density calculations:

(pdf-at geometric 3)
(pdf-at geometric (list 0.0 .5 1 2 3 4 5 6 7 8 9 10 11))
(pdf-at geometric (array '(0 10 20 30 40 50) :dimensions '(2 3)))

;;;   Cumulative distribution calculations:

(cdf-at geometric 3)
(cdf-at geometric (list -1 0 .5 1 2 3 4 5 6 7 8 9 10 11))
(cdf-at geometric (array '(0 10 20 30 40 50) :dimensions '(2 3)))

;;;   Quantile (inverse cdf) calculations:

(quantile-at geometric .3)
(quantile-at geometric (list 0.0 .5 1))
(quantile-at geometric (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)))

;;;   Pseudo-random values

(random-value geometric)
(random-value geometric 10)

;;;  And to make sure these things look right
;;;  generate lots of data

(<- data (random-value geometric 100))

;;;  draw the histogram

(<- hist (histogram :data data :var 0
                     :title "geometric sample"
                     :histogram-scale :density))

;;;  and overlay a view of the density function

(layer-view (interior-view-of hist)
            (function-view :function 
                           #'(lambda (x) (pdf-at geometric x))
                           :domain '(1 20)
                           :nlines 20))

;;; For the geometric distribution, wrapper functions for these calculations exist
;;; which reuse the same instance of a geometric distribution changing parameters
;;; as requested.
;;;
;;;   Probability density calculations:

(density-geometric 3 :p .5)
(density-geometric (list 0 1 2 3) :p .5)
(density-geometric (array '(0 2 3 5 7 10) :dimensions '(2 3))
                           :p .5)

;;;   Cumulative distribution calculations:

(dist-geometric 3 :p .5)
(dist-geometric (list 0 1 2 3) :p .5)
(dist-geometric (array '(0 2 3 5 7 10) :dimensions '(2 3))
                        :p .5)

;;;   Quantile (inverse cdf) calculations:

(quantile-geometric .3 :p .5)
(quantile-geometric (list 0.0 .5 1) :p .5)
(quantile-geometric (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3))
                            :p .5)

;;;   Pseudo-random values

(random-geometric :p .5)
(random-geometric :p .5 :n 10)

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
