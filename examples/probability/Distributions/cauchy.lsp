;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                       The Cauchy distribution                           
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
;;;  \subsubsection The Cauchy distribution
;;;
;;;  In this section, we consider the Cauchy distribution as represented in 
;;;  Quail.
;;;  The cauchy distribution can also be explored through the help system

(help 'cauchy :topic)

;;;
;;;  The Cauchy (or Witch of Agnesi) is a Student t distribution on one 
;;;  degree of freedom.
;;;  The mathematical form of the cauchy distribution is:
;;;
;;;
;;;  pdf at x:           1
;;;               ---------------       
;;;                          2
;;;                pi *(1 + x  )
;;;     
;;;
;;; 
;;;
;;;  An instance of a standard cauchy with 10 degrees of freedom

(setf cauchy (make-instance 'cauchy-dist))

(display cauchy)

;;;  Because it is a Student, we could ask its degrees of freedom

(df-of cauchy)

;;;
;;;  We can now have this distribution produce the usual calculations:
;;;
;;;   Probability density calculations:

(pdf-at cauchy 3)
(pdf-at cauchy (list -3 -2 -1 0 1 2 3))
(pdf-at cauchy (array '(-3 -2 -1 1 2 3) :dimensions '(2 3)))

;;;   Cumulative distribution calculations:

(cdf-at cauchy 3)
(cdf-at cauchy (list -3 -2 -1 0 1 2 3))
(cdf-at cauchy (array '(-3 -2 -1 1 2 3) :dimensions '(2 3)))

;;;   Quantile (inverse cdf) calculations:

(quantile-at cauchy .3)
(quantile-at cauchy (list 0.0 .5 1))
(quantile-at cauchy (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)))

;;;   Pseudo-random values

(random-value cauchy)
(random-value cauchy 2)

;;;  And to make sure these things look right
;;;  generate lots of data

(<- data (random-value cauchy 1000))

;;;  draw the histogram

(<- hist (histogram :data data :var 0 :title "Cauchy sample"
                     :histogram-scale :density))

;;;  and overlay a view of the density function

(layer-view (interior-view-of hist)
            (function-view :function 
                           #'(lambda (x) (pdf-at cauchy x))
                           :nlines 100
                           :domain '(-100 100)))

;;; This may not look that good because the Cauchy will produce wild outliers
;;; with high probability.
;;;
;;;
;;; For the cauchy distribution, wrapper functions for these calculations
;;; exist which reuse the same instance of a cauchy distribution changing 
;;; parameters as requested.
;;;
;;;   Probability density calculations:

(density-cauchy 3)
(density-cauchy 1)

(density-cauchy (list 0.0 1 2))
(density-cauchy (array '(-3 -2 -1 1 2 3) :dimensions '(2 3)))

;;;   Cumulative distribution calculations:

(dist-cauchy 3)
(dist-cauchy 10)
(dist-cauchy 100)

(dist-cauchy (list 0.0 5 10))
(dist-cauchy (array '(-3 -2 -1 1 2 3) :dimensions '(2 3)))

;;;   Quantile (inverse cdf) calculations:

(quantile-cauchy .3)
(quantile-cauchy (list 0.0 .5 .9 .99 .999 .9999 1))
(quantile-cauchy (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)))

;;;   Pseudo-random values

(random-cauchy)
(random-cauchy :n 10)

;;;
;;;
;;; The cauchy distribution in Quail has location and scale parameters as
;;; well.

(setf (location-of cauchy) 5)
(setf (scale-of cauchy) 5)

(cdf-at cauchy 5)
(pdf-at cauchy 3)
(random-value cauchy 100)

(dist-cauchy 5 :location 5 :scale 5)
(density-cauchy 5 :location 5 :scale 5)
(quantile-cauchy .5 :location 5 :scale 5)
(random-cauchy :n 3 :location 5 :scale 5)

;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  You can get to the stock continuous distributions by executing

   (edit-file "eg:Probability;Distributions;stock-cts.lsp")

;;;
;;;  or to the stock distributions discussion by executing

   (edit-file "eg:Probability;Distributions;stock.lsp")

;;; 
;;;  or to the overview by executing

   (edit-file "eg:Probability;Distributions;overview.lsp")
