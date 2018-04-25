;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                       The exponential distribution                           
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
;;;  \subsubsection The exponential distribution
;;;
;;;  In this section, we consider the exponential distribution as represented in 
;;;  Quail.
;;;  The exponential can also be explored through the help system

(help 'exponential :topic)

;;;
;;;  The mathematical form of the location scale exponential distribution with 
;;;  location u and scale s is as follows:
;;;
;;;
;;;  pdf at x:
;;;            for x > u
;;;
;;;                 1         - ((x - u) / s)
;;;                ---  *   e
;;;                 s   
;;;     
;;;            0 otherwise
;;;
;;; 
;;;  cdf at x:   
;;;            for x > u
;;;                        - ((x - u) / s)     
;;;                 1  -  e  
;;;     
;;;            0 otherwise
;;;   
;;;  random value :    u - s * ln ( uniform )
;;;
;;;  quantile at p :   u - s * ln ( 1 - p )
;;;
;;;  An instance of a standard exponential (location 0, scale 1)

(setf exponential (make-instance 'exponential-dist))

(display exponential)

;;;
;;;  We can now have this distribution produce the usual calculations:
;;;
;;;   Probability density calculations:

(pdf-at exponential 3)
(pdf-at exponential (list 0.0 5 10))
(pdf-at exponential (array '(0.0 2.5 5 7.5 9.75 10) :dimensions '(2 3)))

;;;   Cumulative distribution calculations:

(cdf-at exponential 3)
(cdf-at exponential (list 0.0 5 10))
(cdf-at exponential (array '(0.0 2.5 5 7.5 9.75 10) :dimensions '(2 3)))

;;;   Quantile (inverse cdf) calculations:

(quantile-at exponential .3)
(quantile-at exponential (list 0.0 .5 1))
(quantile-at exponential (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)))

;;;   Pseudo-random values

(random-value exponential)
(random-value exponential 2)

;;;  And to make sure these things look right
;;;  generate lots of data

(<- data (random-value exponential 1000))

;;;  draw the histogram

(<- hist (histogram :data data :var 0 :title "exponential sample"
                     :histogram-scale :density))

;;;  and overlay a view of the density function

(layer-view (interior-view-of hist)
            (function-view :function 
                           #'(lambda (x) (pdf-at exponential x))
                           :nlines 100
                           :domain '(0 10)))

;;;
;;; We could also create a location scale exponential:
;;;

(<- exponential-l-s (make-instance 'exponential-dist :location 10 :scale 5))
(display exponential-l-s)

;;; For the exponential distribution, wrapper functions for these calculations exist
;;; which reuse the same instance of a exponential distribution changing parameters
;;; as requested.
;;;
;;;   Probability density calculations:

(density-exponential 3)
(density-exponential 35 :location 10 :scale 5)

(density-exponential (list 0.0 5 10))
(density-exponential (array '(0.0 2.5 5 7.5 9.75 10) :dimensions '(2 3)))

;;;   Cumulative distribution calculations:

(dist-exponential 3)
(dist-exponential 35 :location 10 :scale 5)

(dist-exponential (list 0.0 5 10))
(dist-exponential (array '(0.0 2.5 5 7.5 9.75 10) :dimensions '(2 3)))
;;;   Quantile (inverse cdf) calculations:

(quantile-exponential .3)
(quantile-exponential .3 :location 100)
(quantile-exponential .3 :scale 100)
(quantile-exponential .3 :location 100 :scale 100)
(quantile-exponential (list 0.0 .5 1))
(quantile-exponential (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)))

;;;   Pseudo-random values

(random-exponential)
(random-exponential :n 10)
(random-exponential :n 10 :location 100)
(random-exponential :n 10 :scale 100)

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
