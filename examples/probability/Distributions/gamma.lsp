;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                       The Gamma distribution                           
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
;;;  \subsubsection The Gamma distribution
;;;
;;;  In this section, we consider the Gamma distribution as represented in 
;;;  Quail.
;;;  The gamma can also be explored through the help system

(help 'gamma :topic)

;;;
;;;  The mathematical form of the location scale Gamma distribution with 
;;;  location u, scale s and shape parameter a is as follows:
;;;
;;;
;;;  pdf at x:
;;;            for x > u
;;;
;;;                       1                    (a - 1)       - ((x - u) / s)
;;;                ----------------   *  (x - u)         *  e
;;;                              a 
;;;                Gamma (a) * s   
;;;     
;;;            0 otherwise
;;;
;;;  cdf at x :    This is just the so-called incomplete gamma function,
;;;
;;;                                    y
;;;                         1        /
;;;             P(a,y) = --------   /   exp(-t) t^(a-1) dt
;;;                      gamma(a)  /
;;;                                0
;;;
;;; evaluated at y = (x - u) / s
;;;                                 
;;;    quantile at p :       Applied Statistics   AS91 ... percentiles of chi
;;;
;;;  An instance of a standard Gamma (location 0, scale 1)
;;;  with shape parameter 7 is created as 

(setf gamma (make-instance 'Gamma-dist :shape 7))

(display gamma)

;;;
;;;  We can now have this distribution produce the usual calculations:
;;;
;;;   Probability density calculations:

(pdf-at gamma 3)
(pdf-at gamma (list 0.0 5 10))
(pdf-at gamma (array '(0.0 2.5 5 7.5 9.75 10) :dimensions '(2 3)))

;;;   Cumulative distribution calculations:

(cdf-at gamma 3)
(cdf-at gamma (list 0.0 5 10))
(cdf-at gamma (array '(0.0 2.5 5 7.5 9.75 10) :dimensions '(2 3)))

;;;   Quantile (inverse cdf) calculations:

(quantile-at gamma .3)
(quantile-at gamma (list 0.0 .5 1))
(quantile-at gamma (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)))

;;;   Pseudo-random values

(random-value gamma)
(random-value gamma 2)

;;;  And to make sure these things look right
;;;  generate lots of data

(<- data (random-value gamma 1000))

;;;  draw the histogram

(<- hist (histogram :data data :var 0 :title "Gamma 10 sample"
                     :histogram-scale :density))

;;;  and overlay a view of the density function

(layer-view (interior-view-of hist)
            (function-view :function 
                           #'(lambda (x) (pdf-at gamma x))
                           :nlines 100
                           :domain '(0 20)))

;;;
;;; We could also create a location scale gamma:
;;;

(<- gamma-l-s (make-instance 'gamma-dist :location 10 :scale 5 :shape 10))
(display gamma-l-s)

;;; For the Gamma distribution, wrapper functions for these calculations exist
;;; which reuse the same instance of a Gamma distribution changing parameters
;;; as requested.
;;;
;;;   Probability density calculations:

(density-gamma 3 :shape 10)
(density-gamma 35 :shape 10 :location 10 :scale 5)

(density-gamma (list 0.0 5 10) :shape 10)
(density-gamma (array '(0.0 2.5 5 7.5 9.75 10) :dimensions '(2 3))
              :shape 10)

;;;   Cumulative distribution calculations:

(dist-gamma 3 :shape 10)
(dist-gamma 35 :shape 10 :location 10 :scale 5)

(dist-gamma (list 0.0 5 10) :shape 10)
(dist-gamma (array '(0.0 2.5 5 7.5 9.75 10) :dimensions '(2 3))
              :shape 10)
;;;   Quantile (inverse cdf) calculations:

(quantile-gamma .3 :shape 10)
(quantile-gamma (list 0.0 .5 1)
                 :shape 10)
(quantile-gamma (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3))
                 :shape 10)

;;;   Pseudo-random values

(random-gamma :shape 10)
(random-gamma :shape 10 :n 10)
(random-gamma :shape 2 :n 10)
(random-gamma :shape 0.2 :n 10)

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
