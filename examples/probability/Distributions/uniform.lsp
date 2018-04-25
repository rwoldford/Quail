;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                    The continuous uniform distribution                           
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

;;;  The uniform can also be explored through the help system

(help 'uniform :topic)

;;;
;;;  \subsubsection The uniform distribution
;;;
;;;  In this section, we consider the uniform distribution as represented in 
;;;  Quail.
;;;
;;;  The mathematical form of the distribution is as follows:
;;;
;;;  The uniform(a,b) distribution is a continuous distribution
;;;  positive on (a, b) and 0 elsewhere.
;;;
;;;   pdf:     1   
;;;         --------      x in (a,b), 0 otherwise.
;;;          b - a
;;;
;;;   cdf:     0          x <= a
;;;  
;;;          x - a   
;;;         --------      x in (a,b)
;;;          b - a
;;;
;;;            1          x >= b
;;; 
;;;
;;;  The lower bound a is determined with the function lower-bound-of,
;;;  and the upper bound b with the function upper-bound-of.
;;; 
;;;
;;;  An instance of a uniform (2 4) is created as 

(setf uniform24 (make-instance 'uniform
                  :lower-bound 2
                  :upper-bound 4))

(display uniform24)

;;;
;;;  We can now have this distribution produce the usual calculations:
;;;
;;;   Probability density calculations:

(pdf-at uniform24 3)
(pdf-at uniform24 (list 1 2 2.1 3 3.9 4))
(pdf-at uniform24 (array (list 1 2 2.1 3 3.9 4) :dimensions '(2 3)))

;;;   Cumulative distribution calculations:

(cdf-at uniform24 3)
(cdf-at uniform24 (list 1 2 2.1 3 3.9 4))
(cdf-at uniform24 (array (list 1 2 2.1 3 3.9 4) :dimensions '(2 3)))

;;;   Quantile (inverse cdf) calculations:

(quantile-at uniform24 .3)
(quantile-at uniform24 (list 0.0 .5 1))
(quantile-at uniform24 (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)))

;;;   Pseudo-random values

(random-value uniform24)
(random-value uniform24 10)

;;;  And to make sure these things look right
;;;  generate lots of data

(<- data (random-value uniform24 1000))

;;;  draw the histogram

(<- hist (histogram :data data :var 0 :title "Uniform 2 4 sample"
                     :histogram-scale :density))

;;;  and overlay a view of the density function

(layer-view (interior-view-of hist)
            (function-view :function 
                           #'(lambda (x) (pdf-at uniform24 x))
                           :domain '(2 4)
                           :nlines 100))

;;; For the uniform distribution, wrapper functions for these calculations exist
;;; which reuse the same instance of a uniform distribution changing parameters
;;; as requested.
;;;
;;;   Probability density calculations:

(density-uniform 3 :from 2 :to 4)
(density-uniform (list 1 2 2.1 3 3.9 4 5)
              :from 2 :to 4)
(density-uniform (array '(2 2.1 3 3.9 4 5) :dimensions '(2 3))
              :from 2 :to 4)

;;;   Cumulative distribution calculations:

(dist-uniform 3 :from 2 :to 4)
(dist-uniform (list 1 2 2.1 3 3.9 4 5)
            :from 2 :to 4)
(dist-uniform (array '(2 2.1 3 3.9 4 5) :dimensions '(2 3))
            :from 2 :to 4)

;;;   Quantile (inverse cdf) calculations:

(quantile-uniform .3 :from 2 :to 4)
(quantile-uniform (list 0.0 .5 1)
                :from 2 :to 4)
(quantile-uniform (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3))
                :from 2 :to 4)

;;;   Pseudo-random values

(random-uniform :from 2 :to 4)
(random-uniform :from 2 :to 4 :n 10)

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
