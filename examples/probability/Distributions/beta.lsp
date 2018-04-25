;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                       The Beta distribution                           
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
;;;  \subsubsection The Beta distribution
;;;
;;;  In this section, we consider the Beta distribution as represented in 
;;;  Quail.
;;;  The beta can also be explored through the help system

(help 'beta :topic)

;;;
;;;  The mathematical form of the distribution is as follows:
;;;
;;;  
;;;  First, note that the incomplete-beta function, I(a,b,x), is defined to
;;;  be
;;;                          x
;;;                  1      /   a-1      b-1
;;;  I(a,b,x) =  --------  /   t    (1-t)    dt           (a,b > 0) (0<=x<=1)
;;;               B(a,b)  /
;;;                       0
;;;
;;;  where B(a,b) is the complete beta function
;;;
;;;                1
;;;               /   a-1      b-1
;;;  B(a,b) =    /   t    (1-t)    dt           (a,b > 0)
;;;             /
;;;            0
;;;
;;;         = Gamma(a)*Gamma(b)/Gamma(a+b)
;;;
;;; These are computed with the following functions (here a=2 b=3):

(incomplete-beta 2 3 .5)
(beta 2 3)

;;;  The beta distribution is a continuous distribution positive on (0,1)
;;;  and 0 elsewhere.
;;;  It is parameterized by two shape parameters shape1 = a > 0 
;;;  and shape2 = b > 0.
;;;
;;;   pdf:      
;;;             1      a-1      b-1
;;;         --------  x    (1-x)    
;;;          B(a,b)
;;;
;;;   cdf:  I(a,b,x) the incomplete beta function
;;;
;;; 
;;;  An instance of a Beta with shape parameters 2 and 4 is created as 

(setf beta24 (make-instance 'beta-dist :shape1 2 :shape2 4))

(display beta24)

;;;
;;;  We can now have this distribution produce the usual calculations:
;;;
;;;   Probability density calculations:

(pdf-at beta24 .3)
(pdf-at beta24 (list 0.0 .5 1))
(pdf-at beta24 (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)))

;;;   Cumulative distribution calculations:

(cdf-at beta24 .3)
(cdf-at beta24 (list 0.0 .5 1))
(cdf-at beta24 (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)))

;;;   Quantile (inverse cdf) calculations:

(quantile-at beta24 .3)
(quantile-at beta24 (list 0.0 .5 1))
(quantile-at beta24 (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)))

;;;   Pseudo-random values

(random-value beta24)
(random-value beta24 10)

;;;  And to make sure these things look right
;;;  generate lots of data

(<- data (random-value beta24 1000))

;;;  draw the histogram

(<- hist (histogram :data data :var 0 :title "Beta 2 4 sample"
                     :histogram-scale :density))

;;;  and overlay a view of the density function

(layer-view (interior-view-of hist)
            (function-view :function 
                           #'(lambda (x) (pdf-at beta24 x))
                           :domain '(0 1)
                           :nlines 100))

;;; For the beta distribution, wrapper functions for these calculations exist
;;; which reuse the same instance of a beta distribution changing parameters
;;; as requested.
;;;
;;;   Probability density calculations:

(density-beta .3 :a 2 :b 4)
(density-beta (list 0.0 .5 1)
              :a 2 :b 4)
(density-beta (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3))
              :a 2 :b 4)

;;;   Cumulative distribution calculations:

(dist-beta .3 :a 2 :b 4)
(dist-beta (list 0.0 .5 1)
            :a 2 :b 4)
(dist-beta (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3))
            :a 2 :b 4)

;;;   Quantile (inverse cdf) calculations:

(quantile-beta .3 :a 2 :b 4)
(quantile-beta (list 0.0 .5 1)
                :a 2 :b 4)
(quantile-beta (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3))
                :a 2 :b 4)

;;;   Pseudo-random values

(random-beta :a 2 :b 4)
(random-beta :a 2 :b 4 :n 10)

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
