;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                        Empirical distributions                           
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
;;;  \subsubsection Empirical distributions
;;;
;;;  In this section, we consider empirical distributions as represented in 
;;;  Quail.
;;;
;;;  The mathematical form of the distribution is as follows:
;;;
;;;  Let y_1, y_2, y_3, ..., y_n be a random sample of size n from some
;;;  distribution F.  Then the empirical distribution F-hat is defined
;;;  to be such that probability mass 1/n is assigned to each observation y_i.
;;;
;;;  The result is
;;;
;;;   pdf:   f-hat(y)  = #{y_i = y} / n
;;;
;;;   cdf:   F-hat(y)  = #{y_i <= y} / n
;;; 
;;;
;;;  
;;;  In Quail, any collection of numbers can be thought of as an empirical
;;;  distribution.
;;;
;;;  For example,  suppose we consider a sample of size 10 from a Gaussian
;;;  distribution.

(<- y (random-gaussian :n 10))
y

;;;  The data collection y itself can now be treated as if it were an
;;;  empirical distribution.
;;;
;;;  The density, first at a value not (likely to be) in the sample

(pdf-at y 0)

;;;  Then at a few points in the sample

(pdf-at y (eref y 0))
(pdf-at y (min y))
(pdf-at y (max y))

;;;  Similarly, the distribution function at any of these points

(cdf-at y 0)
(cdf-at y (eref y 0))
(cdf-at y (min y))
(cdf-at y (max y))

;;;  Any method applicable to a probability-measure object will also be
;;;  applicable to a dataset.
;;;
;;;  Quantiles, 

(quantile-at y 0.5)

;;;  all that correspond to the order statistic

(quantile-at y (seq 0.0 1.0 0.1))

;;;  compared to 

(sort y #'<)

;;;  and we interpolate quantiles in between (where ties exist)

(quantile-at y (seq 0.9 1.0 0.01))


;;;  Perhaps most interesting are samples from the empirical distribution.
;;;

(random-value y 5)
(random-value y 30)

;;;  It has become commonplace to call simulations based on the empirical
;;;  distribution, ``bootstrapping''.
;;;
;;;  Here is a simple example.  Suppose we write a function that
;;;  generates B samples of size k from some distribution G and returns
;;;  the means of those samples in an array.  Here's the function

(defun sim-means (G B k)
  "Generate B samples of size k from some distribution G and return ~
   the means of those samples in an array."
  (array
     (loop for i from 1 to B collect
           (mean (random-value G k)))))

;;;
;;;  If we were interested in samples of size 10 from Gaussian 0, 1
;;;  distributions say, then we might collect 50 such means as follows:
;;;  
;;;  first get the distribution

(<- gaussian-dist (make-instance 'gaussian-dist :location 0 :scale 1))

;;;  then carry out the simulation

(<- g-means (sim-means gaussian-dist 50 10))

;;;  and perhaps examine the resulting empirical distribution of means.

(histogram :data g-means :var 0 :title "Gaussian means")

;;;  If we want to do some ``bootstrapping'' we need only use the original
;;;  data y as the distribution.

(<- emp-means (sim-means y 50 10))

;;;  and again examine the resulting empirical distribution of means.

(histogram :data emp-means :var 0 :title "Bootstrap means")

;;;  The two sets of results might even be compared in the same plot.
;;;  Let's first make them a dataset

(<- means-data
    (dataset (cglue g-means emp-means)
             :variates (list "Gaussian means" "Bootstrap means")
             :name "Simulated means"))

;;; and then layout the histograms
(1d-layout-plot
 :data means-data
 :subview-type 'histogram
 :vars (list "Gaussian means" "Bootstrap means")
 :format :col)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  You can get to the stock discrete distributions by executing

   (edit-file "eg:Probability;Distributions;stock-disc.lsp")

;;;
;;;  or to the stock continuous distributions discussion by executing

   (edit-file "eg:Probability;Distributions;stock-cts.lsp")

;;;
;;;  or to the stock distributions discussion by executing

   (edit-file "eg:Probability;Distributions;stock.lsp")

;;; 
;;;  or to the overview by executing

   (edit-file "eg:Probability;Distributions;overview.lsp")
