;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                         Stock Distributions                            
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
;;;  \section Stock Distributions
;;;
;;;    In Quail, there are many ``stock'' distributions for which
;;; one routinely wants the standard calculations of cdf-at,
;;; pdf-at, quantile-at, and random-value.
;;; Moreover, considerable research effort has been spent by many people
;;; over the years developing algorithms to make these calculations efficient
;;; and accurate.
;;; By having separate classes for these distributions the methods (cdf-at, etc.)
;;; can be specialized to take advantage of these algorithms.
;;;
;;; The classes can be examined in a brower with

(class-browse 'prob-measure)

;;; or through the help system with 

(help 'distributions :topic)

;;;
;;; Because many distributional calculations are routinely required,
;;; a number of ``wrapper'' functions have been created to allow access to the
;;; results without creating any new instances of distributions whenever the
;;; parameter values change.
;;; A standard naming strategy is adopted for these functions in the hope of
;;; making them more mnemonic.
;;;
;;; For example, the Gaussian (or Normal) distribution is a commonly used one.
;;; Calculations for Gaussian distributions can be had without resorting to
;;; first creating an instance of Gaussian-dist; the functions are

(dist-gaussian 1.645 :location 0.0 :scale 1.0)

;;; for cdf calculations,

(density-gaussian 1.645 :location 0.0 :scale 1.0)

;;; for pdf calculations,

(quantile-gaussian .95 :location 0.0 :scale 1.0)

;;; for quantiles, and

(random-gaussian :n 10 :location 0.0 :scale 1.0)

;;; for pseudo-random Gaussian observations.
;;; The pattern for other distributions is the same -- prefixes
;;; dist-, density-, quantile-, and random-.
;;; Keyword parameters will vary depending on the distribution.
;;;
;;; These specialized functions make use of the generic-functions cdf-at, etc.
;;; called on instances of the appropriate distribution class.
;;; These instances are cached on the global variable *distributions*.

*distributions*

;;;
;;;
;;; The stock distributions are grouped according to whether they represent
;;; continuous or discrete random variates.
;;; Within these groupings, the class hierarchy of the distributions is defined
;;; so that whenever a distribution is mathematically a special case of a second
;;; distribution, its class is a descendant of the class representing the second
;;; distribution.
;;;
;;; The stock distributions include the Gaussian (or Normal) distribution,
;;; the Student t, the beta distribution, the gamma distribution, the binomial,
;;; the hypergeometric and many others.
;;;
;;;  \subsection 2.1 Stock continuous distributions

              (edit-file "eg:Probability;Distributions;stock-cts.lsp")
;;;
;;;  \subsection 2.2 Stock discrete distributions

              (edit-file "eg:Probability;Distributions;stock-disc.lsp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;  You can get back to the overview by executing

