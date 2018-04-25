;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;              Introduction to Probability Distributions in Quail                            
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
;;;  \section Introduction
;;;
;;;    In Quail, a class is used to represent a family of probability 
;;;    distributions such as the gaussian or binomial family.
;;;    So the class ``gaussian-dist'' represents the general class of gaussian
;;;    or ``normal'' distributions having unspecified location and scale.
;;;
;;;    The class hierarchy has ``prob-measure'' as its top-most class and
;;;    may be seen by typing
       (class-browse 'prob-measure)
;;;    or
       (help 'prob-measure :class)
;;;
;;;    In contrast, instances of the class represent particular family members
;;;    having all unknown parameters specified.
;;;    So an instance of gaussian-dist having location 0 and scale 1 is a
;;;    representation of the standard Gaussian or Normal(0,1) distribution.
;;;    Here is just such an instance

(setf N-0-1 (make-instance 'gaussian-dist :location 0 :scale 1))

;;;
;;;    And you can see a simple display of it

(display N-0-1)

;;;    A set of instances of stock distribution classes is cached on the global
;;;    variable

*distributions*

;;;    Every instance of a sub-class of prob-measure must respond to the
;;;    following generic-functions: pdf-at, cdf-at, quantile-at, and
;;;    random-value.
;;;
;;;    (pdf-at distribution x)
;;;        ... This returns the value of the probability density function
;;;            (or pdf) for the distribution evaluated at x.
;;;            For discrete distributions this is just the probability of x.
;;;            X can be a number,  a list of numbers,
;;;            an array of numbers, or any other dimensioned ref-object of
;;;            numbers.
;;;    For example,

(pdf-at N-0-1 0.0)
(pdf-at N-0-1 '(-1.0 0.0 1.0))
(pdf-at N-0-1 (array '(-1.0 0.0 1.0 2.0) :dimensions '(2 2)))

;;;    The pdf-at generic-function has methods specialized for particular classes
;;;    and these may be explored through the help system with

(help 'pdf-at)

;;;
;;;
;;;    (cdf-at distribution x)
;;;        ... This returns the value of the cumulative distribution function
;;;            (or cdf) for the distribution evaluated at x.
;;;            X can be a number, an extended number, a list of these,
;;;            an array of these, or any other dimensioned ref-object of
;;;            these.
;;;    For example,

(cdf-at N-0-1 0.0)
(cdf-at N-0-1 infinity)
(cdf-at N-0-1 '(-1.0 0.0 1.0))
(cdf-at N-0-1 (array '(-1.0 0.0 1.0 2.0) :dimensions '(2 2)))

;;;    The cdf-at generic-function has methods specialized for particular classes
;;;    and these may be explored through the help system with

(help 'cdf-at)

;;;
;;;    (quantile-at distribution p)
;;;        ... This returns the quantile of the distribution that corresponds
;;;            to the cumulative probability p (0 =< p =< 1).
;;;            That is, if F(x) is the cumulative distribution evaluated at
;;;            the value x, then the p'th quantile of F is that value x
;;;            such that F(x) = p.  If F is invertible, its p'th quantile is just
;;;                     -1
;;;                    F  (p).
;;;            P can be a number,  a list of numbers,
;;;            an array of numbers, or any other dimensioned ref-object of
;;;            numbers.
;;;    For example,

(quantile-at N-0-1 0.0)
(quantile-at N-0-1 0.975)
(quantile-at N-0-1 (seq 0.0 1.0 .2))

;;;    The quantile-at generic-function has methods specialized for particular classes
;;;    and these may be explored through the help system with

(help 'quantile-at)

;;;    
;;;    (random-value distribution n)
;;;        ... This returns n pseudo-random values from the distribution;
;;;            n is optional, if missing it defaults to 1.
;;;            numbers.
;;;    For example,

(random-value N-0-1)
(random-value N-0-1 20)

;;;    The random-value generic-function has methods specialized for particular
;;;    classes and these may be explored through the help system with

(help 'random-value)


;;;
;;; Finally, because there are many ``stock'' distributions for which one
;;; one routinely wants these kinds of calculations, a number of ``wrapper''
;;; functions have been created which provide a more mnemonic interface
;;; to the stock distributional results.
;;; 
;;; For example, the Gaussian (or Normal) distribution is a commonly used one.
;;; Calculations for Gaussian distributions can be had without resorting to
;;; first creating an instance of Gaussian-dist; the functions are
;;; for cdf calculations,

(dist-gaussian 1.645 :location 0.0 :scale 1.0)

;;; for pdf calculations,

(density-gaussian 1.645 :location 0.0 :scale 1.0)

;;; for quantiles,

(quantile-gaussian .95 :location 0.0 :scale 1.0)

;;; and for pseudo-random Gaussian observations,

(random-gaussian :n 10 :location 0.0 :scale 1.0)

;;; The pattern for other distributions is the same -- prefixes
;;; dist-, density-, quantile-, and random-.
;;; Keyword parameters will vary depending on the distribution.
;;;
;;; These specialized functions make use of the generic-functions cdf-at, etc.
;;; called on instances of the appropriate distribution class.
;;; These instances are cached on the global variable *distributions*.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;  You can get back to the overview by executing

