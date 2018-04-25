;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                     Stock discrete distributions    
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
;;;  \subsection Stock discrete distributions
;;;
;;;  In this section, we consider the set of discrete distributions in 
;;;  Quail.
;;;  To see the entire hierarchy,

(class-browse 'discrete-dist)

;;; or

(help 'discrete-dist :class)

;;; or

(help 'distributions :topic)

;;; As you will see, the class hierarchy of the distributions is defined
;;; so that whenever a distribution is mathematically a special case of a second
;;; distribution, its class is a descendant of the class representing the second
;;; distribution.
;;;
;;; We will now traverse this collection in order from the super classes
;;; down to the sub-classes.
;;;
;;; First the Binomial distribution

             (edit-file "eg:Probability;Distributions;binomial.lsp")

;;; The bernoulli is a special case of the binomial where n = 1.

             (edit-file "eg:Probability;Distributions;bernoulli.lsp")
;;;
;;;  The negative-binomial distribution
;;;
             (edit-file "eg:Probability;Distributions;negative-binomial.lsp")
;;;
;;;  The geometric distribution is a special case of the negative-binomial
;;;  where n = 1.
;;;
             (edit-file "eg:Probability;Distributions;geometric.lsp")

;;;  The hypergeometric, for sampling with replacement from finite populations.

             (edit-file "eg:Probability;Distributions;hypergeometric.lsp")

;;;  The discrete-uniform, equiprobability at each element of a finite population.

             (edit-file "eg:Probability;Distributions;discrete-uniform.lsp")

;;;  The Poisson, modelling counts from 0 to infinity.

             (edit-file "eg:Probability;Distributions;poisson")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  You can get to the stock continuous distributions by executing

   (edit-file "eg:Probability;Distributions;stock-cts.lsp")

;;;
;;;  or back to the stock distributions discussion by executing

   (edit-file "eg:Probability;Distributions;stock.lsp")

;;; 
;;;  or back to the overview by executing

   (edit-file "eg:Probability;Distributions;overview.lsp")
