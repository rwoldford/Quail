;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                     Stock continuous distributions    
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
;;;  \subsection Stock continuous distributions
;;;
;;;  In this section, we consider the set of continuous distributions in 
;;;  Quail.
;;;  To see the entire hierarchy,

(class-browse 'continuous-dist)

;;; or

(help 'continuous-dist :class)

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
;;; First the Beta distribution
             (edit-file "eg:Probability;Distributions;beta.lsp")
;;;
;;; and then the continuous Uniform, a subclass of the beta-dist
             (edit-file "eg:Probability;Distributions;uniform.lsp")

;;; The Gamma distribution 
             (edit-file "eg:Probability;Distributions;gamma.lsp")
;;;
;;; and its sublasses the Exponential
             (edit-file "eg:Probability;Distributions;exponential.lsp")
;;;
;;; and the Chi squared distribution.
             (edit-file "eg:Probability;Distributions;chi-squared.lsp")
;;;
;;; The Student, or t, distribution
             (edit-file "eg:Probability;Distributions;student.lsp")
;;;
;;; and its two common subclasses: the Gaussian (degrees of freedom = infinity)
             (edit-file "eg:Probability;Distributions;gaussian.lsp")
;;;
;;; and the Cauchy (degrees of freedom = 1)
             (edit-file "eg:Probability;Distributions;cauchy.lsp")
;;;
;;; Finally some unrelated distributions.
;;;
;;; The Pareto
             (edit-file "eg:Probability;Distributions;pareto.lsp")
;;;
;;; The Weibull
             (edit-file "eg:Probability;Distributions;weibull.lsp")
;;;
;;; Fisher's F distribution
             (edit-file "eg:Probability;Distributions;F-dist.lsp")
;;;
;;; and our very own K distribution
             (edit-file "eg:Probability;Distributions;K-dist.lsp")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  You can get to the stock discrete distributions by executing

   (edit-file "eg:Probability;Distributions;stock-disc.lsp")

;;;
;;;  or back to the stock distributions discussion by executing

   (edit-file "eg:Probability;Distributions;stock.lsp")

;;; 
;;;  or back to the overview by executing

