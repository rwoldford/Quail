;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               probability.asd                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1993 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1993.
;;;
;;;
;;;--------------------------------------------------------------------------------

;(in-package :make)

;(eval-when (compile load eval)
;  (load-pathnames "probability")
;  )
        
(asdf:defsystem "probability"
    :default-component-class cl-source-file.lsp
   :components
   ((:module "probability/generators"
             :components
             ((:file "random")
              (:file "system")
              (:file "congruential")
              (:file "default")
              ))
    
    (:module "probability/distributions"
             :components
             ((:file "prob-measure")
              (:file "findlimits")
              (:file "discrete-dist")
              (:file "bisection")
              (:file "continuous-dist")
              (:file "location-scale")
              (:file "beta")
              (:file "uniform")
              (:file "prob-methods")
              (:file "gamma")
              (:file "exponential")
              (:file "chi-squared")
              (:file "student")
              (:file "gaussian")
              (:file "cauchy")
              (:file "binomial")
              (:file "bernoulli")
              (:file "negative-binomial")
              (:file "geometric")
              (:file "hypergeometric") ;;; <---- fix simultaneous setting
              (:file "discrete-uniform")
              (:file "poisson")
              (:file "finite-mixture")
              (:file "data-prob")
              (:file "pareto")
              (:file "weibull")
              (:file "F-dist")
              (:file "K-dist")
              (:file "init-dist")
              ))
    (:module "probability/random"
             :components
             ((:file "permute")))
    
    
    )
    )