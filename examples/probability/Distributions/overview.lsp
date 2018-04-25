;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                  Probability Distributions in Quail:  
;;;                            Overview                          
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

;;; The material to be covered about probability distributions
;;; in Quail is spread out over several example files.
;;; A not unreasonable tutorial would be to browse the files in order.
;;; An alternative approach would be to follow the topic documentation
;;; through Quail's help system as in

(help 'distributions :topic)

;;; or more generally

(help 'probability :topic)

;;; 
;;; Here we outline the contents of the example files and offer a form to
;;; evaluate that will bring each example file up as desired.
;;;
;;;
;;;
;;; Contents:
;;;
;;;    1. Introduction.
;;;        ... top-most class hierarchy of distributions
;;;        ... the generic functions 
;;;               - pdf-at, cdf-at, quantile-at, random-value
          (edit-file "eg:Probability;Distributions;intro.lsp")
;;;
;;;    2. Stock distributions
;;;        ... classes and generic functions
;;;        ... instances stored on *distributions*
          (edit-file "eg:Probability;Distributions;stock.lsp")
;;;
;;;       2.1 Stock continuous distributions
              (edit-file "eg:Probability;Distributions;stock-cts.lsp")
;;;
;;;       2.2 Stock discrete distributions
              (edit-file "eg:Probability;Distributions;stock-disc.lsp")
;;;
;;;    3. Data as empirical distributions
          (edit-file "eg:Probability;Distributions;data-prob.lsp")

;;;
;;;    4. Finite Mixtures of Distributions
          (edit-file "eg:Probability;Distributions;finite-mixture.lsp")

;;;
;;;    5. Extending the system.
