;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                   Special mathematical functions
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1994.

(in-package :quail-user)

;;;
;;;  Several so-called ``special functions'' have been implemented in Quail.
;;;  Most are hand-coded translatations from the Fortran code in Numerical Recipes
;;;  by Press, Flannery, Teukolsky, and Vetterling (1986).
;;;  
;;;  Caveat:
;;;      All have been implemented to operate on real numerical arguments.
;;;      They have not been implemented to handle extended or complex arithmetic.
;;;      Should it be desirable to operate on arrays the Quail function
;;;      map-element should be called with the special function as an argument.
;;;
;;; In these files we consider the special functions:
;;;
;;;      gamma                         ... the complete gamma function
;;;      log-gamma                     ... the natural log of the gamma function
;;;      incomplete-gamma              ... the incomplete-gamma function
;;;      incomplete-gamma-complement   ... the complement of the incomplete-gamma

(edit-file "eg:Mathematics;Special-Functions;gamma.lsp")

;;;      beta                          ... the complete beta function B(a,b)
;;;      incomplete-beta               ... the incomplete beta function B(a,b)

(edit-file "eg:Mathematics;Special-Functions;beta.lsp")

;;;      error-function                ... the error function erf(x)
;;;      error-function-complement     ... the complement erfc(x) of the error
;;;                                        function 1 - erf(x)

(edit-file "eg:Mathematics;Special-Functions;error-fun.lsp")

;;;      continued-fraction-eval       ... evaluate an arbitrary continued
;;;                                        fraction approximation

(edit-file "eg:Mathematics;Special-Functions;continued-fraction.lsp")

