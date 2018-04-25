;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               mathematics.asd     
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1992.
;;;     G.W.Bennett 2018
;;;
;;;--------------------------------------------------------------------------------



;(in-package :make)

;(eval-when (compile load eval)
;  (load-pathnames "mathematics")
;  )
        

(asdf:defsystem "mathematics"
    :default-component-class cl-source-file.lsp
   :components
   ((:module "mathematics/special-functions"
             :components
             ((:file "log-gamma")
              (:file "continued-fraction")
              (:file "complete-beta")
              (:file "incomplete-gamma")
              (:file "incomplete-beta")
              (:file "error-function")
              ))

    (:module "mathematics/combinatorics"
             :components
             ((:file "factorial")
              (:file "choose")
              (:file "factor")
              ))

    (:module "mathematics/functions"
             :components ((:file "fn")
                    
      ;(:file "surface")
                          )
             )
    #|
    (:module borel-sets
             :source-pathname (path-borel-sets)
             :binary-pathname (bin (path-borel-sets))
             :components ((:file "utility")
	                  (:file "borel-special")
                          (:file "set-collections")
                          (:file "borel-sets")
	                  (:file "borel-sets2")
	                  (:file "borel-functions")
                          (:file "borel-special2")
                          (:file "borel-sets-methods")
	                  (:file "memberp-methods")
	                  (:file "insidep-methods")
			  (:file "copy-methods")
			  (:file "disjoint-sets-p-methods")
	 		  (:file "simplify-methods")
			  (:file "set-union-methods")
			  (:file "set-intersection-methods")
			  (:file "set-complement-methods")
                          )
             )
    (:module measures
             :source-pathname (path-measures)
             :binary-pathname (bin (path-measures))
             :components ((:file "measure")
                          (:file "measure-generics")
                          (:file "lebesgue-measure")
                          (:file "counting-measure")
                          (:file "finite-measure")
                          (:file "probability-measure")
                          )
             )
    |#

    (:module "mathematics/calculus"
             :components ((:file "numerical-deriv")
                          (:file "simplify")
                          (:file "deriv")
                          (:file "simpsons")
                          )
             )

    (:module "mathematics/root-finders"
             :components ((:file "newton")
                          (:file "illinois")
                          )
             )
     #|
    (:module graphics
             :source-pathname (path-math-graphics)
             :binary-pathname (bin (path-math-graphics))
             :components ((:file "surface-view")
                          (:file "surface-plot")
                          (:file "rotate-surface")
                          )
             )
    |#
    ))