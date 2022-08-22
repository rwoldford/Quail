;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               new-math.asd                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     M.E.Lewis 1991.
;;;     G.W.Bennett 2017. 
;;;
;;;----------------------------------------------------------------------------

(asdf:defsystem "new-math"
    :default-component-class cl-source-file.lsp
      :components ((:file "new-math/new-math-package")
               (:file "new-math/new-math")
               (:file "new-math/num-array-math")
               (:file "new-math/init-new-math")))
