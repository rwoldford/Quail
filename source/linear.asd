;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               linear.asd                            
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1994 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Michael E. Lewis 1991.
;;;     R.W. Oldford 1994.
;;;
;;;
;;;--------------------------------------------------------------------------------

(asdf:defsystem "linear"
    :default-component-class cl-source-file.lsp
  :components  ((:file "linear/misc-defs")
               (:file "linear/pivot")
               (:file "linear/linpack-macros")
               (:file "linear/dasum")
               (:file "linear/daxpy")
               (:file "linear/dchdc")
               (:file "linear/dcopy")
               (:file "linear/ddot")
               (:file "linear/dgeco")
               (:file "linear/dgedi")
               (:file "linear/dgefa")
               (:file "linear/dgels")
               (:file "linear/dgesl")
               (:file "linear/dnrm2")
               (:file "linear/dpoco")
               (:file "linear/dpodi")
               (:file "linear/dpofa")
               (:file "linear/dposl")
               (:file "linear/dqrdc")
               (:file "linear/dqrls")
               (:file "linear/dqrsl")
               (:file "linear/drot")
               (:file "linear/drotg")
               (:file "linear/dscal")
               (:file "linear/dsico")
               (:file "linear/dsidi")
               (:file "linear/dsifa")
               (:file "linear/dsisl")
               (:file "linear/dsvdc")
               (:file "linear/dswap")
               (:file "linear/dtrco")
               (:file "linear/dtrdi")
               (:file "linear/dtrls")
               (:file "linear/dtrsl")
               (:file "linear/dpbco")
               (:file "linear/dpbfa")
               (:file "linear/dpbsl")
               (:file "linear/dpbdi")
               (:file "linear/dgbco")
               (:file "linear/dgbfa")
               (:file "linear/dgbsl")
               (:file "linear/dgbdi")
               (:file "linear/idamax")
               (:file "linear/jacobi")
               (:file "linear/linpack")
               (:file "linear/table-mixin")
               (:file "linear/matrix-decomposition")
               (:file "linear/diagonal")
               (:file "linear/triangular")
               (:file "linear/qr-decomposition")
               (:file "linear/lu-decomposition")
               (:file "linear/cholesky-decomposition")
               (:file "linear/sv-decomposition")
               (:file "linear/eigen-decomposition")
               (:file "linear/inverse")
               (:file "linear/identity-matrix")
               (:file "linear/solve")
               (:file "linear/qr-solution")
               (:file "linear/lin-binary-methods")
               (:file "linear/rank")
               (:file "linear/condition-number")
               (:file "linear/determinant")
               (:file "linear/trace")
               (:file "linear/matrix-sqrt")
               (:file "linear/apply-weight")
               ))
