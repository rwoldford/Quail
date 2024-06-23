;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               quail.asd                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1989, 1990, 1991.
;;;     R.W. Oldford 1989 +
;;;     Bob White
;;;     Greg Bennett 2017
;;;
;;;
;;;----------------------------------------------------------------------------


(asdf:defsystem "quail" ;; since that is where quail-package.lsp is! 
                                    ;; There is now a quail directory within source.
    :default-component-class cl-source-file.lsp
    :components ((:file "quail/quail-package")
    	         ;(:file "force-to-pathname") ;; this is in test-documentation-path.lsp
    ))