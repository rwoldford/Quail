;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               initialization.asd                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1991.
;;; 
;;;
;;;----------------------------------------------------------------------------

(asdf:defsystem "initialization"
    :default-component-class cl-source-file.lsp
  :components (;(:file "initialization/update-special-vars") ;; contains vw:
               (:file "initialization/synonyms")
               ;; no longer supported
               ;; (:file (add-system-extension "ffenv-std"))
               (:file "initialization/load-quail-init")
               ;; a temporary way to deal with this ...
               ;(:file "initialization/release-path")
               ;(:file "initialization/release") << contains vw:
               (:file "initialization/restore-sblx")
               ;(:file "initialization/restored-lisp-functions") << some undefined functions invoked
               ;(:file "export-syms-from-quail") << contains wb:
               ;(:file "initialization/views-init") << contains vw:
               ;(:file "redefine-quail-io") << contains wb:
               ))
          
