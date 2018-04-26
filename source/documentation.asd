;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               documentation.asd                             
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1991.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PLACEHOLDER ONLY

(defsystem documentation
   :source-pathname (identity (append-directories
                                 (path-source)
                                 (path-documentation)))
   :binary-pathname (identity (append-directories
                                      (path-binary)
                                      (bin (path-documentation))))
   :components ((:file "inform-user")
                (:file "edit-file")
                (:file "doc-vars")
                (:file "doc-utility")
                (:file "documentation")
                (:file "doc-type")
                (:file "utility-args")
                (:file "doc")
                (:file "write-doc")
                (:file "format-ext")
                (:file "format-doc")
                (:file "topic")
                (:file "install-topics")
                (:file "generate-topic")
                (:file "format-tree")
                (:file "documentation-string")
                (:file "make-doc")
                (:file "document-topics")
                (:file "auto-topics")
                (:file "help")
                (:file "tex-basic")
                (:file "write-tex")
                (:file "tex-ext")
                (:file "doc-tex-index")
                (:file "write-tex-doc")
                (:file "document-symbols")
                (:file "tex-doc-symbols")
                (:file "track-new-symbols")
                (:file "header-box")
                (:file "doc-display-lists")
                (:file "help-window")
                (:file "help-sub-views")
                (:file "help-view")
                (:file "help-display")
                (:file "doc-index")
                ))

(defun compile-documentation (&rest op-on-sys-keyword-pairs
                              &key (verbose T)
                              &allow-other-keys)
  "Compiles the documentation system using make's~
   operate-on-system's keywords."
  (apply #'operate-on-system 'documentation 'compile
         :verbose verbose
         :allow-other-keys T op-on-sys-keyword-pairs))

(defun load-documentation (&rest op-on-sys-keyword-pairs
                           &key (verbose T)
                           &allow-other-keys)
  "Loads the documentation system using make's~
   operate-on-system's keywords."
  (apply #'operate-on-system 'documentation 'load
         :verbose verbose
         :allow-other-keys T op-on-sys-keyword-pairs))