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

(asdf:defsystem "documentation"
    :default-component-class cl-source-file.lsp
   :components ((:file "documentation/documentation-path") ;07JAN2020
                (:file "documentation/inform-user")
                (:file "documentation/edit-file")
                (:file "documentation/doc-vars")
                (:file "documentation/doc-utility")
                (:file "documentation/documentation")
                (:file "documentation/doc-type")
                (:file "documentation/utility-args")
                (:file "documentation/doc")
                (:file "documentation/write-doc")
                (:file "documentation/format-ext")
                (:file "documentation/format-doc")
                (:file "documentation/topic")
                (:file "documentation/install-topics")
                (:file "documentation/generate-topic")
                (:file "documentation/format-tree")
                (:file "documentation/documentation-string")
                (:file "documentation/make-doc")
                (:file "documentation/document-topics")
                (:file "documentation/auto-topics")
                (:file "documentation/help")
                (:file "documentation/tex-basic")
                (:file "documentation/write-tex")
                (:file "documentation/tex-ext")
                (:file "documentation/doc-tex-index")
                (:file "documentation/write-tex-doc")
                (:file "documentation/document-symbols")
                (:file "documentation/tex-doc-symbols")
                (:file "documentation/track-new-symbols")
                (:file "documentation/header-box")
                (:file "documentation/help-window")
                ;(:file "documentation/header-box")
                (:file "documentation/doc-display-lists")
                ;(:file "documentation/help-window")
                (:file "documentation/help-sub-views")
                (:file "documentation/help-view")
                (:file "documentation/help-display")
                (:file "documentation/doc-index")
           ))
