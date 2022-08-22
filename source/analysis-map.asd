;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               analysis-map.asd                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1992.
;;;
;;;
;;;----------------------------------------------------------------------------
 PLACEHOLDER ONLY

(defsystem analysis-map
   :source-pathname (identity (append-directories
                                 (path-source)
                                 (path-analysis-map)))
   :binary-pathname (identity (append-directories
                                      (path-binary)
                                      (bin (path-analysis-map))))
   :components ((:module browser
                         :source-pathname (path-browser)
                         :binary-pathname (bin (path-browser))
                         :components ((:file "cl-extensions")
                                      (:file "grapher-package")
                                      (:file "specific-package")
                                      (:file "browser-package")
                                      ;;(:file (add-system-extension "wb-extensions"))
                                      (:file "grapher-var")
                                      (:file "graph-window")
                                      (:file (add-system-extension "graph-window"))
                                      (:file "grapher")
                                      ;;(:file "quail-window")
                                      ;;(:file "mop")
                                      (:file (add-system-extension "browser"))
                                      (:file (add-system-extension "browser-2"))
                                      (:file "browser")
                                      (:file (add-system-extension "browser-menu"))
                                      )
                         )
                (:module display-network
                         :source-pathname (path-display-network)
                         :binary-pathname (bin (path-display-network))
                         :components ((:file (add-system-extension "quail-functions"))
                                      (:file "utility")
                                      (:file (add-system-extension "quail-icon"))
                                      (:file "prompt-mixin")
                                      (:file "dated-object")
                                      (:file "initable-object")
                                      (:file "editable-object")
                                      (:file "indexed-object")
                                      (:file "named-object")
                                      (:file "documented-object")
                                      (:file "linked-object")
                                      (:file "tool-box-link")
                                      (:file "find-where-mixin")
                                      (:file "body-menu")
                                      (:file "title-bar-mixin")
                                      (:file "quail-object")
                                      (:file "memo")
                                      (:file "quail-browser")
                                      (:file "network-view")
                                      (:file "micro-view")
                                      (:file "analysis-network")
                                      (:file "toolbox-network")
                                      (:file "analysis-path")
                                      ;;(:file "junk-extract")
                                      (:file (add-system-extension "quail-methods"))
                                      )
                         )
                )
   )
