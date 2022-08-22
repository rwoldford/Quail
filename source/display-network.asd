;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               display-network.system                               
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

(defsystem display-network
  :source-pathname (identity (append-directories
                                (path-source)
                                (path-display-network)))
  :binary-pathname (identity (append-directories
                                (path-binary)
                                (bin (path-display-network))))
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
               (:file "zoom-mixin")
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
               ;; to be added (:file "setup-menus")
               )
  )


(defun compile-display-network (&rest op-on-sys-keyword-pairs
                              &key (verbose T)
                              &allow-other-keys)
  "Compiles the display-network system using make's~
   operate-on-system's keywords."
  (apply #'operate-on-system 'display-network 'compile
         :verbose verbose
         :allow-other-keys T op-on-sys-keyword-pairs))

(defun load-display-network (&rest op-on-sys-keyword-pairs
                           &key (verbose T)
                           &allow-other-keys)
  "Loads the display-network system using make's~
   operate-on-system's keywords."
  (apply #'operate-on-system 'display-network 'load
         :verbose verbose
         :allow-other-keys T op-on-sys-keyword-pairs))