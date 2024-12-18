;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               top-level.asd                              
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

(asdf:defsystem "top-level"
    :default-component-class cl-source-file.lsp
  :components (#-:sbcl-linux(:file "top-level/quail-top-level-read")
               ;#+:sbcl-linux(:file  "top-level/top-level-sblx")
               #+:aclpc-linux(:file "top-level/top-level-pc")
               (:file "top-level/infix") 
               #-:sbcl-linux(:file "top-level/top-level")
               ;; the following allows execution of forms from within an edit
               ;; window *when* the Quail top-level loop is running.
               #+:sbcl-linux(:file "top-level/editor-sblx")
               #+:ccl (:file "top-level/editor-mcl")
               #+:aclpc-linux(:file "top-level/editor-pc")
               #-:sbcl-linux(:file "top-level/quail-menu")
               #-:sbcl-linux(:file "top-level/quail-plot-menu")
               #+:sbcl-linux(:file "top-level/quail-menubar-sblx")
               #+:sbcl-linux(:file "top-level/quail-menu-sblx") ;07NOV2024
               #+:sbcl-linux(:file "top-level/plots-menu-sblx") ;07NOV2024
               #+:sbcl-linux(:file "top-level/canvas-menu-sblx") ;07NOV2024
               #+:sbcl-linux(:file "top-level/quail-plots-menus-sblx") ;07NOV2024
               #+:sbcl-linux(:file "top-level/quail-plots-canvas-menus-sblx") ;07NOV2024
               #+:sbcl-linux(:file "top-level/change-menu-bar-sblx") ;07NOV2024
               ;#+:sbcl-linux(:file "top-level/quail-menubar-sblx")
               #+:aclpc-linux(:file "top-level/quail-menubar-pc")  
               ))

