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

PLACEHOLDER ONLY

(defsystem top-level
  :source-pathname (identity (append-directories
                                (path-source)
                                (path-top-level)))
  :binary-pathname (identity (append-directories
                                (path-binary)
                                (bin (path-top-level))))
  :components ((:file "quail-top-level-read")
               (:file (add-system-extension "top-level"))
               (:file "infix") 
               (:file "top-level")
               ;; the following allows execution of forms from within an edit
               ;; window *when* the Quail top-level loop is running.
               #+:ccl (:file (add-system-extension "editor"))
               (:file "quail-menu")
               (:file "quail-plot-menu")
               (:file (wb-add-system-extension "quail-menubar"))
               ))

