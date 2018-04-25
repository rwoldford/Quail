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

;(in-package :make)

;(eval-when (compile load eval)
;  (load-pathnames "new-math"))

(asdf:defsystem "new-math"
    :default-component-class cl-source-file.lsp
      :components ((:file "new-math/new-math-package")
               (:file "new-math/new-math")
               (:file "new-math/num-array-math")
               (:file "new-math/init-new-math")))

#|
(defun compile-new-math (&rest op-on-sys-keyword-pairs
                               &key (verbose T)
                               &allow-other-keys)
  "Compiles the new-math system using make's~
   operate-on-system's keywords."
  (apply #'operate-on-system 'new-math 'compile
         :verbose verbose
         :allow-other-keys T op-on-sys-keyword-pairs))

(defun load-new-math (&rest op-on-sys-keyword-pairs
                            &key (verbose T)
                            &allow-other-keys)
  "Loads the new-math system using make's~
   operate-on-system's keywords."
  (apply #'operate-on-system 'new-math 'load
         :verbose verbose
         :allow-other-keys T op-on-sys-keyword-pairs))
|#          