;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               browser-package.lisp                             
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1991.
;;;
;;;
;;;----------------------------------------------------------------------------

#+:cl-2
(defpackage "BROWSER"
  (:use 
   "WINDOW-BASICS"
   "COMMON-LISP"
   #+:ccl "CCL"
   )
  (:import-from "QUAIL" "QUAIL-PRINT" "QUAIL-ERROR")
  (:import-from "QUAIL-KERNEL"
                "GENERIC-FUNCTION-P"
                "DIRECT-SUBCLASSES"
                "CLASS-P"
                "PRECEDENCE-LIST")
  )

#-:cl-2
(in-package "BROWSER"
            :use '(grapher
                   window-basics
                   pcl
                   lisp
                   )
            )
