;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               host-draw-package.lisp                             
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1992.
;;;
;;;
;;;----------------------------------------------------------------------------

#+:cl-2
(defpackage "HOST-DRAW"
  ;#+:sbcl-linux (:use :clim-lisp :clim :clim-extensions) ; "COMMON-LISP" 19 November 2019
  ;#+:aclpc-linux (:use :common-lisp)
  (:use "COMMON-LISP")
  (:nicknames "H-DRAW")
  (:IMPORT-FROM "QUAIL-KERNEL"
                *QUAIL-RESTORE-LISP-FUNCTIONS*
                *QUAIL-STANDARD-INPUT* 
                *QUAIL-STANDARD-OUTPUT* 
                *QUAIL-QUERY-IO*
                *QUAIL-DEBUG-IO*
                *QUAIL-ERROR-OUTPUT*
                *QUAIL-TRACE-OUTPUT*
                *QUAIL-TERMINAL-IO*
                QUAIL-PRINT
                QUAIL-ERROR
                QUAIL-CERROR
                QUAIL-QUERY
                QUAIL-Y-OR-N-P 
                QUAIL-YES-OR-NO-P)
  #+:ccl
  (:shadow
   "MAKE-POINT")
 #+:aclpc
(:shadow
  "MOVE-TO" "LINE-TO" "DRAW-LINE" "DRAW-ELLIPSE" "DRAW-POLYGON")
  #+:ccl
  (:export
   "MAKE-POINT"))

#-:cl-2
(in-package "HOST-DRAW" :use '(pcl lisp) :nicknames '(H-DRAW))

#-:CL-2
(IMPORT '(QUAIL-KERNEL::*QUAIL-RESTORE-LISP-FUNCTIONS*
          QUAIL-KERNEL:*QUAIL-STANDARD-INPUT* 
          QUAIL-KERNEL:*QUAIL-STANDARD-OUTPUT* 
          QUAIL-KERNEL:*QUAIL-QUERY-IO*
          QUAIL-KERNEL:*QUAIL-DEBUG-IO*
          QUAIL-KERNEL:*QUAIL-ERROR-OUTPUT*
          QUAIL-KERNEL:*QUAIL-TRACE-OUTPUT*
          QUAIL-KERNEL:*QUAIL-TERMINAL-IO*
          QUAIL-KERNEL:QUAIL-ERROR
          QUAIL-KERNEL:QUAIL-CERROR
          QUAIL-KERNEL:QUAIL-QUERY
          QUAIL-KERNEL:QUAIL-Y-OR-N-P 
          QUAIL-KERNEL:QUAIL-YES-OR-NO-P)
        "HOST-DRAW")
