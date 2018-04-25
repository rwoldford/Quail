;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               views-package.lisp                             
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


(in-package "MAKE")

#+:cl-2
(defpackage "VIEWS"
  (:use ;;;;;"WINDOW-BASICS"
   "COMMON-LISP")
  (:nicknames "VW" "VI")
  
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
                QUAIL-YES-OR-NO-P
                ADD-MIXIN-TO-QUAIL-OBJECT)
  (:IMPORT-FROM "WINDOW-BASICS"
                HANDLE-KEY-EVENT
                *black-color*
                *white-color*
                *pink-color*
                *red-color*
                *orange-color*
                *yellow-color*
                *green-color*
                *dark-green-color*
                *light-blue-color*
                *blue-color*
                *purple-color*
                *brown-color*
                *tan-color*
                *light-gray-color*
                *gray-color*
                *dark-gray-color*
                *gray-color* 
                *light-gray-color* 
                *dark-gray-color* 
                *bright-green-color*
                *magenta-color*
                *black-shade*
                *white-shade*
                *light-gray-shade*
                *dark-gray-shade*
                *gray-shade*)
  (:export *black-color*
           *white-color*
           *pink-color*
           *red-color*
           *orange-color*
           *yellow-color*
           *green-color*
           *dark-green-color*
           *light-blue-color*
           *blue-color*
           *purple-color*
           *brown-color*
           *tan-color*
           *light-gray-color*
           *gray-color*
           *dark-gray-color*
           *gray-color* 
           *light-gray-color* 
           *dark-gray-color* 
           *bright-green-color*
           *magenta-color*
           *black-shade*
           *white-shade*
           *light-gray-shade*
           *dark-gray-shade*
           *gray-shade*)
  )

#-:cl-2
(in-package "VIEWS" :use '(;;;;window-basics
                           pcl lisp) :nicknames '(vw vi))

#-:cl-2
(export '())

#-:CL-2
(IMPORT '(WINDOW-BASICS:HANDLE-KEY-EVENT
          wb:*black-color*
          wb:*white-color*
          wb:*pink-color*
          wb:*red-color*
          wb:*orange-color*
          wb:*yellow-color*
          wb:*green-color*
          wb:*dark-green-color*
          wb:*light-blue-color*
          wb:*blue-color*
          wb:*purple-color*
          wb:*brown-color*
          wb:*tan-color*
          wb:*light-gray-color*
          wb:*gray-color*
          wb:*dark-gray-color*
          wb:*gray-color* 
          wb:*light-gray-color* 
          wb:*dark-gray-color* 
          wb:*bright-green-color*
          wb:*magenta-color*
          wb:*black-shade*
          wb:*white-shade*
          wb:*light-gray-shade*
          wb:*dark-gray-shade*
          wb:*gray-shade*
          QUAIL-KERNEL::*QUAIL-RESTORE-LISP-FUNCTIONS*
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
        "VIEWS")