;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               window-basics-package.lisp                             
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
(defpackage "WINDOW-BASICS"
  (:use "COMMON-LISP")
  (:nicknames "WB")
  (:import-from "QUAIL-KERNEL"
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
  (:shadow
   "MAKE-POINT" "MOUSE-DOWN-P" "SHIFT-KEY-P" "CONTROL-KEY-P"
   "MAKE-COLOR" "*BLACK-COLOR*" "*WHITE-COLOR*" "*PINK-COLOR*"
    "*RED-COLOR*" "*ORANGE-COLOR*" "*YELLOW-COLOR*" "*GREEN-COLOR*"
    "*DARK-GREEN-COLOR*" "*LIGHT-BLUE-COLOR*" "*BLUE-COLOR*"
    "*PURPLE-COLOR*" "*BROWN-COLOR*" "*TAN-COLOR*"
    "*LIGHT-GRAY-COLOR*" "*GRAY-COLOR*" "*DARK-GRAY-COLOR*")
  (:export
   "MAKE-POINT" "MOUSE-DOWN-P" "SHIFT-KEY-P" "CONTROL-KEY-P"
   "MAKE-COLOR" "*BLACK-COLOR*" "*WHITE-COLOR*" "*PINK-COLOR*"
    "*RED-COLOR*" "*ORANGE-COLOR*" "*YELLOW-COLOR*" "*GREEN-COLOR*"
    "*DARK-GREEN-COLOR*" "*LIGHT-BLUE-COLOR*" "*BLUE-COLOR*"
    "*PURPLE-COLOR*" "*BROWN-COLOR*" "*TAN-COLOR*"
    "*LIGHT-GRAY-COLOR*" "*GRAY-COLOR*" "*DARK-GRAY-COLOR*"))

#-:cl-2
(in-package "WINDOW-BASICS" :use '(pcl lisp) :nicknames '(wb))

#-:cl-2
(export
 '(*CANVAS* MAKE-CANVAS))

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
        "WINDOW-BASICS")
