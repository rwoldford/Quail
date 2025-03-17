;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               window-basics-package.lsp                             
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
(defpackage #:window-basics
  #+:sbcl-linux (:use :clim-lisp :clim :clim-extensions :clim-listener) ; "COMMON-LISP" 19 November 2019
  #+:aclpc-linux (:use :common-lisp)
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
   "MAKE-POINT" "POINT-X" "POINT-Y" 
   "DRAW-LINE" "DRAW-RECTANGLE" "DRAW-ELLIPSE" "DRAW-POLYGON"
   "MOUSE-DOWN-P" "SHIFT-KEY-P" "CONTROL-KEY-P"
   "MAKE-POSITION" "COPY-POSITION" "POSITION-X" "POSITION-Y"
   "MOVE-TO" "LINE-TO"
   "MAKE-COLOR" "*BLACK-COLOR*" "*WHITE-COLOR*" "*PINK-COLOR*"
    "*RED-COLOR*" "*ORANGE-COLOR*" "*YELLOW-COLOR*" "*GREEN-COLOR*"
    "*DARK-GREEN-COLOR*" "*LIGHT-BLUE-COLOR*" "*BLUE-COLOR*"
    "*PURPLE-COLOR*" "*BROWN-COLOR*" "*TAN-COLOR*"
    "*LIGHT-GRAY-COLOR*" "*GRAY-COLOR*" "*DARK-GRAY-COLOR*" "REDISPLAY" "COLORP"
    "COLOR") ;"COLOR" added 07MAY2024 to avoid sbcl compiler error


  (:export
   "MAKE-POINT" "POINT-X" "POINT-Y" 
   "DRAW-LINE" "DRAW-RECTANGLE" "DRAW-ELLIPSE" "DRAW-POLYGON"
   "MOUSE-DOWN-P" "SHIFT-KEY-P" "CONTROL-KEY-P"
   "MAKE-POSITION" "COPY-POSITION" "POSITION-X" "POSITION-Y"
   "MOVE-TO" "LINE-TO"
   "MAKE-COLOR" "*BLACK-COLOR*" "*WHITE-COLOR*" "*PINK-COLOR*"
    "*RED-COLOR*" "*ORANGE-COLOR*" "*YELLOW-COLOR*" "*GREEN-COLOR*"
    "*DARK-GREEN-COLOR*" "*LIGHT-BLUE-COLOR*" "*BLUE-COLOR*"
    "*PURPLE-COLOR*" "*BROWN-COLOR*" "*TAN-COLOR*"
    "*LIGHT-GRAY-COLOR*" "*GRAY-COLOR*" "*DARK-GRAY-COLOR*")
  (:export #:window-basics))

;#-:cl-2
;(in-package "WINDOW-BASICS" :use '(pcl lisp) :nicknames '(wb))

;#-:cl-2
;(export
; '(*CANVAS* MAKE-CANVAS))

;#-:CL-2
;(IMPORT '(QUAIL-KERNEL::*QUAIL-RESTORE-LISP-FUNCTIONS*
;          QUAIL-KERNEL:*QUAIL-STANDARD-INPUT* 
;          QUAIL-KERNEL:*QUAIL-STANDARD-OUTPUT* 
;          QUAIL-KERNEL:*QUAIL-QUERY-IO*
;          QUAIL-KERNEL:*QUAIL-DEBUG-IO*
;          QUAIL-KERNEL:*QUAIL-ERROR-OUTPUT*
;          QUAIL-KERNEL:*QUAIL-TRACE-OUTPUT*
;          QUAIL-KERNEL:*QUAIL-TERMINAL-IO*
;          QUAIL-KERNEL:QUAIL-ERROR
;          QUAIL-KERNEL:QUAIL-CERROR
;          QUAIL-KERNEL:QUAIL-QUERY
;          QUAIL-KERNEL:QUAIL-Y-OR-N-P 
;          QUAIL-KERNEL:QUAIL-YES-OR-NO-P)
;        "WINDOW-BASICS")
