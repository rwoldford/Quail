;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                      QUAIL-KERNEL-PACKAGE.LISP                             
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; COPYRIGHT (C) 1990 STATISTICAL COMPUTING LABORATORY, UNIVERSITY OF WATERLOO
;;;
;;;
;;;  AUTHORS:
;;;     GREG ANGLIN 1989, 1990.
;;;     M.E. LEWIS 1991.
;;;
;;;----------------------------------------------------------------------------

(IN-PACKAGE "MAKE")

#+:CL-2
(DEFPACKAGE "QUAIL-KERNEL"
  (:USE "COMMON-LISP") 
  (:NICKNAMES "QK" "quail-kernel")
  (:SHADOW "ARRAY-ELEMENT-TYPE"
           "ARRAY-RANK"
           "ARRAY-DIMENSION"
           "ARRAY-DIMENSIONS"
           "ARRAY-TOTAL-SIZE"
           "ARRAY-IN-BOUNDS-P"
           "ADJUSTABLE-ARRAY-P"
           "ARRAY-ROW-MAJOR-INDEX")
  (:EXPORT "ARRAY-ELEMENT-TYPE"
           "ARRAY-RANK"
           "ARRAY-DIMENSION"
           "ARRAY-DIMENSIONS"
           "ARRAY-TOTAL-SIZE"
           "ARRAY-IN-BOUNDS-P"
           "ADJUSTABLE-ARRAY-P"
           "ARRAY-ROW-MAJOR-INDEX"))

#-:CL-2
(IN-PACKAGE :QUAIL-KERNEL :USE '(PCL LISP) :NICKNAMES '(QK QUAIL-KERNEL ZK))

#-:CL-2
(SHADOW '(ARRAY-ELEMENT-TYPE
          ARRAY-RANK
          ARRAY-DIMENSION
          ARRAY-DIMENSIONS
          ARRAY-TOTAL-SIZE
          ARRAY-IN-BOUNDS-P
          ADJUSTABLE-ARRAY-P
          ARRAY-ROW-MAJOR-INDEX))

#-:CL-2
(EXPORT '(ARRAY-ELEMENT-TYPE
          ARRAY-RANK
          ARRAY-DIMENSION
          ARRAY-DIMENSIONS
          ARRAY-TOTAL-SIZE
          ARRAY-IN-BOUNDS-P
          ADJUSTABLE-ARRAY-P
          ARRAY-ROW-MAJOR-INDEX))

