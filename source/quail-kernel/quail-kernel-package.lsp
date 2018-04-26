;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                      quail-kernel-package.lisp                             
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1990 Statistical Computing Laboratory, University Of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1989, 1990.
;;;     M.E. Lewis 1991.
;;;
;;;----------------------------------------------------------------------------

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
           "ARRAY-ROW-MAJOR-INDEX"
           "SORT"
           "PACKAGE")
  (:EXPORT "ARRAY-ELEMENT-TYPE"
           "ARRAY-RANK"
           "ARRAY-DIMENSION"
           "ARRAY-DIMENSIONS"
           "ARRAY-TOTAL-SIZE"
           "ARRAY-IN-BOUNDS-P"
           "ADJUSTABLE-ARRAY-P"
           "ARRAY-ROW-MAJOR-INDEX"
           "SORT"
           "SORT-OBJECT"
           ))

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
          ARRAY-ROW-MAJOR-INDEX
          SORT))
#-:CL-2
(EXPORT '(ARRAY-ELEMENT-TYPE
          ARRAY-RANK
          ARRAY-DIMENSION
          ARRAY-DIMENSIONS
          ARRAY-TOTAL-SIZE
          ARRAY-IN-BOUNDS-P
          ADJUSTABLE-ARRAY-P
          ARRAY-ROW-MAJOR-INDEX))

