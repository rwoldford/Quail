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
  #+:sbcl-linux (:USE :clim :clim-lisp :clim-extensions) ;"COMMON-LISP")  ; 19 November 2019
  #+:aclpc-linux (:USE :common-lisp)
  (:NICKNAMES "QK" "quail-kernel")
  ;(:SHADOWING-IMPORT-FROM "COMMON-LISP" "ARRAY") ; 19 November 2019 commented out 07JUN2023 to avoid compile complaint?
  #+:aclpc-linux(:SHADOWING-IMPORT-FROM "ACLMOP" "CLASS-PRECEDENCE-LIST") ; 05 FEB 2020
  #+:aclpc-linux(:SHADOWING-IMPORT-FROM "SYSTEM" "FUNCTION-INFORMATION") ; 05 FEB 2020
  (:SHADOW "ARRAY-ELEMENT-TYPE"
           "ARRAY-RANK"
           "ARRAY-DIMENSION"
           "ARRAY-DIMENSIONS"
           "ARRAY-TOTAL-SIZE"
           "ARRAY-IN-BOUNDS-P"
           "ADJUSTABLE-ARRAY-P"
           "ARRAY-ROW-MAJOR-INDEX"
           "SORT"
           "PACKAGE"
           ;"ARRAY"  ;; see above shadowing-import-from 19 November 2019
           "CLASS-SLOTS"
           ;#-:aclpc-linux "CLASS-PRECEDENCE-LIST" ; see above 23feb2020 It's in :SHADOWING-IMPORT-FROM ABOVE l23
           "CLASS-PROTOTYPE")
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
           "<-" ;; 05FEB2020
           "+INFINITY" ;;07APR2020
           "INFINITY"  ;;07APR2020
           "-INFINITY" ;; 07APR2020
           ;"DIMENSIONED-REF-OBJECT" ;;15AUG2020
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

