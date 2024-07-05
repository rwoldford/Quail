;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               quail-package.lisp                             
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1989, 1990, 1992.
;;;     R.W. Oldford 1992.
;;;
;;;----------------------------------------------------------------------------

#+:cl-2
(defpackage "QUAIL"
  (:use "VIEWS" "NEW-MATH" "QUAIL-KERNEL" "COMMON-LISP")
  (:SHADOWING-IMPORT-FROM "NEW-MATH"
                          "ARRAY-ELEMENT-TYPE"
                          "ARRAY-RANK"
                          "ARRAY-DIMENSION"
                          "ARRAY-DIMENSIONS"
                          "ARRAY-TOTAL-SIZE"
                          "ARRAY-IN-BOUNDS-P"
                          "ADJUSTABLE-ARRAY-P"
                          "ARRAY-ROW-MAJOR-INDEX"
                          "SORT"
                          "+" "-" "*" "/"
                          "FLOAT" "RATIONAL"
                          "RATIONALIZE"
                          "<" "<=" "=" ">" ">=" "MIN" "MAX" "EXP" "SQRT" "ISQRT"
                          "ABS" "PHASE" "SIGNUM" 
                          "SIN" "COS" "TAN" "CIS" "ASIN" "ACOS" "ATAN"
                          "SINH" "COSH" "TANH" "ASINH" "ACOSH" "ATANH"  "NUMERATOR" 
                          "DENOMINATOR" "REALPART" "IMAGPART"
                          "FLOOR" "CEILING" "TRUNCATE" "ROUND"
                          "FFLOOR" "FCEILING" "FTRUNCATE" "FROUND"
                          "COMPLEX" "EXPT" "LOG" "REM" "MOD"
                          "GCD" "LCM" "CONJUGATE"
                          "LOGIOR" "LOGXOR" "LOGAND" "LOGEQV" "LOGNAND"
                          "LOGNOR" "LOGANDC1" "LOGANDC2"
                          "LOGORC1""LOGORC2" "LOGNOT" "LOGTEST"
                          "LOGBITP" "ASH" "LOGCOUNT" "INTEGER-LENGTH" "BOOLE")
  ;(:IMPORT-FROM "QUAIL-KERNEL" "<-")
  ;(:IMPORT-FROM "VIEWS" "ROTATING-PLOT") ; 05 FEB 2020
  (:nicknames "Q")
  )

#-:cl-2
(in-package "QUAIL" :use '(new-math quail-kernel pcl lisp) :nicknames '(q))

