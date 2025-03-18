;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               quail-user-package.lisp                             
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1989, 1990.
;;;
;;;
;;;----------------------------------------------------------------------------

#+:cl-2
(defpackage "QUAIL-USER" 
  #+:sbcl-linux (:use "QUAIL" :clim-lisp :clim :clim-extensions)
  #+:aclpc-linux (:use :common-lisp)
  ;(:use "QUAIL" "COMMON-LISP")
  (:IMPORT-FROM "QUAIL-KERNEL"
                 "<-"  ;28JUN2020
                 "CGLUE" ;03AUG2023
                 "REF") ;03AUG2023
  (:IMPORT-FROM "VIEWS"
                 "DATASET") ;28JUN2020
  (:SHADOWING-IMPORT-FROM "QUAIL"
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
                          "LOGBITP" "ASH" "LOGCOUNT" "INTEGER-LENGTH"
                          "BOOLE")
  (:nicknames "Q-USER" "quail-user"))
#-:cl-2
(in-package "QUAIL-USER" 
            :use '("QUAIL" "PCL" "LISP")
            :nicknames '("Q-USER" "quail-user"))
