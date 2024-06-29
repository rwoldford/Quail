;;; -*- MODE: LISP; SYNTAX: COMMON-LISP -*-
;;; NEW-MATH-PACKAGE.LISP
;;; COPYRIGHT, 1991, STATISTICAL COMPUTING LABORATORY, UNIVERSITY OF WATERLOO

(DEFPACKAGE "NEW-MATH"
  #+:sbcl-linux (:USE :clim :clim-lisp :clim-extensions) ;"COMMON-LISP") ;19November 2019
  #+:aclpc-linix (:USE :common-lisp)
  (:SHADOWING-IMPORT-FROM "QUAIL-KERNEL"
                          "ARRAY-ELEMENT-TYPE"
                          "ARRAY-RANK"
                          "ARRAY-DIMENSION"
                          "ARRAY-DIMENSIONS"
                          "ARRAY-TOTAL-SIZE"
                          "ARRAY-IN-BOUNDS-P"
                          "ADJUSTABLE-ARRAY-P"
                          "ARRAY-ROW-MAJOR-INDEX"
                          "SORT")
  (:IMPORT-FROM "QUAIL-KERNEL"
                "DIMENSIONED-REF-OBJECT" ;;22JAN2020
                "MATRIX" ;;22JAN2020
                "SEQ-DIFFERENCE" ;;22JAN2020
                "DEFMETHOD-MULTI"
                "EXT_+"
                "EXT_*"
                "EXT_-"
                "EXT_/"
                "EXT_>"
                "EXT_<"
                "EXT_="
                "EXT_<="
                "EXT_>="
                "EXT_MAX"
                "EXT_MIN"
                "EXT_EXP"
                "EXT_SQRT"
                "EXT_EXP"
                "EXT_SQRT"
                "EXT_ISQRT"
                "EXT_ABS"
                "EXT_PHASE"
                "EXT_SIGNUM"
                "EXT_SIN"
                "EXT_COS"
                "EXT_TAN"
                "EXT_CIS"
                "EXT_ASIN"
                "EXT_ACOS"
                "EXT_ATAN"
                "EXT_SINH"
                "EXT_COSH"
                "EXT_TANH"
                "EXT_ASINH"
                "EXT_ACOSH"
                "EXT_ATANH"
             ;;   "EXT_RATIONALIZE"
             ;;   "EXT_RATIONAL"
             ;;   "EXT_FLOAT"
                "EXT_NUMERATOR" 
                "EXT_DENOMINATOR"
                "EXT_REALPART"
                "EXT_IMAGPART"
                "EXT_FLOOR"
                "EXT_CEILING"
                "EXT_TRUNCATE"
                "EXT_ROUND"
                "EXT_FFLOOR"
                "EXT_FCEILING"
                "EXT_FTRUNCATE"
                "EXT_FROUND"
             ;;   "EXT_COMPLEX"
                "EXT_EXPT"
                "EXT_LOG"
                "EXT_REM"
                "EXT_MOD"
                "EXT_GCD"
                "EXT_LCM"
                "EXT_CONJUGATE"
                ;;"GET-LAMBDA-LIST"
                "MAP-ELEMENT" ;; 03FEB2020
           "+INFINITY" ;;07APR2020
           "INFINITY"  ;;07APR2020
           "-INFINITY" ;; 07APR2020
           "EREF" ;; 12JAN2021
           "MATRIX-DIMENSIONS-OF" ;; 12JAN2021
           "MISSING-METHOD" ;; 12JAN2021
           "NUMBER-OF-ELEMENTS" ;; 12JAN2021
           "REDUCE-SLICES" ;; 12JAN2021
           )
                
  (:SHADOW
   "+" "-" "*" "/"
   "FLOAT" "RATIONAL"
   "RATIONALIZE"
   "<" "<=" "=" ">" ">=" "MIN" "MAX" "EXP" "SQRT" "ISQRT"
   "ABS" "PHASE" "SIGNUM" "SIN" "COS" "TAN" "CIS" "ASIN" "ACOS" "ATAN"
   "SINH" "COSH" "TANH" "ASINH" "ACOSH" "ATANH"
   "NUMERATOR" 
   "DENOMINATOR" "REALPART" "IMAGPART" "FLOOR" "CEILING" "TRUNCATE" "ROUND"
   "FFLOOR" "FCEILING" "FTRUNCATE" "FROUND" "COMPLEX" "EXPT" "LOG" "REM" "MOD"
   "GCD" "LCM" "CONJUGATE"
   "LOGIOR" "LOGXOR" "LOGAND" "LOGEQV" "LOGNAND" "LOGNOR" "LOGANDC1" "LOGANDC2"
   "LOGORC1""LOGORC2" "LOGNOT" "LOGTEST" "LOGBITP" "ASH" "LOGCOUNT" "INTEGER-LENGTH"
   "BOOLE")
  (:EXPORT
   "+" "-" "*" "/"
    "FLOAT" "RATIONAL"
    "RATIONALIZE"
   "<" "<=" "=" ">" ">=" "MIN" "MAX" "EXP" "SQRT" "ISQRT"
   "ABS" "PHASE" "SIGNUM" "SIN" "COS" "TAN" "CIS" "ASIN" "ACOS" "ATAN"
   "SINH" "COSH" "TANH" "ASINH" "ACOSH" "ATANH"  "NUMERATOR" 
   "DENOMINATOR" "REALPART" "IMAGPART" "FLOOR" "CEILING" "TRUNCATE" "ROUND"
   "FFLOOR" "FCEILING" "FTRUNCATE" "FROUND" "COMPLEX" "EXPT" "LOG" "REM" "MOD"
   "GCD" "LCM" "CONJUGATE"
   "LOGIOR" "LOGXOR" "LOGAND" "LOGEQV" "LOGNAND" "LOGNOR" "LOGANDC1" "LOGANDC2"
   "LOGORC1""LOGORC2" "LOGNOT" "LOGTEST" "LOGBITP" "ASH" "LOGCOUNT" "INTEGER-LENGTH"
   "BOOLE"))
