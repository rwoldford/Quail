;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                        Z-misc-constants.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1990.
;;;
;;;
;;;----------------------------------------------------------------------------------
;;;
;;;                    Miscellaneous Z system constants
;;;
;;;  In this file a number of Z system constants are defined.
;;;  NOTE: Not all system constants are defined here.  Only those which are not
;;;        more reasonably placed in other files.
;;;
;;;


(in-package 'Z)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Handy constants to use programmatically to
;;; determine number of iterations to perform before
;;; asking the user for more information.
;;;

(defconstant *small-number-of-iterations* 10)

(defconstant *reasonable-number-of-iterations* 100)

(defconstant *max-reasonable-number-of-iterations* 1000)
