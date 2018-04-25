;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                        special-vars.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; copyright (c) 1990 statistical computing laboratory, university of waterloo
;;;
;;;
;;;  authors:
;;;     r.w. oldford 1990.
;;;
;;;
;;;----------------------------------------------------------------------------------
;;;
;;;                    miscellaneous quail system constants
;;;
;;;  in this file a number of quail system constants are defined.
;;;  note: not all system constants are defined here.  only those which are not
;;;        more reasonably placed in other files.
;;;
;;;


(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(*small-number-of-iterations* *reasonable-number-of-iterations*
          *max-reasonable-number-of-iterations*
          )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; handy constants to use programmatically to
;;; determine number of iterations to perform before
;;; asking the user for more information.
;;;

(defvar *small-number-of-iterations*  10)

(defvar *reasonable-number-of-iterations* 100)

(defvar *max-reasonable-number-of-iterations* 1000)

