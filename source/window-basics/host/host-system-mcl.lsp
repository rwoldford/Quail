;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                        host-system-mcl.lisp                             
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1991.
;;;
;;;
;;;--------------------------------------------------------------------------------

(in-package :wb)

(use-package :ccl)

;;; library files record.lisp and traps.lisp must be available
;;; when evalling or compiling.
;;; hostdraw is made a package and some file changes are here as well.

(eval-when (eval load compile)
 ;; (make::require :records)  ;<-- *** host requires these already!
  (make::require :traps)    ;<-- *** ... rwo
  (make::require :scrollers)
  )


