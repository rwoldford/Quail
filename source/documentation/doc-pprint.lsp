;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               doc-pprint.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1991.
;;;     M.E. Lewis 1991.
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail-kernel)

#|
;; pretty print is part of MCL 2.0 final, distinguished by :cltl2
;;     ... dga 92 10 19

#+(and :ccl-2 (not :cltl2))
(eval-when (load compile)
           (load "ccl-Library;lisp-package"))

#+(and :ccl-2 (not :cltl2))
(eval-when (load compile)
           (load "ccl-xp;xp"))

(defmacro doc-pprint (&rest args)
  #+(and :ccl-2  (not :cltl2))                `(xp::pprint ,.args)
  #+(or (and :cltl2 :ccl-2) (not :ccl-2))     `(pprint ,.args)
  )
|#

(defmacro doc-pprint (&rest args)
