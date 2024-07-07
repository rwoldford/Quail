;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                              editor-sblx.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     M.E. Lewis 1991.
;;;     R.W. Oldford 1991.
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail)


;;;  Just tries to ensure that expressions can be evaluated from a Fred
;;;  window when the the top-level Quail loop is running.

#|
(defmethod ccl::ed-eval-or-compile-current-sexp :around ((w ccl::fred-mixin))
  (when (quail-running-p)
    (ccl::eval-enqueue (ccl::ed-current-sexp w))
    (ccl::toplevel)
    )
  (call-next-method))

|#

