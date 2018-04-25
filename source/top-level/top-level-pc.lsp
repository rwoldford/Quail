;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                              top-level-pc.lsp                               
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(*system-top-level-loop-function* install-top-level current-top-level
          get-next-form get-function-name setup-system-top-level-loop-variable)))

(defvar *system-top-level-loop-function* NIL
  "Variable containing the stack of top-level loop functions.")


(defun setup-system-top-level-loop-variable ()
  (declare (special *system-top-level-loop-function*))
  (setf *system-top-level-loop-function* NIL )) ;;#'ccl:toplevel-loop))
;;;----------

(eval-when (load eval)
  (setup-system-top-level-loop-variable))

;;;;;;;;;;;;
;;;
;;;

(defun install-top-level (&optional
                          (top-level *system-top-level-loop-function*))
  "Installs the top-level function, if provided.  Default ~
   is the one supplied with the system."
  (declare (special *system-top-level-loop-function*))
  (inform-user "New top level functions cannot be installed yet in ~
                this system.")
  ;;(ccl:%set-toplevel top-level)
  ;;(ccl:toplevel)
   )

(defun current-top-level ()
  "Returns the current top-level loop function."
  ;;(ccl:%set-toplevel)
   (inform-user "New top level functions cannot be installed yet in ~
                this system.")
   )

(defun get-next-form ()
  "Retrieves the next form to be evaluated."
  (declare (special *forms-awaiting-evaluation*))
  (or ;; don't know how to do this yet
      ;; (ccl:get-next-queued-form) 
      (pop *forms-awaiting-evaluation*)
      (quail-toplevel-reader-function)))

(defun get-function-name (function)
  (qk::function-name function))

;;;(defun set-listener-package (&optional (package :cl-user))
;;;  (ccl:eval-enqueue `(in-package ,package)))

(defun system-quit-lisp ()
  "System dependent function that quits Lisp."
  (excl::exit) ;(acl:quit) from help 15jun2004
  )
