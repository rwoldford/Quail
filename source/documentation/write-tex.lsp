;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               write-tex.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; copyright (c) 1990 statistical computing laboratory, university of waterloo
;;;
;;;
;;;  authors:
;;;     m.e. lewis 1991.
;;;     r.w. oldford 1991.
;;;
;;;
;;;----------------------------------------------------------------------------



(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(write-tex)))



;;;----------------------------------------------------------------------------
;;;
;;;  The generic TeX write function:
;;;
;;;        write-tex
;;;
;;;---------------------------------------------------------------------------


(defgeneric write-tex (destination-file thing)
  (:documentation "Writes TeX commands to the destination-file ~
                   to produce a TeX version of the second argument."))


;;;---------------------------------------------------------------------------
;;;
;;;  The default case: T
;;;
;;;  Just produces the printed representation of the argument.
;;;
;;;---------------------------------------------------------------------------

(defmethod write-tex (destination-file doc)
  (write-tex-value destination-file doc))
