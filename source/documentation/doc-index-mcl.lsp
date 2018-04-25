;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                                doc-index.lisp                               
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

(eval-when (:compile-top-level :load-toplevel :execute) (export '(structure-p built-in-class-p)))

 (defun structure-p (symbol)
  "Returns T if the given symbol is the name of a structure, NIL otherwise."
  (let ((the-class-itself (find-class symbol NIL)))
    (eql (and the-class-itself
              (class-name (class-of the-class-itself)))
         'structure-class)))

 (defun built-in-class-p (symbol)
  "Returns T if the given symbol is the name of a built in class, NIL otherwise."
  (let ((the-class-itself (find-class symbol NIL)))
    (eql (and the-class-itself
              (class-name (class-of the-class-itself)))
