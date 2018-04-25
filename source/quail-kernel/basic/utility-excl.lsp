;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               utility-excl.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1989, 1990, 1991.
;;;     R.W. Oldford 1991
;;;     M.E. Lewis 1991
;;;     Greg Anglin 1993, -excl port
;;;
;;;--------------------------------------------------------------------------------
;;;
;;;  Includes:
;;;
;;;    get-lambda-list

(in-package :quail-kernel)


(eval-when (:compile-toplevel :load-toplevel :execute) (export '(structure-p built-in-class-p)))

;;;---------------------------------------------------------------------------------
;;; preliminaries
;;;

#+:cl-1
(eval-when (:compile-toplevel :load-toplevel :execute)
 
  "A gimmick to accomodate CLtL 1.  The package ~
   containing the LISP language is called lisp in CLtL 1 but ~
   common-lisp in CLtL 2.  Here LISP is given the same ~
   nicknames."
 
  (let ((old-package (find-package "LISP")))
    (excl::enter-new-nicknames old-package (list "COMMON-LISP" "CL")))
  )

;;;----------------------

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
         'built-in-class)))

(defun get-lambda-list (symbol)
  (tree-apply #'(lambda (x) (if (symbolp x)
                              (intern (string x))
                              x))
              (excl:arglist symbol)))

