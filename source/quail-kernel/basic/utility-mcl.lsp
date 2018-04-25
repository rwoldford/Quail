;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               utility-mcl.lisp                               
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
(eval-when (compile load eval)

  "A gimmick to accomodate both CLtL 1 and 2.  The package ~
   containing the LISP language is called lisp in one and ~
   common-lisp in the other.  Here they are given the same ~
   nickname."

  (let ((old-package-name
         (package-name (find-package "LISP" )))
        (old-package-nicknames
         (package-nicknames (find-package "LISP")))
       )
    (rename-package
     (find-package "LISP")
     old-package-name
     (append (list "CL" "COMMON-LISP") old-package-nicknames))))


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
  (let ((arglist (ccl:arglist symbol)))
    (when (member 'ccl::&method arglist)
      (setf arglist (cddr arglist)))
    (tree-apply #'(lambda (x) (if (symbolp x)
                                (intern (string x))
                                x))
                arglist)))
