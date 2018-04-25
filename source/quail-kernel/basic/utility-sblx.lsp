;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               utility-sblx.lsp                               
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
;;; A copy of utility-excl.lisp <= ***
;;; There is no EXCL package in aclwin
;;;
;;;  Includes:
;;;
;;;    get-lambda-list

(in-package :quail-kernel)


(eval-when (:compile-toplevel :load-toplevel :execute) (export '(structure-p built-in-class-p)))

#+:cl-1(eval-when (:compile-toplevel :load-toplevel :execute)
 
  "A gimmick to accomodate CLtL 1.  The package ~
   containing the LISP language is called lisp in CLtL 1 but ~
   common-lisp in CLtL 2.  Here LISP is given the same ~
   nicknames."
 
  (let ((old-package (find-package "LISP")))
    (excl::enter-new-nicknames old-package (list "COMMON-LISP" "CL")))
  )


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
  (format t "~%symbol is ~s of type ~s " symbol (type-of symbol))
  (let ((lam-list 
         (sb-introspect:function-lambda-list symbol);(acl::lambda-list symbol) see spr29023
         ))
      (if (equal lam-list :unknown)
         (list :unknown)
         (let
          ((args
            (tree-apply #'(lambda (x) (if (symbolp x)
                                         (intern (string x))
                                         x))
              lam-list)))
          (labels
           ((get-first-atom (x)
              (cond
               ((symbolp x) x)
               ((listp x) (get-first-atom (first x)))))
            )
           (loop for arg in args
             collect (get-first-atom arg)))))
      )
  )
