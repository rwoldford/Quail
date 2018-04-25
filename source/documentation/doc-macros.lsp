;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                           doc-macros.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1992 Statistical Computing Laboratory, University Of Waterloo
;;;
;;;
;;;  Authors:
;;;     M.E. Lewis 1992.
;;;
;;;
;;;----------------------------------------------------------------------------

(in-package :quail-kernel)

(defmacro def-search-function (function-name search-path)
  `(defun ,function-name (key-tree)
     (value (find-if (share-family-tree-p key-tree ',search-path) key-tree))))

(defun prettify (result)
  (cond ((null result) nil)
        ((atom result) (format nil "~a" result))
        (t
         (format nil "~{~a ~}" result))))
  
