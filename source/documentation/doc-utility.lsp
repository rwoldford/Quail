;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                            doc-utility.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1991 Statistical Computing Laboratory, University Of Waterloo
;;;
;;;
;;;  Authors:
;;;     M.E. Lewis 1991.
;;;     R.W. Oldford 1991.
;;;
;;;
;;;----------------------------------------------------------------------------


(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(documentable-uses destring string-downcase-object
          string-downcase-list string-first-n...)))

;;;---------------------------------------------------------------------------
;;;
;;;  Miscellaneous utility functions for making documentation.
;;;
;;;---------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;
;;;  Uses of a symbol to be documented.
;;;
;;;-----------------------------------------------------------------------------

(defun documentable-uses (sym &optional package)
  "Returns the list of documentable uses of this symbol in the package ~
   (if specified).  ~
   For example as a variable, a class name, a function, a macro, et cetera."
  (let ((uses '()))
    (if sym
      ;; then process it if we find it
      (and
       (setf sym (if package
                   (find-symbol (string sym) package)
                   (find-symbol (string sym))))
       (progn
         (if (eql :function (function-information sym))
           (cond
            ((generic-function-p (symbol-function sym))
             (push :generic-function uses)
             )
            (T (push :function uses)))
           )
         (if (eql :macro (function-information sym))
             (push :macro uses))
         (if (eql :special-form (function-information sym))
             (push :special-form uses))
         (if (eql :special-operator (function-information sym)) ;27oct05
             (push :special-operator uses)) ;27oct05
         (if (and (boundp sym)
                  (constantp sym))
           (push :constant uses))
         (if (and (boundp sym)
                  (not (constantp sym)))
           (push :variable uses))
         (if (get sym :topic NIL) 
           (push :topic uses))
         (if (find-package sym)
           (push :package uses))
         (cond 
          ((structure-p sym)       (push :structure uses))
          ((built-in-class-p sym)  (push :built-in-class uses))
          ((find-class sym nil)    (push :class uses))))
       )
      ;; else NIL is a constant
      (push :constant uses))
    uses))



(defun list-names-of-docs (doc-list)
  "Returns a list of the names of the documents given in the argument doc-list.  ~
   Duplicates are removed."
  (if (listp doc-list)
    (remove-duplicates (loop for d in doc-list collect (name d)))
    (list (name doc-list))))


(defun symbol-from-doc (doc)
  "Returns the symbol to which the argument, a documentation-object object, ~
   is attached."
  (find-symbol (name doc) (package doc)))

  

;;;-------------------------------------------------------------------------------------
;;;
;;;  Some handy string utility functions.
;;;
;;;
;;;------------------------------------------------------------------------------------

(defun destring (string)
  "Reads and returns the first item in the string."
  (with-input-from-string (s string)
    (read s)))


(defun string-downcase-object (object)
  "Returns the downcased string of the argument print name."
  (string-downcase (format NIL "~a" object)))

(defun string-downcase-list (list-of-objects)
  "Returns a list of the downcased elements of the given list of objects."
  (loop for object in list-of-objects
       collect
       (if (listp object)
         (string-downcase-list object)
         (string-downcase-object object))))

(defun string-first-n... (string &optional (n 50))
  "Returns a string of characters of length no greater than the value ~
   of the second optional argument n.  ~
   The result is either the value of the input string (if the string ~
   has n or fewer characters) or it is a string containing the first ~
   n-3 elements of the input string and ... as its last three elements."
  (if (<= (length string) n)
    string
    (concatenate 'string (subseq string 0 (- n 4)) "...")))


;;;------------------------------------------------------------------------------------
