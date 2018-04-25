;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               synonym.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1989, 1990, 1991.
;;;
;;;
;;;----------------------------------------------------------------------------
;;;
;;;  Includes:
;;;           make-synonym
;;;           alias
;;;          _           
;;;          **
;;;          ^
;;;

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(make-synonym)))

;;;----------------------------------------------------------------------------
;;;
;;;  MAKE-SYNONYM:
;;;
;;;  A useful but potentially dangerous macro that lets the user
;;;  introduce synonyms for existing or user defined functions.
;;;
;;;  Danger can be eliminated by disallowing redefinition of existing
;;;  functions.
;;;  
;;;----------------------------------------------------------------------------

(defmacro make-synonym (&key old new (warn t))
  "Makes the argument new a synonym for the argument old.  ~
   If warn is T (the default), the user is warned when new is ~
   already defined."
  (declare (special *quail-query-io*))
  (if (and (fboundp new)
           (or (symbol-function new)
               (macro-function new))
           warn)
    ;; then
    (if
      (quail-yes-or-no-p "WARNING: ~S is already a defined function! ~%~
                          Are you sure you want to replace its definition~%~
                          by that of ~S?" new old)
      
      ;; if yes, then
      `(defmacro ,new (&rest args)
         ,(format nil "~a is simply a synonym for ~a.  See make-synonym."
                  new old)
         (append (quote (,old)) args))
      
      ;; else
      (format *quail-query-io* "Aborted. ~S is not redefined." new)
      )
    
    ;; else
    `(defmacro ,new (&rest args)
       ,(format nil "~a is simply a synonym for ~a.  See make-synonym."
                new old)
       (append (quote (,old)) args))))
