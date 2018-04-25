;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                            prompt-mixin.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1992.
;;;
;;;
;;;----------------------------------------------------------------------------

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(prompt-mixin)))

(defclass prompt-mixin ()
  ()
  (:documentation
   "This class specializes the prompt  methods of web-editor for quail-browser ~
    so that a *quail-prompt-window* is used for the prompt.  ~
    the place of this mixin in the inheritance list of quail-browser or ~
    quail-object is important."))

(defmethod prompt-eval ((self prompt-mixin)  msg)
  ;;"Specialization."
  
  (wb::prompt-user :read-type :eval :prompt-string msg ))

(defmethod prompt-read ((self prompt-mixin) msg)
  ;; 
  ;; Specialization
  ;; 
  
  (wb::prompt-user :read-type :read :prompt-string msg))

