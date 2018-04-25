;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               quail-system-io.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1990.
;;;
;;;
;;;----------------------------------------------------------------------------------
;;;
;;;                       QUAIL SYSTEM INPUT/OUTPUT
;;;
;;;  In this file the officially sanctioned functions for direct communication
;;;  with the user of quail software are defined.  So are the io-streams for this
;;;  communication. 
;;;
;;;  Variables:
;;;       *quail-standard-input* 
;;;       *quail-standard-output*
;;;       *quail-query-io*
;;;
;;;  Functions:
;;;       quail-error            - error function.
;;;       quail-cerror           - continuable error.
;;;       quail-query           - prompt user for a form to evaluate.
;;;       quail-y-or-n-p        - ask user a question; wait for single character
;;;                           y or n response.
;;;       quail-yes-or-no-p     - ask user a question; wait for multiple character
;;;                           yes or no response.
;;;
;;;-------------------------------------------------------------------------------------

(defvar *quail-standard-input* (make-synonym-stream *standard-input*))

(defvar *quail-standard-output* (make-synonym-stream *standard-output*))

(defvar *quail-query-io* (make-synonym-stream *query-io*))

(defun quail-error (format-string &rest args)
  "See error function for documentation."
  (apply #'error  format-string args))

(defun quail-cerror (continue-format-string error-format-string &rest args)
  "See cerror function for documentation."
  (apply #'cerror (append (list continue-format-string error-format-string) args)))

(defun quail-query (format-string &rest args)
  "Prompts the user with format-string and reads in user-typed expression."
  (declare (special *quail-query-io*))
  (apply #'format *query-io* format-string args)
  (read *query-io*))
  
(defun quail-y-or-n-p (&optional format-string &rest args)
  "See y-or-n-p function for documentation. ~%~
   Difference is all i/o is performed using the stream ~%~
   that is the value of *quail-query-io* ."
  ;(declare (special *quail-query-io*))
  ;(let ((*query-io* *quail-query-io*))                 ;Shadowing doesn't seem to work
    (apply #'y-or-n-p  format-string args)
    ;)
  )
  
(defun quail-yes-or-no-p (&optional format-string &rest args)
  "See yes-or-no-p function for documentation. ~%~
   Difference is all i/o is performed using the stream ~%~
   that is the value of *quail-query-io* ."
  ;(declare (special *quail-query-io*))
  ;(let ((*query-io* *quail-query-io*))                 ;Shadowing doesn't seem to work
    (apply #'yes-or-no-p  format-string args)
    ;)
  )
