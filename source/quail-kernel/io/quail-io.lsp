;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               quail-io.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1990.
;;;     M.E. Lewis 1991.
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
;;;       *quail-help-io*
;;;
;;;  Functions:
;;;       quail-print             - print function
;;;       quail-error             - error function.
;;;       quail-cerror            - continuable quail-error.
;;;       quail-query             - prompt user for a form to evaluate.
;;;       quail-y-or-n-p          - ask user a question; wait for single character
;;;                                 y or n response.
;;;       quail-yes-or-no-p       - ask user a question; wait for multiple character
;;;                                 yes or no response.
;;;      
;;;-------------------------------------------------------------------------------------

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(*quail-standard-input* 
          *quail-standard-output* 
          *quail-query-io*
          *quail-debug-io*
          *quail-error-output*
          *quail-trace-output*
          *quail-terminal-io*
          *quail-help-io*
          quail-print
          quail-error
          quail-cerror
          quail-query
          quail-y-or-n-p 
          quail-yes-or-no-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;

(defvar *quail-standard-output* (make-synonym-stream '*standard-output*))

(defvar *quail-standard-input* (make-synonym-stream '*standard-input*))

(defvar *quail-query-io* (make-synonym-stream '*query-io*))

(defvar *quail-debug-io* (make-synonym-stream '*debug-io*))

(defvar *quail-error-output* (make-synonym-stream '*error-output*))

(defvar *quail-trace-output* (make-synonym-stream '*trace-output*))

(defvar *quail-terminal-io* (make-synonym-stream '*terminal-io*))

(defvar *quail-help-io* (make-synonym-stream '*terminal-io*)
  "The stream to be used by Quail's online help function.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;

(defun quail-print (object &optional output-stream)
  "Outputs the printed representation of data object to output-stream, ~
   preceded by a newline and followed by a space. Returns object."
  (print object output-stream))

(defun quail-error (datum &rest args)
  "Invokes the signal facility on a condition. If the condition is not handled, ~
   (invoke-debugger condition) is executed. "
  (apply #'error datum args))

(defun quail-cerror (continue-format-string datum &rest args)
  "Invokes the signal facility on a condition. If the condition is not handled, ~
   (invoke-debugger condition) is executed. While signaling is going on, ~
   it is possible to return from cerrror by invoking continue. cerror returns nil."
  (apply #'cerror continue-format-string datum args))
#|
(defmacro quail-error (datum &rest args)
  "Invokes the signal facility on a condition. If the condition is not handled, ~
   (invoke-debugger condition) is executed. "
  `(error , datum ,@args))

(defmacro quail-cerror (continue-format-string datum &rest args)
  "Invokes the signal facility on a condition. If the condition is not handled, ~
   (invoke-debugger condition) is executed. While signaling is going on, ~
   it is possible to return from cerrror by invoking continue. cerror returns nil."
  `(cerror ,continue-format-string ,datum ,@args))
|#

(defun quail-y-or-n-p (&optional format-string &rest format-args)
  "Prints a message from format-string and format-args, followed by (y or n), ~
   and waits for the user to type y or n. ~
   Returns T if the user typed y, or nil if the user typed n.  ~
   (:see-also y-or-n-p quail-yes-or-no-p prompt-t-or-f quail-query)"
  (apply #'y-or-n-p format-string format-args)
  )
   

(defun quail-yes-or-no-p (&optional format-string &rest format-args)
  "Prints a message from format-string  and format-args, followed by (yes or no), ~
   and waits for the user to type yes or no followed by a carriage return.  ~
   Returns t if the user typed yes,  nil if the user typed no.  ~
   (:see-also yes-or-no-p quail-y-or-n-p prompt-true-or-false)"
  (apply #'yes-or-no-p format-string format-args))

(defun quail-query (format-string &rest args)
  "Prompts the user with format-string and reads in user-typed expression.  ~
   (:see-also prompt-user quail-yes-or-no-p *quail-query-io* quail-y-or-n-p ~
   quail-query)"
  (declare (special *quail-query-io*))
  (apply #'format *quail-query-io* format-string args)
  (read *quail-query-io*)
  )

