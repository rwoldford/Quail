;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               redefine-quail-io.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1992.
;;;
;;;
;;;----------------------------------------------------------------------------------
;;;

(in-package :qk)

(when (find-package :window-basics)
  
  (defun quail-y-or-n-p (&optional format-string &rest format-args)
    "Prints a message from format-string and format-args, followed by (y or n), ~
     and waits for the user to type y or n. ~
     Returns T if the user typed y, or nil if the user typed n.  ~
     (:see-also y-or-n-p quail-yes-or-no-p prompt-t-or-f quail-query)"
    ;;(apply #'y-or-n-p format-string format-args)
    (wb::prompt-t-or-f
     (if format-string
       (apply #'format NIL format-string format-args)
       "") :true-text "Yes" :false-text "No")
    )
  
  
  (defun quail-yes-or-no-p (&optional format-string &rest format-args)
    "Prints a message from format-string  and format-args, followed by (yes or no), ~
     and waits for the user to type yes or no followed by a carriage return.  ~
     Returns t if the user typed yes,  nil if the user typed no.  ~
     (:see-also yes-or-no-p quail-y-or-n-p prompt-true-or-false)"
    ;;(apply #'yes-or-no-p format-string format-args)
    (wb::prompt-true-or-false 
     (if format-string
       (apply #'format NIL format-string format-args)
       "") :true-text "Yes" :false-text "No"))
  
  (defun quail-query (format-string &rest args)
    "Prompts the user with format-string and reads in user-typed expression.  ~
     (:see-also prompt-user quail-yes-or-no-p *quail-query-io* quail-y-or-n-p ~
     quail-query)"
    (wb::prompt-user
     :result-type T ;24NOV2024
     :read-type :read
     :prompt-string (apply #'format NIL format-string args))
    )
  
  (setf *help-in-windows* T)
  )