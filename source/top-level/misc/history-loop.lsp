;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                              history.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     M.E. Lewis
;;;     R.W. Oldford 1991.
;;;
;;;--------------------------------------------------------------------------------


(defclass foo ()
  ((form :initarg :form :accessor form-of)
   (back-links :initarg :back-links :accessor back-links-of :initform NIL)
   (to-links :initarg :to-links :accessor to-links-of :initform NIL)))

(defmethod link ((a foo) (b foo))
  (push a (back-links-of b))
  (push b (to-links-of a)))

(defmethod link (a b)
  )

(defmethod set-spawning-expression (thing expression)
  )

(defmethod set-spawning-expression ((thing foo) expression)
  (setf (form-of thing) expression))

(defun map-top (&aux f)
  (print 'map?)
  (setq f (read))
  (cond
   ((eq f :end-map)
    (%set-toplevel #'quail-top))
   ((listp f)
    (let* ((args (loop for a in (cdr f) collect (eval a)))
           (result (eval `(,(car f) ,@args))))
      (loop for a in args do (link a result))
      (set-spawning-expression result (append (list (car f)) args))
      (setf +++ ++)
      (setf ++ +)
      (setf + f)
      (setf *** **)
      (setf ** *)
      (setf * (eval f))
      (print * *terminal-io*)))
   (T "?")))

(defun my-map ()
  (format *terminal-io* "history on")
  (%set-toplevel #'map-top)
  (toplevel))