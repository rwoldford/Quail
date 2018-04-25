;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               cl-extensions.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1988-1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     G. Desvignes 1988,1989
;;;     R.W. Oldford 1988-1992
;;;
;;;----------------------------------------------------------------------------------
;;;
;;;
;;; Common Lisp Extensions
;;;
;;;


(in-package :cl)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(;;generic-function-p
          objectp
          ;;inspect-object
          ;;inspect
          )))
#|
(defun generic-function-p (symbol)
  "True if symbol names a generic-function, NIL otherwise."
  (if (and (fboundp symbol)
           (typep (symbol-function symbol)
                  'standard-generic-function))
    t nil))
|#
(defun objectp (object)
  "Return T if object is of type standard-object."
  (typep object 'standard-object))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Make inspect behave analogously to CL's describe by
;;;  having it call the generic function inspect-object.
;;;  Should be shadowed elsewhere.
;;;

#|
(setf (symbol-function 'CL-inspect) (symbol-function 'inspect))
(setf (symbol-function 'inspect)
      (function (lambda (thing)
                  "Calls the generic function inspect-object on its argument."
                  (inspect-object thing))))

(defgeneric inspect-object (thing)
  (:documentation "Used to provide an interactive inspector to examine ~
                   and potentially modify the contents of the argument."))

(defmethod inspect-object ((thing t))
  (CL-inspect thing))
|#
