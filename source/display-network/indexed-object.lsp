;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               indexed-object.lisp                               
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(indexed-object
          )))

(defclass indexed-object ()
  ()
  (:documentation
   "Adds two methods inherited-methods-classification and ~
    local-methods-classification which produce indexes to the methods ~
    available in this class."))



(defmethod get-local-methods-classification ((self indexed-object))
       (classify-local-methods (class-of self)))


(defmethod get-inherited-methods-classification ((self indexed-object))
       (classify-inherited-methods (class-of self)
              'quail-object))
