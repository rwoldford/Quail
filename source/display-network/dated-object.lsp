;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               dated-object.lisp                               
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(dated-object creation-time-of creator-of)))

(defclass dated-object ()
  ((created :initform (get-universal-time)
            :reader creation-time-of
            :documentation "Internal format time of creation of object.")
   (creator :initform (user-name)
            :reader creator-of
            :documentation "Username of creator of object.")
   )
  (:documentation
   "A useful mixin to record the date that the object was created and ~
    the name of the user who created it."))
