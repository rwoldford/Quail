;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               editable-object.lisp                               
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(editable-object notes-of)))


(defclass editable-object (dated-object initable-object)
  ((notes :initform nil
          ;; (CONCATENATE 'STRING "Object
          ;; " (OR (SLOT-VALUE SELF 'NAME)
          ;; "") " created by "
          ;; (SLOT-VALUE SELF 'CREATOR) "
          ;; " (IL:GDATE (SLOT-VALUE SELF
          ;; 'CREATED)) " using " (CAR
          ;; (SLOT-VALUE SELF
          ;; 'SPAWNING-EXPRESSION)))
          :initarg :notes
          :accessor notes-of
          :documentation "User added notes on this object."
          )
   )
  (:documentation 
   "Mixin class used to collect notes on an object."))



(defmethod read-notes ((self editable-object))
       

;;; 
;;; Read the notes recorded on this item
;;; 

       (if (null (slot-value self 'notes))
           (quail-print-help "Nothing recorded on this item yet")
           (edit-text (slot-value self 'notes)
                  :read-only t)))


(defmethod edit-notes ((self editable-object))
  "Invoke an editor to edit the notes on the Notes IV."
  (setf (notes-of self)
        (edit-text (notes-of self))))


(defmethod add-notes ((self editable-object))
  "Stash user supplied note onto the Notes IV."
  
  (setf (notes-of self)
        (edit-text (concatenate 'string (notes-of self)
                                (format "~&Edited by ~s. ~%~
                                         Time: ~s~%"
                                        (user-name)
                                        (get-universal-time))
                                )
                   )))
