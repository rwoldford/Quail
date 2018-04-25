;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               documented-object.lisp                               
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(documented-object long-summary-of reference-of short-summary-of
          glossary-of)))

(defclass documented-object ()
  ((long-summary
    :initform "No summary is available on this item" 
    :allocation :class
    :accessor long-summary-of)
   (reference :initform "No references are available for this item" 
              :allocation :class
              :accessor reference-of)
   (short-summary :initform "No summary is available on this item" 
                  :allocation :class
                  :accessor short-summary-of)
   (glossary :initform "No glossary is available for this item" 
             :allocation :class
             :accessor glossary-of))
  (:documentation
   "This mixin stores textual information for the given class of object."))



(defmethod references ((self documented-object)
                       object)
                                               ; Produce a list of references
                                               ; in the litterature for this
                                               ; kind of quail-object
       (quail-print-help (if (slot-exists-p object 'references)
                             (slot-value object 'references)
                             "description not available")))


(defmethod short-summary ((object documented-object))
                                               ; Produce a short summary of
                                               ; this kind of OBJECT
       (quail-print-help (if (slot-exists-p object 'short-summary)
                             (slot-value object 'short-summary)
                             "description not available")))



(defmethod long-summary ((object documented-object))
                                               ; Produce a long summary of
                                               ; this kind of OBJECT
       (quail-print-help (if (slot-exists-p object 'long-summary)
                             (slot-value object 'long-summary)
                             "description not available")))


(defmethod print-summary ((self documented-object))
                                               ; Produce a summary of
                                               ; this kind of quail-object
       (quail-print-help (if (slot-exists-p self 'glossary)
                             (slot-value self 'glossary)
                             "description not available")))
