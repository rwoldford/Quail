;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               linked-object.lisp                               
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(linked-object analysis-links-of back-analysis-links-of
          causal-links-of back-causal-links-of )))

(defclass linked-object ()
  ((analysis-links
    :initform nil
    :accessor analysis-links-of
    :documentation "Forward pointer list for the analysis links.")
   (back-analysis-links
    :initform nil
    :accessor back-analysis-links-of
    :documentation "Backward pointer list for the analysis links.")
   (causal-links
    :initform nil
    :reader causal-links-of
    :documentation "Forward pointer list for the causal links.")
   (back-causal-links
    :initform nil
    :reader back-causal-links-of
    :documentation "Backward pointer list for the causal links.")
   (back-data-links
    :initform nil
    :reader back-data-links-of
    :documentation "Backward pointer list for Data links -- Not implemented yet.")
   (list-of-path-elements
    :initform nil
    :accessor list-of-path-elements-of
    :documentation "The path-elements associated with this object because the ~
                    object has been created by this path-element or the path ~
                    element has been created by this object."))
  (:documentation
   "An object having several kinds of links."))

(defmethod remove-back-analysis-link ((self linked-object) linkee)
  "Removes Linkee from Back Analysis links of self."
  
  (setf (slot-value self 'back-analysis-links)
        (remove linkee (slot-value self 'back-analysis-links))))


(defmethod remove-forward-analysis-link ((self linked-object) linkee)
  "Removes Linkee from  Analysis links of self."
  
  (setf (slot-value self 'analysis-links)
        (remove linkee (slot-value self 'analysis-links))))



(defmethod analysis-unlink ((self linked-object) linkee)
  "Break an existing analysis link between two linked-objects both directions."
  (remove-forward-analysis-link self linkee)
  (remove-back-analysis-link self linkee)
  (remove-forward-analysis-link linkee self)
  (remove-back-analysis-link linkee self))



(defmethod analysis-link ((self linked-object) linkee)
  "Establish an Analysis Link between two linked-objects."
  
  (add-forward-analysis-link self linkee)
  (add-back-analysis-link linkee self)
  linkee)



(defmethod causal-link ((self linked-object)  linkee)
  "Establish a Causal Link between two linked-Objects."
  (add-forward-causal-link self linkee)
  (add-back-causal-link linkee self)
  linkee)



(defmethod add-forward-analysis-link ((self linked-object) linkee)
  "Adds linkee to Analysis-Links of self."
  (if (not (member linkee (slot-value self 'analysis-links)
                   :test
                   #'eq))
    (push linkee (slot-value self 'analysis-links))))



(defmethod add-forward-causal-link ((self linked-object) linkee)
  "Adds linkee to Causal-Links of self."
  (if (not (member linkee (slot-value self 'causal-links)
                   :test
                   #'eq))
    (push linkee (slot-value self 'causal-links))))



(defmethod add-back-analysis-link ((self linked-object) linkee)
  "Adds linkee to Back-Analysis-Links of self."
  
  (if (not (member linkee (slot-value self 'back-analysis-links)
                   :test
                   #'eq))
    (push linkee (slot-value self 'back-analysis-links))))



(defmethod add-back-causal-link ((self linked-object) linkee)
  "Adds linkee to Back-Causal-Links of self."
  
  (if (not (member linkee (slot-value self 'back-causal-links)
                   :test
                   #'eq))
    (push linkee (slot-value self 'back-causal-links))))


