;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               dataset.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is part of the Views system, a toolkit for interactive statistical graphics.
;;;  
;;;
;;;  Authors:
;;;     C.B. Hurley 1988-1991 George Washington University
;;;     R.W. Oldford 1988-1991
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------

(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(dataset list-identifiers list-variates dataset-name *datasets*)))

;; Function version below (cbh 7/94) because I needed a local method
;; for make-data-subset. Since make-data-subset uses dataset recursively,
;; dataset could not be a macro.
;; I also added a keyword arg cases to dataset, so that the cases returned
;; by list-cases on the subset are eq to the appropriate cases returned
;; by list-cases on the entire dataset.

(defun dataset (data &key (identifiers NIL) (variates NIL) (name NIL) (identifier NIL) (cases NIL) (save? T)
                     (case-class 'simple-case-object))
  "This function provides a uniform facility for identifying a given data ~
   structure as a dataset. ~
   (:required ~
   (:arg data The object to be regarded as a dataset.  Usually an array, ~
   or possibly a list.)) ~
   (:key ~
   (:arg identifiers NIL A list of strings to be used to identify the cases ~
   in the dataset.) ~
   (:arg variates NIL A list of strings to be used to identify the variates ~
   in the dataset.) ~
   (:arg cases NIL A list of simple case objects to be used for the dataset.) ~
   (:arg name NIL A string used to name the dataset.)
   (:arg save? T  A non nil value causes a named dataset to be added to *datasets*.)) ~
   (:see-also list-identifiers list-variates list-cases dataset-name *datasets*)"
  
  (when data
  (unless (dataset-p data)
    (defmethod dataset-p ((d (eql data)))
      T))
  (setq identifiers (or identifiers 
                        (loop for c in cases
                              collect (identifier-of c))
                        (list-identifiers data)))
  (setq variates (or variates (list-variates data)))
  (when (and variates (not (eq variates (list-variates data))))
    (defmethod list-variates ((d (eql data)))
      variates)
    )
  (if (and (null cases) identifiers)
    (setq cases
          (loop 
             for lab in identifiers
            as x in (list-cases data)
            collect (make-instance case-class
                      :identifier (or lab
                                      (identifier-of x))
                       :parent-dataset data
                      :case-vars variates
                      :case-data x))))
   
  (when cases
    (defmethod list-cases ((d (eql data)))
      cases))
  
  
  
  (defmethod make-data-subset ((d (eql data)) case-list &key name identifier save? &allow-other-keys)
    (if case-list
      (let* ((case-data (if (typep (car case-list) 'simple-case-object)
                          (mapcar #'case-data-of case-list)
                          case-list))
             (subset (or (call-next-method data case-data :name name) case-list))
             (ans  (dataset 
                    subset
                    :variates variates
                    :save? save?
                    :name name :identifier identifier
                    :cases case-list)))
        (defmethod parent-dataset-of((b (eql ans)))
          d)
        ans)))
             
             
  
  (if (stringp name)
    (defmethod dataset-name ((d (eql data)))
      name))
     (setq identifier (or identifier name))
     (if identifier
       (defmethod identifier-of ((d (eql data)))
         identifier))
    (if save? (push data *datasets*))
   
  data))
