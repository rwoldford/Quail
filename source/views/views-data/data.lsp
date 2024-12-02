;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               data.lisp
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
(eval-when (:compile-toplevel :load-toplevel :execute)
(export  '(*datasets* choose-batches group-case-fn data-subsets-fn subset-cases-fn
           subsets-by-var subsets-by-vars group-case-test order-levels
           dataset-p list-of-datasets-p has-variate-p eq-dataset
             make-dataset-from-vars
           inspect-data dataset-name identifier-name list-cases 
           extract-case  values-of 
           list-variates value-of identifier-of eq-identifiers identifier-p
           list-of-identifiers-p list-identifiers  eq-variates
           variate-expr-p eq-variate-exprs variate-string-of
           make-dataset-from-vars make-data-subset select-data-subset
           choose-dataset choose-variable choose-some-variables select-value *safe-value-of?*
           eq-values-fn contains-data-p parent-dataset-of contains-cases-p select-case-value)))
;;;----------------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute) (proclaim '(inline dataset-p eq-dataset dataset-name )))
;;
;; The following methods are expected by views to be defined for data.


(defgeneric make-dataset-from-vars (var &rest other-vars)
  (:documentation "Makes a dataset from the vars"))

(defgeneric make-data-subset(dataset  case-list &key &allow-other-keys)
  (:documentation "Constructs a subset of dataset using case-list."))

#+:sbcl-linux(proclaim '(sb-ext:maybe-inline dataset-p)) ;14NOV2024
(defgeneric dataset-p (d)
  (:documentation "Test whether d is a viewable dataset."))

(defgeneric eq-dataset (d1 d2)
  (:documentation "Test whether d1 and d2 are both viewable datasets ~
                   and are eq.")
  )


(defgeneric has-variate-p (dataset var &key)
  (:documentation "Tests whether the dataset has a variate var."))



(defgeneric inspect-data (d)
  (:documentation "Inspects the dataset d."))

#+:sbcl-linux(proclaim '(sb-ext:maybe-inline dataset-name)) ;14NOV2024
(defgeneric dataset-name (d)
  (:documentation "Returns the name of d if it has one")
  )

(defgeneric identifier-name (d)
  (:documentation "Returns the identifier of d as a string if it has one")
  )







(defgeneric list-cases (d)
  (:documentation "Returns the 'cases' of a dataset ~
                   in a list.")
  )



(defgeneric extract-case (d index)
  (:documentation "Returns the 'case'  of a dataset ~
                   indexed by index.")
  )

(defgeneric list-variates (d)
  (:documentation "Return the variates of a dataset ~
                   in a list.")
  )

(defgeneric extract-variate (d index)
  (:documentation "Returns the 'variate'  of a dataset ~
                   indexed by index.")
  )

(defgeneric list-identifiers (d)
  (:documentation "Return the case identifiers of a dataset ~
                   in a list.")
  )
;;----------------------------------------------------------------------------------------------------

(defgeneric value-of (c var &key default &allow-other-keys)
  (:documentation "Returns the value of the case c corresponding to the ~
                   variate var.  Keywords  may be used to determine the ~
                   value. If no such value is determined, then the value of ~
                   default is returned.  If the value of default is :error then ~
                   an error will result.")
  )

#| 28SEP2023 email from rwo model no longer uses (c ..) cases, but (d ..) datasets
(defgeneric values-of (c variates &key default &allow-other-keys)
  (:documentation "List the values of c for each variate of variates. ~
                   If default is :error an error will be signalled if ~
                   there is no such value, otherwise default ~
                   (when supplied) is returned."))

|#
(defgeneric values-of (d var &key default &allow-other-keys)
  (:documentation "Returns the values in order of  list cases~
                   of the dataset c corresponding to the ~
                   variate var.  Keywords  may be used to determine the ~
                   value. If no such value is determined, then the value of ~
                   default is returned.  If the value of default is :error then ~
                   an error will result."))

(defgeneric select-value (d var  &key default test &allow-other-keys)
  (:documentation "Return a value  from a case in satisfying test.~
                    Keywords  may be used to determine the ~
                   value. If no such value is determined, then the value of ~
                   default is returned.  If the value of default is :error then ~
                   an error will result."))

;;----------------------------------------------------------------------------------------------------

(defgeneric identifier-of (c)
  (:documentation "Returns the identifer of the case c if it has one.")
  )

(defun list-of-identifiers-p(ids)
  "Tests whether the given argument is a list of identifiers."
  (and ids (listp ids) (every #'identifier-p ids)))

(defgeneric identifier-p (i)
  (:documentation "Test whether i is an identifier.")
  )

(defgeneric eq-identifiers (i1 i2)
  (:documentation "Test whether i1 and i2 are both identifiers ~
                   and are eq.")
  )
(defgeneric variate-string-of (v)
  (:documentation "Returns a string representation of variate v."))


(defgeneric variate-expr-p (v)
  (:documentation "Test whether v is variate expression-~
                   a legal secong argument to value-of.")
  )

(defgeneric eq-variate-exprs (v1 v2)
  (:documentation "Test whether v1 and v2 are both variate expressions-~
                   and are eq.")
  )
;; defined at line 153
;(defgeneric eq-identifiers (i1 i2)
;  (:documentation "Test whether i1 and i2 are both identifiers ~
;                   and are eq.")
;  )

(defgeneric eq-variates (v1 v2)
  (:documentation "Test whether v1 and v2 are both variates ~
                   and are eq.")
  )



;;----------------------------------------------------------------------------------------------------

(defgeneric contains-data-p(a b &key order?)
  (:documentation "Returns true if one dataset contains or is the same as the other"))

(defgeneric parent-dataset-of(a)
  (:documentation "Returns t the parent dataset of A, if it exists."))

(defgeneric parent-dataset-p(a b)
  (:documentation "Returns true if a is a parent dataset of b."))

(defgeneric ancestor-data-p(a b)
  (:documentation "Returns true if a is an ancestor dataset of b."))


(defgeneric contains-cases-p  (a b &key order?)
  (:documentation "Returns true if the cases of either a or b are a subset of the cases of the other."))
;;----------------------------------------------------------------------------------------------------


(defun list-of-datasets-p (arg)
  "Tests whether the given argument is a list of datasets."
  (and arg (listp arg) (every #'dataset-p arg)))


(defun eq-values-fn(&rest facs) 
  #'(lambda(a b)
      (or (eq-dataset a b)
          (loop for f in facs
                always (eql (value-of a f  :default :a)
                            (value-of b f  :default :b)))
          
          )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Methods for class T
;;;
;;;
;;; added listp May 5 98 cbh
(defmethod identifier-p ((i T))
  (if i (or (stringp i) (symbolp i) (listp i))))



(defmethod make-dataset-from-vars (var &rest other-vars)
  (declare (ignore var other-vars))
  NIL)


(defmethod make-data-subset(d  case-list &key)
  (declare (ignore d  case-list))
  NIL)


(defmethod make-data-subset :around (d  case-list &key)
  (if (and case-list (eql case-list (list-cases d)))
    d
    (call-next-method)))



(defmethod dataset-p (d)
  (declare (ignore d))
  NIL)








(defmethod variate-expr-p (v)
  (if (listp v)
    (let ((a (car v)) (b (cdr v)))
      (and (or (and (symbolp a) (fboundp a)) (functionp a))
           b))
    t))


(defmethod eq-variate-exprs (v1 v2)
  (equal v1 v2)
  )

(defmethod eq-variates (v1 v2)
  (equal v1 v2)
  )

(defmethod has-variate-p (dataset var &key vars)
  (or (member var vars :test #'eq-variates)
      (member var (list-variates dataset)
              :test #'eq-variates)))

(defmethod eq-identifiers (a b)
 (and a b (equal a b)))

(defmethod eq-dataset (d1 d2)
  (and (dataset-p d1)
       (dataset-p d2)
       (eq d1 d2)))









(defmethod inspect-data (d)
  (inspect d))







(defmethod identifier-name (d) 
  (let ((id (identifier-of d)))
    (if id
      (if (listp id) (format nil "~{~A~^ ~}"  id) 
          (princ-to-string id)))))

;; identifier name and dataset name can be different. In this case dataset name should be a longer
;; name suitable for plot titles while identifier name is shorter and used for identifying
;; items within plots.

(defmethod dataset-name (d) 
  (identifier-name d))

(defmethod list-cases (d)
  (declare (ignore d))
  nil)


(defmethod list-cases :around (d)
  (or (call-next-method)
  (if (dataset-p d)
    (list d)
    nil)))

(defmethod list-variates (d)
  (declare (ignore d))
  nil)

(defmethod list-variates :around (d)
   (or (call-next-method)
  (if (dataset-p d)
    (let ((c (car (list-cases d))))
      (unless (eq c d)
        (list-variates c)
        )))))
  



(defmethod extract-case((d T) (index t))
  nil)

(defmethod extract-case :around((d T) index)
  (or (call-next-method)
      (if (identifier-p index)
        (find  index (list-cases d) :test #'eq-identifiers :key #'identifier-of))))


(defmethod extract-variate((d T) (index t))
  nil)

(defmethod extract-variate :around((d T) index)
  (or (call-next-method)
      (cond ((numberp index)
             (nth index (list-variates d)))
            (t
             (find  index (list-variates d) :test #'equal )))))


(defmethod variate-string-of (v)
  (princ-to-string v))


(defmethod variate-string-of ((v fixnum))
  (format nil "var ~D" v))

(defmethod list-identifiers (d)
  (loop for v in (list-cases d) 
        for i upfrom 0 collect (or (identifier-of v) (format nil "Case ~S" i)))
  )
;;-----------------------------------------------------------------------------------------------------
(defmethod value-of (d var &key (default :error))
  (if (eq default :error)
    (quail-error "~A has no value for ~A" d var)
    default))


(defvar *safe-value-of?* T)


(defmethod value-of :around (d var &rest key-pairs 
                               &key vars (default :error) (safe? *safe-value-of?*))
  (if (listp var)
    (loop for v in var collect (apply #'value-of d v key-pairs))
  (if safe?
    (let (result index)
      (setq result 
        (cond
         ((numberp var) (call-next-method))
         ((member var (list-variates d) :test #'eq-variates)
          (call-next-method))
         ((setq index (position var vars :test #'eq-variates))
          (value-of d index :default default))
         ((functionp var) (funcall var d))
         (t default)))
        (if (eq result :error)
        (quail-error "~&~%Variate ~s not found in  ~s ." var d)
        result)
      )
    (call-next-method)
    )
  ))

#|
(defmethod values-of (c var &key vars (default :error) (safe? *safe-value-of?*))
  ;; list the values of c for each element of var
  (if supplied-p
    (loop for v in var-list collect (value-of c v :vars vars :default default))
    (loop for v in var-list collect (value-of c v :vars vars ))))

|#


(defmethod values-of (d var &rest key-pairs )
  (let ((result (loop for c in (list-cases d) collect (apply #'value-of c var key-pairs))))
        (if (member :error result)
          (quail-error "~&~%Variate ~s not found in one or more of the cases. ~%~
                        Dataset := ~s ~%~
                        List of case results are~%~
                        ~10T(~{  ~s  ~})." var d result)
          result)
        ))


(defmethod select-value (d var  &rest key-pairs  &key test (default :error)  )
  (let ((case (select-case d :test test)))
    (if case
      (apply #'value-of case var :default default key-pairs)
      (if (eq default :error)
        (quail-error "~&~%No case satisfying ~s found in  ~s ." test d)
        default))))

(defmethod select-values (d var  &rest key-pairs  &key test (default :error)  )
  (let ((cases (select-cases d :test test)))
    (if cases
      (apply #'values-of cases var :default default key-pairs)
      (if (eq default :error)
        (quail-error "~&~%No case satisfying ~s found in  ~s ." test d)
        default))))


(defmethod identifier-of ((c T))
  NIL)





;;-----------------------------------------------------------------------------------------------------

        


                
;;; commented out 11SEP2023 as per rwo email
;(defmethod contains-data-p  (a b &key (order? nil))
;  (declare (ignorable order? a b))
;  NIL)

(defmethod contains-data-p (a b &key (order? nil))
  (and (dataset-p a) (dataset-p b)
       (or (eq-dataset a b)
           (if order? (ancestor-data-p a b)
               (or (ancestor-data-p a b)
                   (ancestor-data-p b a)))
           (contains-cases-p a b :order? order?))))

(defmethod parent-dataset-of  (a)
  (declare (ignore  a))
  NIL)

(defmethod parent-dataset-p(a b)
  (and a b
       (eq a (parent-dataset-of b))))


(defmethod ancestor-data-p(a b)
  (and a b
       (or (parent-dataset-p a b)
           (ancestor-data-p a (parent-dataset-of b)))))

#|
(defmethod contains-data-p :around (a b &key (order? nil))
  (or (eq-dataset a b)
      (call-next-method)
      (contains-cases-p a b :order? order?)
      ))
|#


(defmethod contains-cases-p  (a b &key (order? nil))
  (if (and (dataset-p a) (dataset-p b))
    (or (eq-dataset a b)
        (let ((alist (list-cases a))
              (blist (list-cases b)))
          (if order?
            (subsetp blist alist :test #'eq-dataset)
            (or (subsetp alist blist :test #'eq-dataset)
                (subsetp blist alist :test #'eq-dataset)))))))




(defvar *datasets* nil "A list of named datasets.")


