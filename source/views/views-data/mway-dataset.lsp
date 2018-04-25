;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               mway-dataset.lisp
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
;;;     C.B. Hurley 1996 
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------

(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute)
(export '(mway-dataset dataset-transpose mway-case-object contains-factor-test contains-factor-p eq-factor-levels)))

;; dataset the function acts as a wrapper for arrays and lists so they can still be treated as
;; arrays and lists. Do I want to do the same for mway-data, or construct a new structure?
;; the following is an attempt at the first method.


(defclass mway-case-object (simple-case-object)
  ((case-factors :initform NIL :initarg :case-factors :accessor case-factors-of))
  )


(defmethod contains-case-p((a mway-case-object) (b mway-case-object) &key (order? nil))
  (or (eq-dataset a b)
      (let ((ca (case-factors-of a))
            (cb (case-factors-of b)))
        (if order?
          (and (subsetp ca cb :test #'equal)
               (loop for f in ca
                     always (eql (value-of a f  :default :a :safe? nil)
                                 (value-of b f  :default :b :safe? nil))))
        (if (<= (length ca) (length cb))
          (and (subsetp ca cb :test #'equal)
               (loop for f in ca
                     always (eql (value-of a f  :default :a :safe? nil)
                                 (value-of b f  :default :b :safe? nil))))
          (and (subsetp cb ca :test #'equal)
               (loop for f in cb
                     always (eql (value-of a f  :default :a :safe? nil)
                                 (value-of b f  :default :b :safe? nil)))))))))

#|
(defmethod contains-data-p((a mway-case-object) (b mway-case-object) &key (order? nil))
  (or (eq-dataset a b)
      (contains-case-p a b :order? order?)))

(defmethod contains-case-p(a b &key order?)
  (declare (ignore order?))
  (eq-dataset a b))

(defmethod factor-values((self mway-case-object))
  (value-of self (case-factors-of self) :vars (list-variates self)))

|#




(defmethod eq-factor-levels(a b)
  (declare (ignore a b ))
  NIL)

(defmethod eq-factor-levels((a mway-case-object) (b mway-case-object))
  (or (eq a b)
      (let ((ca (case-factors-of a))
            (cb (case-factors-of b)))
        (and ca cb  (null (set-exclusive-or  ca cb :test #'equal ))
             (loop  for i from 0 below (length ca)
                     always (eql (value-of a i  )
                                 (value-of b i )))))))

(defmethod subsetp-factor-levels(a b &key factors (order? nil) )
  (declare (ignore factors a b order?))
  NIL)


(defmethod subsetp-factor-levels((a mway-case-object) (b mway-case-object) &key factors (order? nil))
  (let ((ca (case-factors-of a))
        (cb (case-factors-of b)))
    (unless (listp factors) (setq factors (list factors)))
    (cond (factors
            (and (subsetp factors ca :test #'equal)
                 (subsetp factors cb :test #'equal)
                 (loop for f in factors
                       always (eql (value-of a f)
                                   (value-of b f)))))
           (order?
            (and (subsetp ca cb :test #'equal)
                 (loop for f in ca
                       always (eql (value-of a f)
                                   (value-of b f)))))
           (t (or (and (subsetp ca cb :test #'equal)
                 (loop for f in ca
                       always (eql (value-of a f)
                                   (value-of b f))))
                  (and (subsetp cb ca :test #'equal)
                 (loop for f in cb
                       always (eql (value-of a f)
                                   (value-of b f)))))))))
            
    
(defun contains-factor-test(factors ) 
  #'(lambda(a b)
      (subsetp-factor-levels a b :factors factors)))

(defun sublist-to-string(tuples)
  (loop for r in tuples
        collect (if (listp r) (format nil "~{~A~^ ~}"  r) r)))




(defun mway-dataset (data &key (factors NIL) (variates NIL) (factor-levels NIL) variate
                          (identifiers NIL) (name NIL)  (identifier  NIL)(cases NIL) (save? T)
                          (case-class 'mway-case-object))
  "This function provides a uniform facility for identifying a given data ~
   structure as a multiway dataset. ~
   (:required ~
   (:arg data An array  to be regarded as a dataset.  )) ~
   (:key ~
   (:arg identifiers NIL A list of strings to be used to identify the cases ~
   in the dataset.) ~
   (:arg factors NIL A list of strings to be used to identify the factors ~
   in the dataset.) ~
   (:arg factor-levels NIL A list of strings to be used to identify the factors ~
   in the dataset.) ~
   (:arg cases NIL A list of simple case objects to be used for the dataset.) ~
   (:arg name NIL A string used to name the dataset.)
   (:arg save? T  A non nil value causes a named dataset to be added to *datasets*.)) ~
   (:see-also list-identifiers list-variates list-cases dataset-name *datasets*)"
  
  (when data
  (if variate (setq variates (list variate)))
  (let* ((data-dim (quail-kernel:dimensions-of data))
         (ndata-dim (length data-dim)))
  (setq factors
        (or factors (loop repeat ndata-dim  for code  from (char-code #\A)
            collect (format nil "~A" (code-char code)))))
  (setq factor-levels (if (and factor-levels (listp factor-levels) (not (every #'numberp factor-levels)))
                        factor-levels
                        (let ((numfacs (if (and factor-levels (listp factor-levels) (every #'numberp factor-levels))
                                         factor-levels
                                         data-dim)))
                          (loop for b in numfacs
                                collect (loop  for i from 1 to b
                                              collect i))
                         ;; (loop for b in numfacs
                          ;;      for f  in factors
                          ;;      collect (loop  for i from 1 to b
                           ;;                   collect (format nil "~A-~A" f i)))
                          )))
  (unless (or
           (and (= ndata-dim 1) (= (car data-dim) (reduce #'* (mapcar #'length factor-levels))))
           (and (>= ndata-dim (length factors))
                (every #'= (mapcar #'length factor-levels) data-dim))
           (quail-error "Cannot construct an mway dataset from ~S " data)))

           (unless (dataset-p data)
    (defmethod dataset-p ((d (eql data)))
      T))
  (let ((fac-tuples (all-tuples factor-levels))
        datasets-by
        (ovars variates ))
    
    (setq identifiers (or identifiers  (mapcar #'list-to-identifier fac-tuples )))

    ;;(setq identifiers (or identifiers (sublist-to-string fac-tuples)))
    (setq variates (or variates (list-variates data)))
    (unless (= (length variates) (/ (qk::array-total-size data) (length identifiers)))
      (setq variates 
            (loop for i from 0 below (/ (qk::array-total-size data) (length identifiers))
                  collect i)))
    (when variates
      (setq variates (append factors variates ))
      (defmethod list-variates ((d (eql data)))
        variates))
    
      
    (defmethod list-cases ((d (eql data)))
      (if cases
        cases
        (setf cases
              (if (and (= ndata-dim 1) (= (car data-dim) (length fac-tuples)))
                (loop  
                     for lab in identifiers
                     as x in (qk::row-major-list-elements data)
                        as f in fac-tuples
                     collect (make-instance case-class
                               :identifier lab
                               :case-factors factors
                               :case-vars variates
                               :case-data (qk::glue f x)))
              (loop  with slice = (qk::iseq 0 (1- (length factors)))
                     for lab in identifiers
                    ;; as x in (qk::row-major-list-elements data)
                     for i upfrom 0
                      as x = (qk::row-major-ref-slice data slice i )
                     as y = (array x :dimensions (qk::array-total-size x))
                  
                     as f in fac-tuples
                     collect (make-instance case-class
                               :identifier lab
                               :case-factors factors
                               :case-vars variates
                               :case-data (qk::glue f y)))))))
   (defmethod dataset-transpose ((d (eql data)) facs &key name)
      (if (and facs (not (listp facs)))
        (setq facs (list facs)))
       (let ( p nfacs
                 new-fac-tuples new-identifiers v new-var-tuples new-var-names new-data perm
                 ans)
         (when (subsetp facs factors :test #'equal)
           (loop for f in factors
                          for i upfrom 0
                          when (member f facs :test #'equal)
                          collect i into index
                          and collect f into new-facs
                          else collect i into other-index
                          finally (setq p index facs new-facs v other-index))
           (setq ans (cdr (assoc p datasets-by :test #'equal)))
           (unless ans
            (setq nfacs (length facs))
            (if (= nfacs (length factors))
              (setq ans d)
              (progn
                
                (setq new-fac-tuples (all-tuples (qk::sel factor-levels p)))
                    (setq new-identifiers (mapcar #'list-to-identifier new-fac-tuples ))

                (setq new-var-tuples 
                      (cond ((and  (= (length v) 1 ) (= (length ovars) 1))
                             (elt  factor-levels (car v)))
                            ((= (length ovars) 1)
                              (all-tuples (qk::sel factor-levels v)))
                            (t
                             (all-tuples (append (qk::sel factor-levels v :shape t) (list ovars) )))))
                (setq new-var-names (sublist-to-string new-var-tuples))
                (setq new-data
                      (if (and (= ndata-dim 1) (= (car data-dim) (length fac-tuples)))
                        (array d :dimensions (mapcar #'length factor-levels))
                        d))
                (setq perm
                      (loop for i from 0 below (length  (qk::dimensions-of new-data))
                            when (member i p :test #'=)
                            collect i into a
                            else collect i into b
                            finally (return (append a b))))
                (setq new-data (qk::tp new-data :perm perm))
               
                
                (setq ans
                      (mway-dataset   new-data
                                      :factors facs
                                      :factor-levels (qk::sel factor-levels p :shape t)
                                         :case-class case-class
                                :identifiers new-identifiers
                                :variates new-var-names
                                
                                :name (or name facs)))
                (setq datasets-by (acons p ans datasets-by)))))
        ans)))
  
  
  (defmethod make-data-subset ((d (eql data)) case-list &key name save? identifier &allow-other-keys)
    (if case-list
      (let* ((case-data (if (typep (car case-list) 'simple-case-object)
                          (mapcar #'case-data-of case-list)
                          case-list))
             (subset (call-next-method data case-data :name name)))
        
        (dataset 
         subset
         :variates variates
         :save? save?
         :name name :identifier identifier
         :cases case-list))))
  
  (if (stringp name)
    (defmethod dataset-name ((d (eql data)))
      name))
     (setq identifier (or identifier name))
     (if identifier
       (defmethod identifier-of ((d (eql data)))
         identifier))
    (if save? (push data *datasets*))
    )
  data)))






