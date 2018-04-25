;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               dataset-class.lisp
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




;;This file parallels dataset.lisp. However, it creates a new "dataset structure",
;;unlike dataset.lisp which makes a list or array act like a dataset.


(eval-when (:compile-toplevel :load-toplevel :execute)
(export '(dataset-class make-dataset mway-dataset-class make-mway-dataset dataset-slice)))






(defclass dataset-class()
  ((identifiers :initform NIL :initarg :identifiers :accessor list-identifiers)
   (variates :initform NIL :initarg :variates :accessor list-variates)
   (cases :initform NIL :initarg :cases :accessor cases-of)
   (identifier :initform NIL :initarg :name :accessor identifier-of )
   (parent-dataset :initform NIL :initarg :parent-dataset :accessor parent-dataset-of)
   ))



(defmethod ancestor-data-p((a dataset-class) (b dataset-class))
  (or (eq a (parent-dataset-of b))
      (ancestor-data-p a (parent-dataset-of b))))





(defmethod dataset-p ((self dataset-class))
  t)


(defmethod list-cases((self dataset-class))
  (cases-of self))


(defun make-dataset (data &key (class 'dataset-class) (identifiers NIL) 
                          (variates NIL) (name NIL) cases (save? T)
                          (case-class 'simple-case-object))
  (let (new)
    
    (setq identifiers (or identifiers 
                          (loop for c in cases
                                collect (identifier-of c))
                          (list-identifiers data)))
    (setq variates (or variates (list-variates data)))
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
    
    (setq new
          (make-instance class
            :identifiers identifiers
            :variates variates
            :name name
            :cases cases))
    (if (and name save?) (push new *datasets*))
    new))


(defmethod make-data-subset  ((d dataset-class) case-list &key name save?)
  (if case-list
    (let ((newd
           (make-dataset nil
                         :class (class-name (class-of d))
                         :save? save?
                         :name name
                         :variates (list-variates d)
                         :cases case-list)))
      (setf (parent-dataset-of newd) d)
      newd)))




   
;;;------------------------------------------------------------------------------------

;;This file parallels mway-dataset.lisp. However, it creates a new "dataset structure",
;;unlike dataset.lisp which makes a list or array act like a dataset.

(defclass mway-dataset-class(dataset-class)
  ((factors :initform NIL :initarg :factors :accessor factors-of)
   (factor-levels :initform NIL :initarg :factor-levels :accessor factor-levels-of)
   (datasets-by :initform NIL  :accessor datasets-by-of)
   (original-data :initform NIL :initarg :original-data  :accessor original-data-of)
  ))

(defmethod response-variates-of((self mway-dataset-class))
  (let ((f (factors-of self)))
    (loop for v in (list-variates self)
          unless (member v f :test #'equal)
          collect v)))





(defun make-mway-dataset (data  &key (class 'mway-dataset-class) 
                                (factors NIL) (variates NIL) (factor-levels NIL) 
                          (identifiers NIL) (name NIL) (cases NIL) (save? T)
                          (case-class 'mway-case-object ))
  (when data
  
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

  (let ((fac-tuples (all-tuples factor-levels))
        new)
   ;; (setq identifiers (or identifiers (sublist-to-string fac-tuples)))
     (setq identifiers (or identifiers  (mapcar #'list-to-identifier fac-tuples )))
                          
    
    (setq variates (append factors (or variates (list-variates data))))
      (setf cases (or cases
              (if (and (= ndata-dim 1) (= (car data-dim) (length fac-tuples)))
                (loop  
                     for lab in identifiers
                     as x in (qk::row-major-list-elements data)
                        as f in fac-tuples
                     collect (make-instance case-class
                               :identifier lab
                               :case-factors factors
                                :parent-dataset data
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
                               :case-data (qk::glue f y))))))
    (setq new
          (make-instance class
            :original-data data
            :identifiers identifiers
            :variates variates
            :name name
            :cases cases
            :factors factors
            :factor-levels factor-levels))
    (if (and name save?) (push new *datasets*))
    new))))
    
    
(defmethod make-data-subset  ((d mway-dataset-class) case-list &key name save?)
  (if case-list
    (let ((newd
           (make-dataset nil
                         :class 'dataset-class
                         :save? save?
                         :name name
                         :variates (list-variates d)
                         :cases case-list)))
      (setf (parent-dataset-of newd) d)
      newd)))


(defmethod dataset-transpose ((self mway-dataset-class) facs &key name)
  (if (and facs (not (listp facs)))
    (setq facs (list facs)))
  (let* ((factors (factors-of self))
         (ovars (response-variates-of self))
         (od (original-data-of self))
         (factor-levels (factor-levels-of self))
         (case-class (class-name (class-of (car (list-cases self)))))
         (data-dim (quail-kernel:dimensions-of od))
         (ndata-dim (length data-dim))

         p nfacs perm 
         new-fac-tuples new-identifiers v new-var-tuples new-var-names new-data
         ans)
    
    (loop for f in factors
          for i upfrom 0
          when (member f facs :test #'equal)
          collect i into index
          and collect f into new-facs
          else collect i into other-index
          finally (setq p index facs new-facs v other-index))
    (setq ans (cdr (assoc p (datasets-by-of self) :test #'equal))) 
    (unless ans
      
      
      (setq nfacs (length facs))
      (if (= nfacs (length factors))
        (setq ans self)
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
          (setq new-var-names (loop for r in new-var-tuples
                                    collect (if (listp r) (format nil "~{~A~^ ~}"  r)
                                                r)))
          
          
          
          (if (and (= ndata-dim 1) (= (car data-dim) (reduce #'* (mapcar #'length factor-levels))))
            (setq od (array od :dimensions (mapcar #'length factor-levels))))
                        
          (setq perm
                (loop for i from 0 below ndata-dim
                      when (member i p :test #'=)
                      collect i into a
                      else collect i into b
                      finally (return (append a b))))
          
          (setq new-data (qk::tp od :perm perm))
          
          (setq ans
                (make-mway-dataset  new-data 
                                    :factors facs
                                    :factor-levels (qk::sel factor-levels p :shape t)
                                    :identifiers new-identifiers
                                    :variates new-var-names
                                    :case-class (or case-class 'mway-case-object)
                                    :name (or name (format nil "~{~A~^ ~}" facs))))
          
          (setf (datasets-by-of self) (acons p ans (datasets-by-of self))))))
    ans))


(defmethod dataset-slice ((self mway-dataset-class) (bylist list) &key name (save? nil))
  
  
  
  (let (facs values )
    (loop for f in bylist by #'cddr
          for v in (cdr  bylist) by #'cddr
          collect f into fs
          collect v into vs
          finally (setq facs fs values vs))
    (setq name (or name (format nil "~A: ~{~A=~A~^ ~}" (dataset-name self) bylist)))
    
    
    (let* ((factors (factors-of self))
           (nf (length factors))
           (factor-levels (factor-levels-of self))
           (vars (response-variates-of self))
           (nfacs (length facs))
           index new-data  new-factors new-levels new-identifiers)
      
      (loop for f in factors
            for levels in factor-levels
            for i = (position f facs :test #'equal)
            when i
            collect (position (elt values i) levels :test #'equal) into tindex
            else collect t into tindex and
            collect f into tnew-factors
            and collect levels into tnew-levels
            finally (setq index tindex
                          new-levels tnew-levels
                          new-factors tnew-factors))
      
      (setq new-data (apply #'qk::ref  (original-data-of self) index))
      
     ;; (setq new-identifiers (loop for r in (all-tuples new-levels)
      ;;                            collect (if (listp r) (format nil "~{~A~^ ~}"  r)
      ;;                                        r)))
      (setq new-identifiers (all-tuples new-levels))
      
      
      (cond 
       ((>= nfacs (- nf 1))
        ;; an ordinary dataset
        (make-dataset new-data
                      :variates vars
                      :identifiers (car new-levels)
                      :name name
                      :save? save?))
      
      (t
       ;; an mway dataset where m = nf - nfacs
       
       (make-mway-dataset   new-data :factors
                            new-factors
                            :factor-levels new-levels
                            :variates vars
                            :identifiers new-identifiers
                            :name name :save? save?
                            ))))))