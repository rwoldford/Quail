;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               data-subsets.lsp
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
;;;     C.B. Hurley 1998 NUIM
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------
(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute)
(export  '(   data-subsets-fn subset-cases-fn
           subsets-by-var subsets-by-vars group-case-test order-levels select-data-subset select-case select-cases
           )))


(defgeneric group-case-test(var)
  (:documentation "Returns a function used to group values of the variable var."))

(defgeneric order-levels(var)
  (:documentation "Returns a function or a list which ~
                   specifies an ordering for the values of var."))


(defgeneric select-data-subset(dataset by &key name &allow-other-keys)
  (:documentation "Finds the cases in dataset using the information in by ~
                   and returns the subset of cases as a dataset."))

(defgeneric select-case(dataset  &key test)
  (:documentation "Finds a case in dataset using the information in test ~
                   and returns the case."))

(defgeneric select-cases(dataset  &key test)
  (:documentation "Finds  cases in dataset using the information in test ~
                   and returns the cases as a list."))

(defun data-subsets-fn (var &rest args)
  "Constructs a function which when applied ~
   to a dataset, groups the cases into datasets according to ~
   the values of variable(s) var."
  (cond ((variate-expr-p var)
         (apply #'subsets-by-var var args))
        ((and (listp var)
              (= (length var) 1)
              (variate-expr-p (car var)))
         (apply #'subsets-by-var (car var) args))
        (t (apply #'subsets-by-vars var args))
        ))



(let ((saved-groups nil))
  (defun subsets-by-var(var &key (test (group-case-test var))
                            level-function
                            (order-levels (order-levels var)))
    "Constructs a function which when applied ~
     to a dataset, groups the cases into datasets according to ~
     the values of variable var."
    (setq order-levels (or order-levels (order-levels var)))
    (flet ((key-test(a b)
             (and (equal (second a) (second b))
                  (eql (first a) (first b))
                  (eq-identifiers (third a) (third b))
                  (equal (fourth a) (fourth b))
                  (equal (fifth a) (fifth b))
                  )))
      #'(lambda(data &key cases)
          (if (functionp cases)
            (setq cases (funcall cases data)))
          (let* ((key (list (or cases data) var test order-levels level-function))
                 (ans (cdr (assoc key saved-groups :test #'key-test))))
            
            (unless ans
              (let* ((vars (list-variates data))
                     (lcases (or cases (list-cases data)))
                     (values (if lcases
                               (values-of lcases var :vars vars)))
                     (case-vals  (cond
                                  ((null level-function) values)
                                  ((and (listp level-function)
                                        (eql (car level-function) :equisize-bins)
                                        (integerp (second level-function)))
                                   (equisize-bins values (second level-function)
                                                  (third level-function)))
                                  ((and (listp level-function)
                                        (eql (car level-function) :equicount-bins)
                                        (integerp (second level-function)))
                                   (equicount-bins values (second level-function)
                                                   (third level-function)))
                                  (t (let ((fn (get-function level-function)))
                                       (mapcar fn values)))))
                     (group-vals (if (listp case-vals)
                                   (remove-duplicates case-vals :test test)
                                   nil))
                     result)   
                (when group-vals
                  
                  (setq group-vals 
                        (cond ((functionp order-levels)
                               (sort group-vals order-levels))
                              ((and (listp order-levels)
                                    (subsetp group-vals order-levels :test #'equal))
                               (loop with sorted-g = (make-list (length order-levels))
                                     for gi in group-vals
                                     for p = (position gi order-levels :test #'equal) do
                                     (setf (nth p sorted-g ) gi)
                                     finally (return sorted-g)))
                              ((some #'numberp group-vals)
                               (sort group-vals #'ltnn))
                              (t group-vals)))
                  
                  (setq result (loop with groups = (make-list (length group-vals) )
                                     for c in lcases
                                     for cval in case-vals
                                     for p = (position  cval group-vals :test test)
                                     when p
                                     do (push c (elt groups p))
                                     finally (return groups)) )
                  (setq ans (list (loop for r in result 
                                        for g in group-vals
                                        for gname = (if (numberp g)
                                                      (format nil "~A-~A" var g)
                                                      g
                                                      ;;  (format nil "~A"  g)
                                                      )
                                        collect (make-data-subset data (nreverse r) :name gname 
                                                                  :identifier (if (identifier-p g) g 
                                                                                  (format nil "~A"  g))))
                                  group-vals))
                  (setq saved-groups 
                        (push (cons key ans) saved-groups)))))
            (values-list (list (car ans) (cdr ans))))))))



(defun list-cross-product(&rest args)
  (labels ((list-cross-product1(a other-lists)
             (cond ((null other-lists)
                    a)
                   (t (let* ((b (car other-lists))
                             (ab 
                              (loop for ai in a
                                    nconc
                                    (loop for bi in b
                                          collect 
                                          (append ai (list bi))))))
                        (list-cross-product1 ab (cdr other-lists)))))))
    (cond ((null (cdr args))
           (mapcar #'list (car args)))
          (t (list-cross-product1 (mapcar #'list (car args)) (cdr args))))))

(defun list-to-identifier(x)
  (cond ((= 1 (length x))
         (if (identifier-p (car x)) (car x)
             (format nil  "~A" (car x))))
        (t (loop for xi in x collect (if (identifier-p xi) xi
             (format nil  "~A" xi))))))

(let ((saved-groups nil))
  (defun subsets-by-vars(var &key (test (mapcar #'group-case-test var))
                             level-function
                             order-levels)
    "Constructs a function which when applied ~
     to a dataset, groups the cases into datasets according to ~
     the values of variables var."
    (setq order-levels (or order-levels (mapcar #'order-levels var)))
    (flet ((key-test(a b)
             (and (eql (first a) (first b))
                  (equal (second a) (second b))
                  (= (length (third a)) (length (third b)))
                  (every #'eq-identifiers (third a) (third b))
                  (every #'equal (fourth a) (fourth b))
                  (every #'equal (fifth  a) (fifth b))
                  )))
      
      #'(lambda(data &key cases)
          (if (functionp cases)
            (setq cases (funcall cases data)))
          (let* ((key (list (or cases data) test var order-levels level-function))
                 (ans (cdr (assoc key saved-groups :test #'key-test))))
            
            (unless ans
              
              (let* ((vars (list-variates data))
                     (lcases (or cases (list-cases data)))
                     (values (loop for v in var collect  (values-of lcases  v :vars vars)))
                     (case-vals  (if (not (and (listp level-function)
                                               (= (length level-function) (length var))))
                                   
                                   values
                                   (loop for f in level-function
                                         for vals in values
                                         collect
                                         (cond
                                          ((null f) vals)
                                          ((and (listp f)
                                                (eql (car f) :equisize-bins)
                                                (integerp (second f)))
                                           (equisize-bins vals (second f) (third f) ))
                                          ((and (listp f)
                                                (eql (car f) :equicount-bins)
                                                (integerp (second f)))
                                           (equicount-bins vals (second f) (third f) ))
                                          (t (let ((fn (get-function f)))
                                               (mapcar fn vals)))))))
                     (group-vals 
                      (mapcar #'(lambda(l tt) (remove-duplicates l :test tt))
                              case-vals test))
                     result group-vals1 ) 
                
                (setq group-vals 
                      (loop for g in group-vals
                            for o in order-levels
                            collect (cond ((functionp o)
                                           (sort g o))
                                          ((and (listp o)
                                                (subsetp g o :test #'equal))
                                           (loop with sorted-g = (make-list (length o))
                                                 for gi in g
                                                 for p = (position gi o :test #'equal) do
                                                 (setf (nth p sorted-g ) gi)
                                                 finally (return sorted-g)))
                                          ((some #'numberp g)
                                           (sort g #'ltnn))
                                          (t g))))
                
                
                (setq group-vals1 (apply #'list-cross-product group-vals)) 
                (setq case-vals (apply #'mapcar #'list case-vals))
                
                (setq result (loop with groups = (make-list (length group-vals1) )
                                   for c in lcases
                                   for cval in case-vals
                                   for p = (position  cval group-vals1 :test 
                                                      #'(lambda(x y)
                                                          (loop for xi in x
                                                                for yi in y
                                                                for ti in test
                                                                always (funcall ti xi yi))))
                                   when p
                                   do (push c (elt groups p))
                                   finally (return groups)))
                (setq ans
                      (cons (loop for r in result
                                  for g in group-vals1
                                   for gname = (reduce #'(lambda(x y)
                                                          (format nil "~A ~A" x y)) 
                                                      (loop for gi in g 
                                                            for vi in var
                                                            collect
                                                            (if (numberp gi)
                                                              (format nil "~A-~A" vi gi)
                                                              gi)))
                                  collect (make-data-subset data (nreverse r) :name gname :identifier (list-to-identifier g)))
                            group-vals)))
              (setq saved-groups 
                    (push  (cons key ans) saved-groups)))
            (values-list (list (car ans) (cdr ans))) )))))




(defmethod group-case-test ((var t))
  #'(lambda (a b) (or (eql a b)
                      (and (numberp a)
                           (numberp b)
                           (= a b))
                      (and (stringp a)
                           (stringp b)
                           (string-equal a b))
                      (and a b (listp a) (listp b) (equal a b)))))

(defmethod order-levels ((var t))
  nil)

(defun data-subsets-fn (var &rest args)
  "Constructs a function which when applied ~
   to a dataset, groups the cases into datasets according to ~
   the values of variable(s) var."
  (cond ((variate-expr-p var)
         (apply #'subsets-by-var var args))
        ((and (listp var)
              (= (length var) 1)
              (variate-expr-p (car var)))
         (apply #'subsets-by-var (car var) args))
        (t (apply #'subsets-by-vars var args))
        ))


(defun subset-cases-fn(var &rest args)
  (let ((f (apply #'data-subsets-fn var args)))
    #'(lambda(d)
        (mapcar #'list-cases (funcall f d)))))




(defun ltnn(a b)
  (cond ( (and (numberp a) (numberp b))
          (< a b))
        ((numberp a) t)
        (t nil)))

(let ((saved-subsets nil))
  (defmethod select-data-subset((dataset t) (f function) &key name)
    (flet ((key-test(a b) 
             (and (eq (first a) (first b))
                  (eq (second a) (second b)))))
      (let* ((key (list dataset f))
             (ans (cdr (assoc key saved-subsets :test #'key-test))))
        (unless ans
          
          (let ((cases (loop for c in (list-cases dataset)
                             when (funcall f c)
                             collect c)))
            (setq ans
                  (make-data-subset dataset cases :name name))
            (setq saved-subsets 
                  (push  (cons key ans) saved-subsets))))
        ans))))






    
         
(let ((saved-subsets nil))
  (defmethod select-data-subset((dataset t) (bylist list) &key name)
    (flet ((key-test(a b) 
             (and (eq (first a) (first b))
                  (tree-equal (second a) (second b) :test #'(lambda(a b) (or (eql a b)
                                                                             (eq-identifiers a b)))))))
      (let* ((key (list dataset bylist))
             (ans (cdr (assoc key saved-subsets :test #'key-test))))
        (unless ans
          (setq ans
                (cond 
                 
                 ((listp (car bylist))
                  (let* ((vars (list-variates dataset))
                         (preds 
                          (loop 
                            for p in bylist collect
                            (let (test var value)
                              (if (= (length p) 3)
                                (setq test (first p) var (second p) value (third p))
                                (setq test #'equal var (first p) value (second p))) 
                              #'(lambda(c) (funcall test (value-of c var :vars vars) value))))))
                    (if (null name)
                      (setq name 
                            (format nil "~{~@(~A~) ~}" 
                            (loop 
                                  for p in bylist collect
                                  (if (= (length p) 3)
                                    (format nil "(~A ~A ~A)" (function-string (first p)) (second p) (third p))
                                    (format nil "~A = ~A" (first p) (second p) ))))))

                    (loop for c in (list-cases dataset)
                          when (and (loop for p in preds
                                          always (funcall p c)) c) 
                          collect c)))
                 (bylist
                  (let ((vars (list-variates dataset))
                        pred test var value)
                    (if (= (length bylist) 3)
                      (setq test (first bylist) var (second bylist) value (third bylist))
                      (setq test #'equal var (first bylist) value (second bylist)))
                    (setq pred #'(lambda(c) (funcall test (value-of c var :vars vars) value)))
                    (if (null name)
                      (setq name 
                           (if (= (length bylist) 3)
                                    (format nil "(~A ~A ~A)" (function-string (first bylist)) (second bylist) (third bylist))
                                    (format nil "~A = ~A" (first bylist) (second bylist) ))))
                    (loop 
                      for c in (list-cases dataset)
                      when (and (funcall pred c) c) 
                      collect c)))
                 (t nil)))
              (when ans
                (setq ans 
                    (make-data-subset 
                     dataset ans 
                     :name name))
              (setq saved-subsets 
                    (push  (cons key ans) saved-subsets))))
          ans))))







(defmethod select-case((dataset t) &key test)
  (cond ((null test)
         (car (list-cases dataset)))
        ((functionp test)
         (loop 
           for c in (list-cases dataset)
           thereis (and (funcall test c) c)))
        ((and (listp test) (listp (car test)))
         (let* ((vars (list-variates dataset))
                (preds 
                 (loop 
                   for p in test collect
                   (let (test1 var value)
                     (if (= (length p) 3)
                       (setq test1 (first p) var (second p) value (third p))
                       (setq test1 #'equal var (first p) value (second p))) 
                     #'(lambda(c) (funcall test1 (value-of c var :vars vars) value))))))
           (loop for c in (list-cases dataset)
                 thereis (and (loop for p in preds
                                    always (funcall p c)) c))))
        ((listp test) (let ((vars (list-variates dataset))
                 pred test1 var value)
             (if (= (length test) 3)
               (setq test1 (first test) var (second test) value (third test))
               (setq test1 #'equal var (first test) value (second test)))
             (setq pred #'(lambda(c) (funcall test1 (value-of c var :vars vars) value)))
             (loop 
               for c in (list-cases dataset)
               thereis (and (funcall pred c) c))))
        (t (car (list-cases dataset)))))



(defmethod select-cases((dataset t) &key test)
  (cond ((null test)
         (list-cases dataset))
        ((functionp test)
         (loop 
           for c in (list-cases dataset)
           when (funcall test c) collect c))
        ((and (listp test) (listp (car test)))
         (let* ((vars (list-variates dataset))
                (preds 
                 (loop 
                   for p in test collect
                   (let (test1 var value)
                     (if (= (length p) 3)
                       (setq test1 (first p) var (second p) value (third p))
                       (setq test1 #'equal var (first p) value (second p))) 
                     #'(lambda(c) (funcall test1 (value-of c var :vars vars) value))))))
           (loop for c in (list-cases dataset)
                 when (loop for p in preds
                                    always (funcall p c))
                 collect c)))
        ((listp test) (let ((vars (list-variates dataset))
                 pred test1 var value)
             (if (= (length test) 3)
               (setq test1 (first test) var (second test) value (third test))
               (setq test1 #'equal var (first test) value (second test)))
             (setq pred #'(lambda(c) (funcall test1 (value-of c var :vars vars) value)))
             (loop 
               for c in (list-cases dataset)
              when (funcall pred c)
               collect c)))
        (t (list-cases dataset))))
            

