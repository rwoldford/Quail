;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               prompt-data.lisp
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
;;;     C.B. Hurley 1992 George Washington University
;;;   
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(make-dataset-from-vars
          prompt-for-dataset dataset-menu-items  variable-menu-items
          choose-dataset choose-batches choose-variable choose-some-variables
          get-dataset-init get-data-inits get-batch-inits
          get-data-inits-1 get-data-inits-2 get-data-inits-3 get-barchart-inits
          get-data-inits-xy get-data-inits-2lists get-data-inits-3lists
          )))




(defun prompt-for-dataset (&optional (msg "Enter dataset" ))
  (let ((d (wb::prompt-user :type t :prompt-string msg :read-type :eval)))
    (if (dataset-p d) d nil)))




(defun dataset-menu-items ()
  (if *datasets*
    (append
     (list (list "A new one?" nil))
     (loop with name with dset
           for d in *datasets* do
           (if (and (consp d) (stringp (car d)))
             (setq name (car d) dset (cdr d))
             (setq name (dataset-name d) dset d))
           when name
           collect (list name dset)))))


(defun variable-menu-items (dataset)
  (loop for v in (list-variates dataset) 
        for v-str = (variate-string-of v)
                collect
                (list v-str v)))



(defun choose-dataset (&optional 
                       (msg NIL))
  (declare (special *current-dataset* *saved-selections*))
  (let ((menu-items (append (dataset-menu-items) *saved-selections*))
        dataset)
    (when menu-items
      
      (setq dataset
            (cadar
             (wb:prompt-for-items menu-items
                                  :prompt-text
                                  (if msg msg "Choose dataset")
                                  :item-function #'first))))
    
    (unless dataset
      (setq dataset (prompt-for-dataset
                     (if msg msg "Enter dataset"))))
    (if (dataset-p dataset)
      (setq *current-dataset* dataset))
    dataset))
      

(defun choose-batches (&optional dataset 
                                 (msg "Choose one or more variables to form batches"))
  "Choose batches from dataset"
  (choose-some-variables dataset 1 msg  ))


(defun choose-variable (&optional dataset (n 1) msg menu-items)
  "Choose n variables from dataset"
  (setq msg (or msg
                (if (= n 1) "Choose a variable"
                    (format nil "Choose ~A  variables" n))))
  ( setq menu-items (or menu-items (if dataset (variable-menu-items dataset))))
    (if menu-items
      (if (= n 1)
        (loop for res = (cadar (wb:prompt-for-items menu-items :prompt-text msg
                             :item-function #'first))
              until res
              finally (return res))
        (loop for result = (reverse (wb:prompt-for-items menu-items :prompt-text msg
                                                :item-function #'first
                                                :selection-type :disjoint))
              when result append result into result-list and
           do (setq msg (format nil "Choose ~A  more" (- n (length result-list))))
              until (>= (length result-list) n )
              finally (return (subseq  (mapcar #'second result-list) 0 n))))
      (loop for i from 1 to n  collect
            (wb::prompt-user :type t :read-type :eval :prompt-string msg))))


(defun choose-some-variables (&optional dataset (n 1) msg menu-items)
  "Choose n or more variables from dataset"
  (setq msg (or msg
                (format nil "Choose ~A or more variables" n)))
  
  (setq menu-items (or menu-items (if dataset (variable-menu-items dataset))))
    (if menu-items
      (loop for res =  (reverse (wb:prompt-for-items menu-items :prompt-text msg
                                              :item-function #'first
                                              :selection-type :disjoint))
             when res append res into result and
             do (setq msg (format nil "Choose ~A  or more" (- n (length result))))
            
            until (>= (length result) n)
            finally (return (mapcar #'second result)))
      
      (loop with v = t 
            until (and (>= (length var) n) (null v))
            do
            (setq v (wb::prompt-user :type t :read-type :eval :prompt-string msg))
            when v collect v into var
            finally (return var))))



     


(defun get-dataset-init (&key 
                         (data :prompt) 
                         &allow-other-keys)
  "Keyword argument DATA should be a dataset if provided.~
   If DATA is :prompt (the default),~
   the user is prompted to provide a dataset."
  
  (if (eq data :prompt)
    (list :data (choose-dataset) )))


(defun get-data-inits (&key nvars (min-nvars 1)
                            (data :prompt) (vars :prompt)
                            &allow-other-keys)
  "Keyword argument DATA should be a dataset and keyword ~
   argument VARS a list of indices into the dataset.~
   If both DATA and VARS are :prompt (the default),~
   the user is prompted to provide a dataset.~
   If VARS is :prompt and  keyword argument NVARS is integer ~
   (default nil) the user is prompted for NVARS variables.~
   If VARS is :prompt and  NVARS is non-integer the user is prompted ~
   for at least MIN-NVARS variables (default 1).~
   If no dataset has been specified, the make-dataset-from-vars ~
   function is used to build a dataset from VARS, and VARS becomes ~
   a list of indices into the dataset obtained by applying the ~
   list-variates function to the dataset. "
  
  ;;(unless vars (setq vars :prompt))
  (when (or (eq data :prompt) (null data) (eq vars :prompt))
    (if (and (or (eq data :prompt) (null data)) (eq vars :prompt))
      (setq data (choose-dataset)))
    
    (when (eq vars :prompt) 
      (setq vars 
            (if (integerp nvars) (choose-variable data nvars)
                (choose-some-variables data min-nvars))))
    
    (when (not (dataset-p  data))
      (setq data (apply #'make-dataset-from-vars vars))
      (if (dataset-p  data)
        (setq vars (list-variates data))))
    
    (list :data data :vars vars)))



(defun get-data-inits-1 (&key (data :prompt) (var :prompt) vars
                               &allow-other-keys)
  "Keyword argument DATA should be a dataset and ~
   keyword argument VAR an index into the dataset.~
   If both DATA and VAR are :prompt, the user is prompted to provide a dataset.~
   If VAR is :prompt (the default) the user is prompted for a variable.~
   If no dataset has been specified, the make-dataset-from-vars ~
   function is used to build a dataset from VAR, and VAR becomes ~
   the first index into the dataset obtained by applying the ~
   list-variates function to the dataset."
  
  
  (when (or (eq data :prompt) (null data) (eq var :prompt))
    (if (and vars (eq var :prompt))
      (setq var (or (first vars) :prompt)))
    (if (and (or (eq data :prompt) (null data)) (eq var :prompt))
      (setq data (choose-dataset)))
    
    (if (eq var :prompt) 
      (setq var (choose-variable data 1 )))
    
    (when (not (dataset-p  data))
      (setq data (make-dataset-from-vars var))
      (if (dataset-p data)
        (setq var (car (list-variates data)))))
    
    (list :data data :var var)))
         


(defun get-data-inits-2 (&key  (data :prompt) vars
                               (x :prompt) (y :prompt)
                               &allow-other-keys)
  "Keyword argument DATA should be a dataset and X and Y indices into the dataset.~
   If DATA and either X or Y are :prompt (the default),~
   the user is prompted to provide a dataset.~
   If X or Y is :prompt the user is prompted for variables.~
   If no dataset has been specified, the make-dataset-from-vars function ~
   is used to build a dataset from X and Y, and X and Y becomes the first ~
   and second indices into the dataset obtained by applying the ~
   list-variates function to the dataset."
  
  (when (or (eq data :prompt) (null data) (eq x :prompt) (eq y :prompt))
    (when vars
      (if (eq x :prompt) (setq x (or (car vars) :prompt)))
      (if (eq y :prompt) (setq y (or (cadr vars) :prompt))))
    
    (if (and (or (eq data :prompt) (null data))
             (or (eq x :prompt) (eq y :prompt)))
      (setq data (choose-dataset)))
    
    (cond
     ((and (eq x :prompt) (eq y :prompt))
      (multiple-value-setq (x y)
        (values-list (choose-variable data 2 "Choose two"))))
     ((eq x :prompt) 
      (setq x (choose-variable data 1 "Choose X")))
     ((eq y :prompt) 
      (setq y
            (choose-variable data  1 "Choose Y")))
     )
     
    (when (not (dataset-p  data))
      (setq data (make-dataset-from-vars x y))
      (if (dataset-p data)
      (multiple-value-setq (x y) (values-list  (list-variates data)))))
    
    (list :data data :x x :y y)))




(defun get-data-inits-3 (&key (data :prompt)
                              (x :prompt) (y :prompt) (z :prompt) vars
                              &allow-other-keys)
  "Keyword argument DATA should be a dataset and X,Y and Z indices into the dataset.~
   If DATA and either X,Y or X are :prompt (the default),~
   the user is prompted to provide a dataset.~
   If X is :prompt the user is prompted for an X variable.~
   If Y is :prompt the user is prompted for a Y variable.~
   If Z is :prompt the user is prompted for a Z variable.~
   If no dataset has been specified, the make-dataset-from-vars function ~
   is used to build a dataset from X,Y and Z, and X,Y and Z becomes the first ~
   second and third indices into the dataset obtained by applying the ~
   list-variates function to the dataset."
  
  ;;(unless x (setq x :prompt))
  ;;(unless y (setq y :prompt))
  ;;(unless z (setq z :prompt))
  
  (when (or (eq data :prompt) (null data)
            (eq x :prompt) (eq y :prompt) (eq z :prompt))
    (when vars
     (if (eq x :prompt) (setq x (or (car vars) :prompt)))
      (if (eq y :prompt) (setq y (or (cadr vars) :prompt)))
      (if (eq z :prompt) (setq z (or (caddr vars) :prompt))))
    (if (and (or (eq data :prompt) (null data))
             (or (eq x :prompt) (eq y :prompt) (eq z :prompt)))
      (setq data (choose-dataset)))

    (if (or (eq x :prompt) (eq y :prompt) (eq z :prompt))
      (multiple-value-setq (x y z)
        (values-list (choose-variable data 3 "Choose 3"))))
    
    (when (not (dataset-p  data))
      (setq data (make-dataset-from-vars x y z))
      (if (dataset-p data)
        (multiple-value-setq (x y z) (values-list  (list-variates data)))))
    
    (list :data data :x x :y y :z z)))


(defun get-barchart-inits (&key (data :prompt) (by :prompt) batches
                             &allow-other-keys)
  "Keyword argument DATA should be a dataset and ~
   keyword argument by is used to form batches from  DATA,~
   using the function get-batches.
   If by is :prompt the user is prompted for a value. ~
   If in addition DATA  is :prompt or nil, the user is prompted for a value. "
  
  
  (if batches
    (list :data (if (eq data :prompt) nil data)
          :by nil :batches batches)
    (when (eq by :prompt)
      (if (or (eq data :prompt) (null data))
        (setq data (choose-dataset)))
      
      (setq by (choose-some-variables  data 1 "Choose one or more by variables" 
                                       (cons  (list "Bar per case" #'list-cases) (variable-menu-items data))))
      
      (if (member #'list-cases by) (setq batches #'list-cases by nil))
      (if (eq 1 (length by))
        (setq by (car by)))
      (list :data (if (eq data :prompt) nil data) :by by :batches batches))))


(defun get-batch-inits (&key (data :prompt) (by :prompt) batches
                             &allow-other-keys)
  "Keyword argument DATA should be a dataset and ~
   keyword argument by is used to form batches from  DATA,~
   using the function get-batches.
   If by is :prompt the user is prompted for a value. ~
   If in addition DATA  is :prompt or nil, the user is prompted for a value. "
  
  
  (if batches
    (list :data (if (eq data :prompt) nil data)
          :by nil :batches batches)
    (when (eq by :prompt)
      (if (or (eq data :prompt) (null data))
        (setq data (choose-dataset)))
      
      (setq by (choose-some-variables data 1 "Choose one or more by variables" ))
      (if (eq 1 (length by))
        (setq by (car by)))
      
      (list :data (if (eq data :prompt) nil data) :by by :batches batches))))

(defun get-data-inits-xy (&key vars
                            (data :prompt) (x-vars :prompt) (y-vars :prompt)
                            &allow-other-keys)
  "Keyword argument DATA should be a dataset and keyword ~
   arguments X-vars and Y-vars  lists of indices into the dataset.~
   If  DATA and X-vars or Y-vars are :prompt (the default),~
   the user is prompted to provide a dataset.~
   If X-vars is :prompt the user is prompted for  X variables.~
   If Y-vars is :prompt the user is prompted for  Y variables.~
   If no dataset has been specified, the make-dataset-from-vars ~
   function is used to build a dataset from X-vars and Y-vars, ~
   and X-vars and Y-vars become ~
   lists of indices into the dataset. "
  
   (when (or (eq data :prompt) (null data)
            (eq x-vars :prompt) (eq y-vars :prompt))
     (when vars
       (if (eq x-vars :prompt)
         (setq x-vars (or (first vars) :prompt)))
       (if (eq y-vars :prompt)
         (setq y-vars (or (second vars) :prompt))))
    
    (if (and (or (eq data :prompt) (null data))
             (or (eq x-vars :prompt) (eq y-vars :prompt)))
      (setq data (choose-dataset)))

    (when (eq x-vars :prompt) 
      (setq x-vars 
            (choose-some-variables data 1 "Choose one or more X variables")))
    (when (eq y-vars :prompt) 
      (setq y-vars 
            (choose-some-variables data 1 "Choose one or more Y variables")))
    
    (when (not (dataset-p  data))
      (setq data (apply #'make-dataset-from-vars (append x-vars y-vars)))
      (when (dataset-p data)
        (setq x-vars (subseq   (list-variates data) 0 (length x-vars)))
        (setq y-vars (subseq   (list-variates data) (length x-vars) ))))
    
    (list :data data :x-vars x-vars :y-vars y-vars)))


(defun get-data-inits-2lists (&key (min-nvars 2)
                            (data :prompt) (x :prompt) (y :prompt)
                            &allow-other-keys)
  "Keyword argument DATA should be a dataset and keyword ~
   arguments x and y  lists of indices into the dataset.~
   If  DATA and x or y are :prompt (the default),~
   the user is prompted to provide a dataset.~
   If x is :prompt the user is prompted for  X variables.~
   If y is :prompt the user is prompted for  Y variables.~
   If no dataset has been specified, the make-dataset-from-vars ~
   function is used to build a dataset from x and y, ~
   and x and y become ~
   lists of indices into the dataset.~
   x and y should be the same length, at least min-nvars"
  
   (when (or (eq data :prompt) (null data)
            (eq x :prompt) (eq y :prompt))
    
    (if (and (or (eq data :prompt) (null data))
             (or (eq x :prompt) (eq y :prompt)))
      (setq data (choose-dataset)))

    (when (eq x :prompt) 
      (setq x 
            (choose-some-variables
             data min-nvars
             (format nil "Choose ~S or more X variables" min-nvars))))
    (when (eq y :prompt) 
      (setq y 
            (if (and (listp x) (>= (length x) min-nvars))
              (choose-variable
               data (length x)
               (format nil "Choose ~S Y variables" (length x)))
              (choose-some-variables   
               data min-nvars
               (format nil "Choose ~S or more Y variables" min-nvars))
            )))
    
    (when (not (dataset-p  data))
      (setq data (apply #'make-dataset-from-vars (append x y)))
      (when (dataset-p data)
        (setq x (subseq   (list-variates data) 0 (length x)))
        (setq y (subseq   (list-variates data) (length x) ))))
    
    (list :data data :x x :y y)))


(defun get-data-inits-3lists (&key (min-nvars 2)
                            (data :prompt) (x :prompt) (y :prompt) (z :prompt)
                            &allow-other-keys)
  "Keyword argument DATA should be a dataset and keyword ~
   arguments x y and z  lists of indices into the dataset.~
   If  DATA and x or y or z are :prompt (the default),~
   the user is prompted to provide a dataset.~
   If x is :prompt the user is prompted for  X variables.~
   If y is :prompt the user is prompted for  Y variables.~
   If z is :prompt the user is prompted for  Z variables.~
   If no dataset has been specified, the make-dataset-from-vars ~
   function is used to build a dataset from x y and z, ~
   and x y and z become ~
   lists of indices into the dataset.~
   x y and z should be the same length, at least min-nvars"
  
   (when (or (eq data :prompt) (null data)
            (eq x :prompt) (eq y :prompt) (eq z :prompt))
    
    (if (and (or (eq data :prompt) (null data))
             (or (eq x :prompt) (eq y :prompt)))
      (setq data (choose-dataset)))

    (when (eq x :prompt) 
      (setq x 
            (choose-some-variables
             data min-nvars
             (format nil "Choose ~S or more X variables" min-nvars))))
    (when (eq y :prompt) 
      (setq y 
            (if (and (listp x) (>= (length x) min-nvars))
              (choose-variable
               data (length x)
               (format nil "Choose ~S Y variables" (length x)))
            (choose-some-variables   
               data min-nvars
               (format nil "Choose ~S or more Y variables" min-nvars)))))
    (when (eq z :prompt) 
      (setq z 
            (cond ((and (listp x) (>= (length x) min-nvars))
                   (choose-variable
                    data (length x)
                    (format nil "Choose ~S Z variables" (length x))))
                  ((and (listp y) (>= (length y) min-nvars))
                   (choose-variable
                    data (length y)
                    (format nil "Choose ~S Z variables" (length y))))
            (t (choose-some-variables   
               data min-nvars
               (format nil "Choose ~S or more Z variables" min-nvars))))))

    
    (when (not (dataset-p  data))
      (setq data (apply #'make-dataset-from-vars (append x y z)))
      (when (dataset-p data)
        (setq x (subseq   (list-variates data) 0 (length x)))
        (setq y (subseq   (list-variates data) (length x) (length y)))
        (setq z (subseq   (list-variates data) (+ (length x) (length y))))))
    
    (list :data data :x x :y y :z z)))



