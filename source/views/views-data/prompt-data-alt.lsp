;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               prompt-data-alt.lisp
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

#|
(defun choose-dataset (&optional 
                       (msg NIL))
  (declare (special *current-dataset*))
  (let ((menu-items (dataset-menu-items))
        dataset)
    (when menu-items
      
      (setq dataset
           (second (select-from-list menu-items
                                  :prompt-text
                                  (if msg msg "Choose dataset")
                                  :item-function #'first))))
     (unless dataset
      (setq dataset (prompt-for-dataset
                     (if msg msg "Enter dataset"))))
    (if (dataset-p dataset)
      (setq *current-dataset* dataset))
    dataset))


(defun choose-variable (&optional dataset (n 1) msg menu-items)
  "Choose n variables from dataset"
  (setq msg (or msg
                (if (= n 1) "Choose a variable"
                    (format nil "Choose ~A  variables" n))))
  ( setq menu-items (or menu-items (if dataset (variable-menu-items dataset))))
    (if menu-items
      (if (= n 1)
        (loop for res = (second (select-from-list menu-items :prompt-text msg
                             :item-function #'first))
              until res
              finally (return res))
        (loop for result = (select-from-list menu-items :prompt-text msg
                                                :item-function #'first
                                                :selection-type :multi)
              when result append result into result-list and
           do (setq msg (format nil "Choose ~A  more" (- n (length result-list))))
              until (>= (length result-list) n )
              finally (return (subseq  (mapcar #'second result-list) 0 n))))
      (loop for i from 1 to n  collect
            (wb::prompt-user :result-type t :read-type :eval :prompt-string msg))))

(defun choose-some-variables (&optional dataset (n 1) msg menu-items)
  "Choose n or more variables from dataset"
  (setq msg (or msg
                (format nil "Choose ~A or more variables" n)))
  
  (setq menu-items (or menu-items (if dataset (variable-menu-items dataset))))
    (if menu-items
      (loop for res =  (select-from-list menu-items :prompt-text msg
                                                :item-function #'first
                                                :selection-type :multi)
             when res append res into result and
             do (setq msg (format nil "Choose ~A  or more" (- n (length result))))
            
            until (>= (length result) n)
            finally (return (mapcar #'second result)))
      
      (loop with v = t 
            until (and (>= (length var) n) (null v))
            do
            (setq v (wb::prompt-user :result-type t :read-type :eval :prompt-string msg))
            when v collect v into var
            finally (return var))))
|#


(defun get-data-inits-2 (&key  (data :prompt)
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
    
    (if (and (or (eq data :prompt) (null data))
             (or (eq x :prompt) (eq y :prompt)))
      (setq data (choose-dataset)))
    
    (cond
     ((and (eq x :prompt) (eq y :prompt))
      (multiple-value-setq (x y)
        (values-list (select-xy-variables data :prompt-text "Choose X and Y"))))
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
                              (x :prompt) (y :prompt) (z :prompt)
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
    
    (if (and (or (eq data :prompt) (null data))
             (or (eq x :prompt) (eq y :prompt) (eq z :prompt)))
      (setq data (choose-dataset)))

    (if (or (eq x :prompt) (eq y :prompt) (eq z :prompt))
      (multiple-value-setq (x y z)
        (values-list (select-xyz-variables data :prompt-text "Choose X,Y,Z"))))
    
    (when (not (dataset-p  data))
      (setq data (make-dataset-from-vars x y z))
      (if (dataset-p data)
        (multiple-value-setq (x y z) (values-list  (list-variates data)))))
    
    (list :data data :x x :y y :z z)))




(defun get-data-inits-xy (&key 
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
    
    (if (and (or (eq data :prompt) (null data))
             (or (eq x-vars :prompt) (eq y-vars :prompt)))
      (setq data (choose-dataset)))

    (cond ((and (eq x-vars :prompt) (eq y-vars :prompt))
           (multiple-value-setq (x-vars y-vars)
             (values-list (select-xy-variables data :selection-type :multi))))
          
          ((eq x-vars :prompt) 
           (setq x-vars 
                 (choose-some-variables data 1 "Choose one or more X variables")))
          ((eq y-vars :prompt) 
           (setq y-vars 
                 (choose-some-variables data 1 "Choose one or more Y variables"))))
    
    (when (not (dataset-p  data))
      (setq data (apply #'make-dataset-from-vars (append x-vars y-vars)))
      (when (dataset-p data)
        (setq x-vars (subseq   (list-variates data) 0 (length x-vars)))
        (setq y-vars (subseq   (list-variates data) (length x-vars) ))))
    
    (list :data data :x-vars x-vars :y-vars y-vars)))