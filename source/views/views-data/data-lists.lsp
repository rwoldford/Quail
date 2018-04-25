;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               data-lists.lisp
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  DATASETS AS LISTS
;;;
;;;  Simple datasets, consisting of a list of lists, each sublist is a "case".
;;;  
;;;  Cases are also regarded as kinds of datasets. Should a list eg (a b c)
;;; be regarded as a dataset of a single case or a dataset of 3 cases?
;;; We resolve this by deciding that:
;;;    If the first object a is itself a dataset then (a b c) is a dataset of 3 cases.
;;;    Otherwise, (a b c) is a dataset of a single case.
;;;    
;;; Therefore (3 4 2) is treated as a dataset of a single case because 
;;; numbers themselves are not dataset-p.
;;; However, the array object created by (array '(3 4 2)) is treated differently,
;;; because the "convention" for arrays is that (array '(3 4 2)) should be treated
;;; as a 3 x 1 array, that is, a dataset of 3 cases.



(defmethod make-dataset-from-vars ((var list) &rest other-vars)
  (apply #'mapcar #'list var other-vars))

(defmethod make-data-subset((d list)  case-list &key)
  case-list) 


(defmethod dataset-p ((d list))
  (not (null d)))


(defmethod eq-dataset ((d1 list) (d2 list))
  (and d1 d2
       (or (eq d1 d2) 
      (and (= (length d1) (length d2))
           (every #'eq-dataset d1 d2)))))




(defmethod list-cases ((d list))
  ;; If d is already a list of datasets then d itself is returned.
  ;;  Otherwise d is interpreted as a dataset with a single case.
  
  (cond ((null d) nil)
         ((dataset-p (car d)) 
         d)
        (t  (list d))))

;; following changed from fixnum to integer by Greg Anglin 93 12
;; to support Allegro port

(defmethod extract-case ((d list) (index integer))
  (elt (list-cases d) index))





(defmethod list-variates ((d list))
  (cond ((null d)
         nil)
        ((dataset-p (car d))
         (list-variates (car d)))
        (t (loop for i from 0  below (length d)
          collect i))))




#|
(defmethod case-val ((c list) (v fixnum) &key (default :error))
  (if (>= v (length c))
      default
      (elt c v)))

|#


(defmethod value-of ((c list) (v fixnum) &key (default :error))
  (if (>= v (length c))
      default
      (elt c v)))