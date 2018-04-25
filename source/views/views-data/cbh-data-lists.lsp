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
;;;  If the zeroth sublist is a list of identifiers, it is taken to be the variable
;;;  names.
;;;  If the zeroth element of each "case" is an identifier, it is taken to be the
;;;  case label.
;;;
;;;

(defmethod make-dataset-from-vars ((var list) &rest other-vars)
  (apply #'mapcar #'list var other-vars))

(defmethod dataset-p ((d list))
  (and (not (null d)) (not (list-of-identifiers-p (cdr d)))))

#|
;;; FLORENCE
(defmethod dataset-p ((d list))
  (not (null d)))
|#

(defmethod eq-dataset ((d1 list) (d2 list))
  (or (eq d1 d2) 
      (and (= (length d1) (length d2))
           (every #'eq-dataset d1 d2))))



(defmethod dataset-intersection ((d1 list) (d2 list))
  (cond ((member d1 d2 :test #'eq-dataset)
         d1)
        ((member d2 d1 :test #'eq-dataset)
         d2)
        ((intersection d1 d2 :test #'eq-dataset))))
#|
(defmethod list-cases ((d list))
  (if (listp (car d))    ;; d could be single case
    (if (list-of-identifiers-p (car d))
      (cdr d)
      d)
    (list d)))

;;; FLORENCE
(defmethod list-cases ((d list))
  (if (dataset-p (car d)) 
    d
    (list d)))
|#





(defmethod list-cases ((d list)) ;;CHANGE oct 93 ch
  (cond ((dataset-p (car d))
         d)
        ((list-of-identifiers-p (car d))
         (cdr d))
        (t (list d))))

(defmethod extract-case ((d list) (index integer))
  (elt (list-cases d) index))

#| FLORENCE
(defmethod list-variates ((d list))
  (if (listp (car d))
        (loop for i from 0  below (length (car d))
              collect i)))
|#

(defmethod list-variates ((d list))
  (let ((case-names? (and (listp (car d))
                          (every #'(lambda (e) (identifier-p (car e))) d)))
        (var-names? (list-of-identifiers-p (car d))))
    (if var-names?
      (car d)
      (if (listp (car d))
        (loop for i from 0  below (- (length (car d)) (if case-names? 1 0))
              collect i)))))



#| FLORENCE

(defmethod value-from-index ((c list) (v number) &key (default v))
  (if (or (not (integerp v)) (>= v (length c)))
      (if (eq default :error)
          (quail-error "~A has no value for ~A" c v)
          default)
      (elt c v)))
|#

(defmethod value-from-index ((c list) (v number) &key (default v))
  (let ((case-vals (if (identifier-p (car c)) (cdr c) c)))
    (if (or (not (integerp v)) (>= v (length case-vals)))
      (if (eq default :error)
          (quail-error "~A has no value for ~A" c v)
          default)
      (elt case-vals v))))

;;;  FLORENCE ... the following method is removed.
(defmethod identifier-of ((c list))
  (if (identifier-p (car c)) (car c) ))


