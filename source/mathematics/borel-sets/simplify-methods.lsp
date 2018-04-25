;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               simplify-methods.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;
;;;
;;;  Authors:
;;;     N. Wiebe 1998
;;;
;;;
;;;----------------------------------------------------------------------------------
;;;
;;;                             SIMPLIFY METHODS
;;;
;;;      Method                 |    Argument type      |
;;;                             |_______________________
;;;      Name                   |      Argument         |
;;;  ___________________________|_______________________|
;;;                             |                       |
;;;                             |                       |
;;;  simplify-object            | explicit-finite-union |
;;;                             | complement-set        |
;;;                             |                       |
(in-package :quail)

(defun touch-p (set1 set2)
  (let ((min1 (infimum set1))
        (max1 (supremum set1))
        (min2 (infimum set2))
        (max2 (supremum set2)))
    (or (and (= min1 max2)
             (or (and (right-open-p set2)
                      (left-closed-p set1))
                 (and (right-closed-p set2)
                      (left-open-p set1))))
        (and (= min2 max1)
             (or (and (right-open-p set1)
                      (left-closed-p set2))
                 (and (right-closed-p set1)
                      (left-open-p set2)))))))
                      

(defgeneric simplify-object (set)
  (:documentation
   "Reduce set-object to its simplest form."))

(defmethod simplify-object ((set explicit-finite-union))
  (labels ((simplify-now (set1 &rest other-sets)
             (let ((set2 (first other-sets))
                   (rest-of-them (rest other-sets))
                   temp collection)            
               (cond  (rest-of-them
                       (if (op-disjoint-sets-p set1 set2)
                         
                         (setf collection (concatenate 'list collection (list set1)
                                                       (apply #'simplify-now 
                                                              (concatenate 'list (list set2) rest-of-them))))
                         (progn (setf temp (op-set-union set1 set2))
                                (if (and (typep temp 'explicit-finite-union)
                                         (not (typep temp 'explicit-finite-set)))
                                  (setf collection (concatenate 'list collection (list (first (contents-of temp)))
                                                                (apply #'simplify-now 
                                                                       (concatenate 'list (rest (contents-of temp)) rest-of-them))))
                                  (setf collection (concatenate 'list collection
                                                                (apply #'simplify-now 
                                                                       (concatenate 'list (list temp) rest-of-them))))))))
                      (set2
                       (if (op-disjoint-sets-p set1 set2)
                         (setf collection (concatenate 'list collection (list set1 set2)))
                         (progn (setf temp (op-set-union set1 set2))
                                (if (and (typep temp 'explicit-finite-union)
                                         (not (typep temp 'explicit-finite-set)))
                                  (setf collection (concatenate 'list collection (list (first (contents-of temp)))
                                                                (apply #'simplify-now (rest (contents-of temp)))))
                                  (setf collection (concatenate 'list collection (list temp)))))))

                      (set1
                       (setf collection (concatenate 'list collection (list set1))))))))

  (if (not (sorted-collection-p set))
    (collection-sort set *default-union-sort*))
  (let ((contents (apply #'simplify-now (contents-of set)))
        new-set)
    (if (= (length contents) 1)
      (setf new-set (elt contents 0))
      (progn (setf new-set (make-instance 'explicit-finite-union :contents contents))
             (disjoint-collection-p new-set))) new-set)))

(defmethod simplify-object ((self explicit-finite-intersection))
  (apply #'set-intersection (contents-of self)))

#|
;;; should this be coerce instead?;; only for singletons, borel-sets, or explicit-finite-sets
(defmethod simplify-object ((self complement-set))
  (cond
   ((op-disjoint-sets-p (set-of self) (complement-wrt self))
    (complement-wrt self))
   ((and (extended-realp (set-of self))
        (typep 'interval (complement-wrt self)))
    self)))

;;not finished
|#

(defmethod simplify-object ((self interval))
  (if (and (= (infimum self) (supremum self))
           (eq (closure-property-of self) :closed))
    (infimum self) self))