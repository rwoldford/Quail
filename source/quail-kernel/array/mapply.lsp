;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               mapply.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1989, 1990.
;;;
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(mapply)))

;--------------------------------------------------------------------------------

;;;
;  DEFUN mapply
;

(defun mapply (fun &rest args)
  (multiple-value-bind (dro-and-margin-list collect-scheme shape-scheme)
                       (interpret args '((:collect :block) (:shape :neither)))
    (declare (ignore collect-scheme))
    (let* ((dro-list (mapcar #'first dro-and-margin-list))
           (dim-list (mapcar #'dimensions-of dro-list))
           (num-dim-list (mapcar #'length dim-list))
           (margin-list (get-margin-list dro-and-margin-list num-dim-list))
           (margin-dim (get-margin-dim margin-list dim-list shape-scheme))
           (current (make-sequence 'list (length margin-dim) :initial-element 0))
           (current-to-index-map (get-current-to-index-map margin-list
                                                           margin-dim
                                                           shape-scheme))
           (index-list (mapcar #'(lambda (n) (make-sequence 'list n :initial-element t))
                               num-dim-list))
           ref-r
           new-value)
      (setf index-list (map-current-to-index current
                                             index-list
                                             current-to-index-map))
      (setf new-value (apply fun (mapcar #'(lambda (x y)
                                             (apply #'ref x y))
                                         dro-list
                                         index-list)))
      (setf ref-r 
            (apply #'make-dimensioned-result
                   (append margin-dim
                           (remove 1 (dimensions-of new-value)))
                   (append dro-list
                           (list :class 'num-array))))    ;; Can this 
      (if (<= (length (dimensions-of ref-r)) 2)           ;;   be done
        (change-class ref-r 'matrix))                     ;;     better later ?
      (apply *setf-ref* new-value ref-r current)
      (setf current (row-major-next-subscript current margin-dim))
      (loop for d
            from 1                            ;; We did 0 already !!
            to (- (apply #'* margin-dim) 1)
            do (progn
                 (setf index-list (map-current-to-index current
                                                        index-list
                                                        current-to-index-map))
                 (setf new-value (apply fun (mapcar #'(lambda (x y)
                                                        (apply #'ref x y))
                                                    dro-list
                                                    index-list)))
                 (apply *setf-ref* new-value ref-r current)
                 (setf current (row-major-next-subscript current margin-dim))))
      ref-r)))

(defun get-margin-list (dro-and-margin-list num-dim-list)
  (loop for dro-and-margin in dro-and-margin-list
        as  num-dim in num-dim-list
        collect (let* ((margin (second dro-and-margin))
                       (margin (if (eq margin t)
                                 (pad-list '() num-dim t)
                                 (pad-list margin num-dim nil)))
                       (m (length margin)))
                  (if (> m num-dim)
                    (quail-error "~&Margin ~S specifies ~S dimensions, but ~
                            object ~S has only ~S dimensions."
                           margin
                           m
                           (first dro-and-margin)
                           num-dim))
                  margin)))

(defun get-margin-dim (margin-list dim-list shape-scheme)
  (declare (ignore shape-scheme))
  (let* ((margin-dim '()))
    (loop for margin in margin-list
          as  dim    in dim-list
          do (loop for m in margin
                   as  d in dim
                   do (if m
                        (setf margin-dim (append margin-dim (list d))))))
    margin-dim))

(defun get-current-to-index-map (margin-list margin-dim shape-scheme)
  (declare (ignore shape-scheme))
  (let* ((k 0)
         (current-to-index-map (make-array (list (length margin-dim) 2))))
    (loop for i from 0 to (- (length margin-list) 1)
          do (let ((margin (elt margin-list i)))
               (loop for j from 0 to (- (length margin) 1)
                     do (if (elt margin j)
                          (progn
                            (setf (aref current-to-index-map k 0) i)
                            (setf (aref current-to-index-map k 1) j)
                            (incf k))))))
    current-to-index-map))

(defun map-current-to-index (current index-list current-to-index-map)
  (loop for k from 0 to (- (length current) 1)
        do (setf (elt (elt index-list (aref current-to-index-map k 0))
                      (aref current-to-index-map k 1)) 
                 (elt current k)))
  index-list)
  


  
                       
                  
  
