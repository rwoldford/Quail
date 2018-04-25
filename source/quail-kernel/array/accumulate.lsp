;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               accumulate.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1989, 1990.
;;;     M.E. Lewis 1991.
;;;     R.W. Oldford 1991.
;;;
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(accumulate)))

;--------------------------------------------------------------------------------

(defgeneric accumulate (x margin fun &key init)
  (:documentation 
   "Like Common Lisp reduce function.  Example: if x is an object ~
    of dimensions (2 3 4 5 6), ~
    then (accumulate x '(t nil t nil t) #'foo) returns a (3 5) ~
    dimensional array.  The (i j)th element of the new array ~
    is the result of ~
    accumulating the elements of a (ref x '(t i t j t)) pairwise using the ~
    binary function foo.  ~
    If margin is T, then the function is applied to the entire array. ~
    If margin is shorter than the number of dimensions, it is appended with NILs."))


(defmethod-multi accumulate
  ((x (symbol number list array ref-array dimensioned-ref-object))
   margin fun 
   &key (init nil init-supplied?))
  
  (let* ((dim (dimensions-of x))
         (num-dim (length dim))
         (m (if (listp margin) (length margin) 0)))
    (if (> m num-dim)
      (quail-error "~&Margin ~S specifies ~S dimensions, but accumulated ~
                    object ~S has only ~S dimensions."
                   margin
                   m
                   x
                   num-dim))
    (setf margin (if (eq margin t)
                   (pad-list '() num-dim t)
                   (pad-list margin num-dim nil)
                   ))
    (let* ((margin-dim (list-if-t dim margin))
           (current (make-sequence 'list (length margin-dim) :initial-element 0))
           (d-start 0)
           (answer init))
      (if (not init-supplied?)
        (case (apply #'* margin-dim)
          (1 (let* ((indices (expand-list current margin t)))
               (setf answer (funcall fun (apply #'ref x indices)))
               (setf d-start 1)))
          (t (let* ((indices-1 (expand-list current margin t))
                    (next-current (row-major-next-subscript current margin-dim))
                    (indices-2 (expand-list next-current margin t)))
               (setf answer (funcall fun
                                     (apply #'ref x indices-1)
                                     (apply #'ref x indices-2)))
               (setf d-start 2)
               (setf current (row-major-next-subscript next-current margin-dim))))))
      (loop for d
            from d-start
            to (- (apply #'* margin-dim) 1)
            do (let* ((indices (expand-list current margin t)))
                 (setf answer (funcall fun answer (apply #'ref x indices)))
                 (setf current (row-major-next-subscript current margin-dim))))
      answer)))

