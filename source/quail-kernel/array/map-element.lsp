;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               map-element.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1991 Statistical Computing Laboratory, University Of Waterloo
;;;
;;;
;;;  Authors:
;;;     Michael Lewis 1991
;;;     Greg Anglin 1991
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(compute-order map-element)))



;;;--------------------------------------------------------------------------------
;;;
(defun compute-order (&rest args)
  "From the dimensions of the arguments supplied, this function tries to ~
   determine a sensible joint iteration order over the elements of the arguments. ~
   It returns either :row or :column to indicate row-major ordering in which ~
   the first index changes most slowly and column major ordering in which the ~
   last index varies most slowly.  ~
   If it cannot choose between these orderings it returns NIL.  ~
   (:see-also (map-lelement :function)) ~
   (:examples (:files eg:Arrays;iter-map.lisp))"
  (labels ((f (dimensions answer from-end?)
             (if (null dimensions) answer
                 (let ((a (find-if #'(lambda (b)
                                       (> b 1))
                                   (car dimensions)
                                   :from-end from-end?)))
                   (cond  ((null a)
                           (f (cdr dimensions) answer from-end?))
                          ((null answer)
                           (f (cdr dimensions) a from-end?))
                          ((= a answer)
                           (f (cdr dimensions) answer from-end?))
                          ((= answer 1)
                           (f (cdr dimensions) a from-end?))
                          (t nil))))))
    (let ((dimensions (mapcar #'dimensions-of args)))
      (cond  ((f dimensions 1 nil) 
              :column)
             ((f (reverse dimensions) nil t)
              :row)
             (t NIL)))))


(defun map-element (op order &rest args)
  "This function applies op elementwise to the arguments in args. ~
   If the number of elements differs, then some elements are reused. ~
   Ordinarily, returns a ref-array whose dimensions are that of the ~
   argument with the most elements. ~
   (:required ~
   (:arg op A function of as many arguments as there are objects ~
   in the args.) ~
   (:arg order Either :column or :row.  This is the major order in which the ~
   function will be applied over the elements of the args.  If NIL, an order ~
   will be determined from the dimensions of the args by using the function ~
   compute-order.) ~
   )~
   (:rest ~
   (:arg args Arbitrarily many refable objects whose elements are to ~
   to be combined with the function op. ~
   Each arg must be an instance of one of the dimensioned classes.))~
   (:see-also map-slices map *dimensioned-classes* compute-order)~
   (:examples (:files eg:Arrays;iter-map.lisp))
   "
  (declare (special *dimensioned-classes*))
  (labels ((dimensionable-object-p 
               (x)
             (some #'(lambda (y) (typep x y))
                   *dimensioned-classes*))
           (scalar-p 
               (x)
             (or (numberp x) (symbolp x) (characterp x)))
           (biggest 
               (&rest args)
             (flet ((f (a b)
                      (if (>= (number-of-elements a)
                              (number-of-elements b))
                        ;; a & b might both have size 1, in which case
                        ;; we prefer (potentially) dimensions == '(1)
                        ;; over dimensions == NIL.  If a is _bigger_ than
                        ;; b, it can't be a scalar, so a will be taken.
                        (if (scalar-p a) b a)
                        ;; if we're here, b is _bigger_, so can't be a scalar
                        b)))
               (reduce #'f args)))
           )
    (cond ((null args)
           (apply op nil))
          ((every #'scalar-p args)
           (apply op args))
          ((notevery #'dimensionable-object-p args)
           (quail-error "MAP-ELEMENT: Not all arguments are dimensionable."))
          (t
           (let ((result (apply #'make-dimensioned-result
                                (dimensions-of (apply #'biggest args))
                                args)))
             (ecase (or order (apply #'compute-order args) :unknown)
               (:column
                (dotimes (i (number-of-elements result))
                  (setf (column-major-eref result i)
                        (apply op
                               (mapcar #'(lambda
                                           (x)
                                           (column-major-eref x i))
                                       args)))))
               (:row
                (dotimes (i (number-of-elements result))
                  (setf (row-major-eref result i)
                        (apply op
                               (mapcar #'(lambda
                                           (x)
                                           (row-major-eref x i))
                                       args)))))
               (:unknown
                (quail-error "MAP-ELEMENT: ~
                              Argument order cannot be determined ~
                              for arguments of these dimensions: ~{~&~s~}."
                             (mapcar #'dimensions-of args)))
               (:col
                (dotimes (i (number-of-elements result))
                  (setf (column-major-eref result i)
                        (apply op
                               (mapcar #'(lambda
                                           (x)
                                           (column-major-eref x i))
                                       args))))))
             result)))))
