;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                           slice.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     M.E. Lewis 1992.
;;;     R.W. Oldford 1992, 1994.
;;;
;;;
;;;----------------------------------------------------------------------------

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(doslices collect-slices)))


(defmacro doslices
          ((slice-var object
                      &optional
                      (slices :elements)
                      return-form
                      (order :row))
           &body body)
  "Provides straightforward iteration over selected slices of a refable object.  ~&~
   Syntax: (doslices (slice-var object {[slices] | [return-form] | [order]}) {form}*) ~
   (:elaboration ~
   First doslices evaluates the form object, which should produce a refable object. ~
   It then executes the body once for each slice in the object as determined by ~
   slices argument with slice-var being the value of the current slice.  ~
   The order of iteration is determined by the optional argument order.  ~
   Upon completion, slice-var is bound to NIL and the return-form is evaluated.) ~
   (:required ~
   (:arg slice-var The symbol to be bound to the current slice for use in the body ~
   of the form.) ~
   (:arg object The ref'able object whose slices are to be iterated over.) 
   )~
   (:optional ~~
   (:arg slices :elements ~
   The integer or list of fixed dimensions that identifies a slice.~
   For example if slices is 0 then a slice is defined to be the set of all ~
   elements of object having a common value of the zeroth index. ~
   There will be as many slices as the size of the zeroth dimension of object. ~
   Similarly, if slices is '(0 1) then a slice of object is the set of all elements ~
   of object having common zeroth and first index and there will be as many slices ~
   as there are pairs of zeroth and first indices.  ~
   If NIL then the whole ofobject is taken to be the slice. ~
   Slices may also be the keyword :elements in which case the elements ~
   are accessed directly as opposed to a ref of them.) ~
   (:arg return-form NIL The form to be evaluated and returned upon completion of ~
   the loop.) ~
   (:arg order :row The order in which the slices are to be iterated over. ~
   If :row, then iteration proceeds in row major order changing the index of ~
   last dimension defining the slice fastest.  If :column or :col, then iteration ~
   is in column major order and the index of the first dimension given in slices ~
   changes fastest.) ~
   )~
   (:body The body of the iteration. As many forms as you would like.)~
   (:see-also collect-slices loop do dolist row-major-ops column-major-ops)~
   "
  
  (let ((slices-var (gensym "SLICES-VAR-")))
    `(cond ((or (eql ,order :col) (eql ,order :column))
            (let ((,slices-var
                   (cond ((eq ,slices :elements) :elements)
                         ((numberp ,slices) (list ,slices))
                         ((null ,slices) NIL)
                         ((listp ,slices) 
                          (remove-duplicates (sort ,slices #'<)))
                         (t ,slices))))
              (loop for index from 0 to (- (number-of-slices ,object ,slices-var)
                                           1)
                    
                    do
                    (let ((,slice-var
                           (column-major-ref-slice ,object ,slices-var index)))
                      (declare (ignorable ,slice-var))
                      ,@body)
                    finally
                    (let* ((,slice-var NIL)
                           (result ,return-form))
                      (declare (ignorable ,slice-var))
                      (return result)))
              ))
           (t
            (let ((,slices-var
                   (cond ((eq ,slices :elements) :elements)
                         ((numberp ,slices) (list ,slices))
                         ((null ,slices) NIL)
                         ((listp ,slices) 
                          (remove-duplicates (sort ,slices #'<)))
                         (t ,slices))))
              (loop for index from 0 to (- (number-of-slices ,object ,slices-var)
                                           1)
                    
                    do
                    (let ((,slice-var
                           (row-major-ref-slice ,object ,slices-var index)))
                      (declare (ignorable ,slice-var))
                      ,@body)
                    finally
                    (let* ((,slice-var NIL)
                           (result ,return-form))
                      (declare (ignorable ,slice-var))
                      (return result))))))))


(defmacro collect-slices
          ((slice-var object
                      &optional (slices :elements) (order :row))
           &body body)
  "Provides straightforward iterations over selected slices of a refable object.  ~&~
   Syntax: (collect-slices (slice-var object {[slices] | [order]}) {form}*)  ~&~
   First collect-slices evaluates the form object, which should produce a refable object. ~
   It then executes the body once for each slice in the object as determined by ~
   the optional argument slices.  ~
   The order of iteration is determined by the optional argument ~
   order: either :row (the default) for row major order, or :col for column ~
   major order.  The result of each iteration is collected into a list and returned ~
   as the value of the collect-slices form.
   (:required ~
   (:arg slice-var The symbol to be bound to the current slice for use in the body ~
   of the form.) ~
   (:arg object The ref'able object whose slices are to be iterated over.) 
   )~
   (:optional ~~
   (:arg slices :elements ~
   The integer or list of fixed dimensions that identifies a slice.~
   For example if slices is 0 then a slice is defined to be the set of all ~
   elements of object having a common value of the zeroth index. ~
   There will be as many slices as the size of the zeroth dimension of object. ~
   Similarly, if slices is '(0 1) then a slice of object is the set of all elements ~
   of object having common zeroth and first index and there will be as many slices ~
   as there are pairs of zeroth and first indices.  ~
   If NIL then the whole ofobject is taken to be the slice. ~
   Slices may also be the keyword :elements in which case the elements ~
   are accessed directly as opposed to a ref of them.) ~
   (:arg order :row The order in which the slices are to be iterated over. ~
   If :row, then iteration proceeds in row major order changing the index of ~
   last dimension defining the slice fastest.  If :column or :col, then iteration ~
   is in column major order and the index of the first dimension given in slices ~
   changes fastest.) ~
   )~
   (:body The body of the iteration. As many forms as you would like.)~
   (:see-also doslices loop do dolist row-major-ops column-major-ops)~
   "
  
  (let ((slices-var (gensym "SLICES-VAR-")))
    `(cond ((or (eql ,order :col) (eql ,order :column))
            (let ((,slices-var
                   (cond ((eq ,slices :elements) :elements)
                         ((numberp ,slices) (list ,slices))
                         ((null ,slices) NIL)
                         ((listp ,slices) 
                          (remove-duplicates (sort ,slices #'<)))
                         (t ,slices))))
              (loop for index from 0 to (- (number-of-slices ,object ,slices-var)
                                           1)
                    
                    collect
                    (let ((,slice-var
                           (column-major-ref-slice ,object ,slices-var index)))
                      (declare (ignorable ,slice-var))
                      ,@body))))
           (t
            (let ((,slices-var
                   (cond ((eq ,slices :elements) :elements)
                         ((numberp ,slices) (list ,slices))
                         ((null ,slices) NIL)
                         ((listp ,slices) 
                          (remove-duplicates (sort ,slices #'<)))
                         (t ,slices))))
              (loop for index from 0 to (- (number-of-slices ,object ,slices-var)
                                           1)
                    
                    collect
                    (let ((,slice-var
                           (row-major-ref-slice ,object ,slices-var index)))
                      (declare (ignorable ,slice-var))
                      ,@body)))))))
