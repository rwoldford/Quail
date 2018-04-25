;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               reduce-slices.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1994 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1994.
;;;
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(reduce-slices)))

;--------------------------------------------------------------------------------

(defun reduce-slices (fun ref-object
                          &key
                          (slices :elements)
                          (type :exterior)
                          (initial-value NIL init-supplied?)
                          (order :row)
                          (list? NIL))
  "Combines all the slices or elements within each slice using the binary function fun. ~
   Proceeds by calling fun on initial-value and the first slice or element, ~
   then calling fun on the result and the next slice or element, and so on. ~
   Typically, the last evaluation is returned. ~
   Type :exterior means operating over slices, whereas type :interior means ~
   operating over elements within slices. ~
   In the latter case, reduce-slices tries to return a sensible structure for ~
   the results. ~
   If initial-value is not supplied then the iteration begins with the first ~
   slice or element.~
   (:required ~
   (:arg fun A binary operator that will return a value when given two arguments.)~
   (:arg ref-object The refable object over which slicing is to take place.) ~
   )~
   (:key ~
   (:arg slices :elements ~
   The integer or list of fixed dimensions that identifies a slice.~
   For example if slices is 0 then a slice is defined to be the set of all ~
   elements of a ref-object having a common value of the zeroth index. ~
   There will be as many slices as the size of the zeroth dimension of the ~
   ref-object. ~
   Similarly, if slices is '(0 1) then a slice of a ref-object ~
   is the set of all elements ~
   having common zeroth and first index and there will be as many slices ~
   as there are pairs of zeroth and first indices.  ~
   If NIL then the whole of the ref-object is taken to be the slice. ~
   Slices may also be the keyword :elements in which case the elements ~
   are accessed directly as opposed to a ref of them.) ~
   (:arg type :exterior The type of reduction to be performed.  If type is ~
   :exterior then the function is applied to pairs of slices and reduction ~
   occurs over slices.  If type is :interior then the function is applied to pairs ~
   of elements within each slice.) ~
   (:arg initial-value no-value If supplied this value will be used as the ~
   first argument to the function, the first slice or element being the second. ~
   If not supplied then the binary function will begin with the first slice ~
   or element as its first argument and the next slice or element as its second. ~
   If in this case there is only one slice so that no next slice is available ~
   to be the second argument, then the function will be ~
   applied as a unary function to the first slice alone.)~
   (:arg order :row The order in which iteration is to take place over and within ~
   slices where applicable.) ~
   (:arg list? NIL If non-NIL and type is :interior the results will be returned ~
   in a list.  Otherwise if type is :interior the results will be returned in ~
   a ref-object of the same dimensions as the slices filled according to order. ~
   If type is not :interior, then list? does not apply and is ignored.) ~
   )~
   (:returns For :exterior type, the results of combining all slices pairwise ~
   with fun.  For :interior type either a ref-object of the same class as the ~
   argument ref-object but with dimensions the same as its slices or a list if list?~
   is non-NIL.)~
   (:elaboration ~
   In the current implementation, the :interior is more efficient than the :exterior ~
   reduction in those cases that are comparable in function. ~
   )~
   (:see-also map-slices doslices collect-slices row-major-ops column-major-ops)~
   (:examples (:files (Mapping functions eg:Arrays;iter-map.lisp ~
   ) ~
   )~
   )~
   "
  (if (eq order :col) (setf order :column))
  (when (numberp slices) (setf slices (list slices)))
  (cond
   ((eq type :exterior)
    (let (result
          (start 0)
          (end (- (number-of-slices ref-object slices) 1)))
      ;; Get starting values set up.
      
      (unless init-supplied?
        (setf initial-value
              (row-major-ref-slice ref-object slices start))
        (incf start))
      
      ;; Now start the thing cranking
      (cond
       ((> start end)
        (funcall fun initial-value))
       ;; exterior row-order
       ((eq order :row)
        (setf result
              (funcall fun initial-value
                       (row-major-ref-slice ref-object slices start)))
        (incf start)
        (loop
          for i from start
          to end
          do
          (setf result
                (funcall fun result
                         (row-major-ref-slice ref-object slices i)))
          )
        result)
       ;; exterior column-order
       ((eq order :column)
        (setf result
              (funcall fun initial-value
                       (column-major-ref-slice ref-object slices start)))
        (incf start)
        (loop
          for i from start to end
          do
          (setf result
                (funcall fun result
                         (column-major-ref-slice ref-object slices i))))
        result)
       (T
        (quail-error "Illegal order given to reduce-slices -- ~s ~%~
                      Order must be one of (:column :row)." order)))))
   ((eq type :interior)
    (if list?
      ;; then just collect them up
      (if init-supplied?
        (collect-slices (slice ref-object slices order)
          (reduce-slices fun slice
                         :order order
                         :type :exterior
                         :slices :elements
                         :initial-value initial-value))
        (collect-slices (slice ref-object slices order)
          (reduce-slices fun slice
                         :order order 
                         :slices :elements
                         :type :exterior)))
      ;; else try and make a structure of the same type as ref-object
      ;; and the same dimsneions as a single slice and stuff the results
      ;; in the cells.
      ;; The way this is now achieved is by copying a  single slice
      ;; and filling it.
      ;; There has to be a more efficient way to do this
      ;; ******* rwo
      (let ((complement-dims (iseq (length (dimensions-of ref-object))))
            result)
        (loop
          for i in slices
          do (setf complement-dims (remove  i complement-dims)))
        (setf result
              (sel (column-major-ref-slice
                    ref-object
                    complement-dims
                    0)))
        (cond
         ((eq order :row)
          (if init-supplied?
            (loop for i from 0 to (- (number-of-slices ref-object slices) 1)
                  do
                  (setf
                   (row-major-eref result i)
                   (reduce-slices
                    fun 
                    (row-major-ref-slice ref-object slices i)
                    :slices :elements
                    :order :row
                    :type :exterior
                    :initial-value initial-value)))
            (loop for i from 0 to (- (number-of-slices ref-object slices) 1)
                  do
                  (setf
                   (row-major-eref result i)
                   (reduce-slices
                    fun 
                    (row-major-ref-slice ref-object slices i)
                    :slices :elements
                    :order :row
                    :type :exterior)))
            )
          result)
         ((eq order :column)
          (if init-supplied?
            (loop for i from 0 to (- (number-of-slices ref-object slices) 1)
                  do
                  (setf
                   (column-major-eref result i)
                   (reduce-slices
                    fun 
                    (column-major-ref-slice ref-object slices i)
                    :slices :elements
                    :order :column
                    :type :exterior
                    :initial-value initial-value)))
            (loop for i from 0 to (- (number-of-slices ref-object slices) 1)
                  do
                  (setf
                   (column-major-eref result i)
                   (reduce-slices
                    fun 
                    (column-major-ref-slice ref-object slices i)
                    :slices :elements
                    :order :column
                    :type :exterior)))
            )
          result)
         (T
          (quail-error "Illegal order given to reduce-slices -- ~s ~%~
                        Order must be one of (:column :row)." order))))
      )
    )
   )
  )
