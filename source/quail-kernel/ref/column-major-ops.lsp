;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                           column-major-ops.lisp    
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     M.E. Lewis 1992.
;;;     R.W. Oldford 1992.
;;;
;;;
;;;----------------------------------------------------------------------------

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(column-major-eref
          column-major-list-elements
          column-major-set-elements
          column-major-ref-slice
          )))


;;;---------------------------------------------------------------------------
;;;
;;; Column-major-eref
;;;
;;; Note definition inside a lexical closure.
;;;
;;;---------------------------------------------------------------------------

(let* ((array nil)
       (dimensions nil)
       (dimensions-product nil)
       (subscripts nil)
       (index nil)
       (result nil))
  (defun column-major-eref (a i)
    "Returns the i-th element of the argument a, counting by column major order.  ~
     It can be used with setf to set elements of its argument.~
     (:required ~
     (:arg A The object whose i'th element is to be returned.) ~
     (:arg i The column major index of the element to be returned.) ~
     )~
     (:see-also row-major-eref column-major-ref-slice)"
    (if (eq a array)
      (case (- i index)
        (-1 (setf subscripts (prior-subscripts subscripts dimensions :col)))
        (0 result)
        (1 (setf subscripts (next-subscripts subscripts dimensions :col)))
        (t (setf subscripts (index-to-subscripts i dimensions :col))))
      (progn
        (setf array a)
        (setf dimensions (dimensions-of a))
        (setf dimensions-product (cum-prod dimensions))
        (setf subscripts (index-to-subscripts i dimensions :col))))
    (setf index i)
    (setf result (apply #'eref a subscripts))
    result))


;;;---------------------------------------------------------------------------
;;;
;;; Column-major-eset
;;;
;;; Note definition inside a lexical closure.
;;;
;;;---------------------------------------------------------------------------

(let* ((array nil)
       (dimensions nil)
       (dimensions-product nil)
       (subscripts nil)
       (index nil)
       (result nil))
  (defun column-major-eset (a i new-value)
    "Sets the value of the i-th element of the argument a, ~
     counting by column major order."
    (declare (special *setf-eref*))
    (if (eq a array)
      (case (- i index)
        (-1 (setf subscripts (prior-subscripts subscripts dimensions :col)))
        (0 result)
        (1 (setf subscripts (next-subscripts subscripts dimensions :col)))
        (t (setf subscripts (index-to-subscripts i dimensions :col))))
      (progn
        (setf array a)
        (setf dimensions (dimensions-of a))
        (setf dimensions-product (cum-prod dimensions))
        (setf subscripts (index-to-subscripts i dimensions :col))))
    (setf index i)
    (setf result (apply *setf-eref* new-value a subscripts))
    result))

;;;--------------------------------------------------------------------------------
;;; column-major-eref setf macro
;;;--------------------------------------------------------------------------------


(defsetf column-major-eref column-major-eset)


;;;---------------------------------------------------------------------------
;;;
;;; Column-major-list-elements
;;;
;;; Note definition inside a lexical closure.
;;;
;;;---------------------------------------------------------------------------

(defun column-major-list-elements (object)
  "Returns a list containing all elements of the object in column major order.~
   (:see-also column-major-set-elements column-major-eref column-major-ref-slice ~
   row-major-list-elements)"

  (do ((counter (- (number-of-elements object) 1)
                (- counter 1))
       (result nil 
               (push (column-major-eref object counter) result)))
      ((minusp counter) 
       result)
    ()))



;;;---------------------------------------------------------------------------
;;;
;;; Column-major-set-elements
;;;
;;;
;;;---------------------------------------------------------------------------

(defun column-major-set-elements (object list)
  "Sets the elements in object to that of list, iterating over object's ~
   elements in column-major-order.~
   (:see-also column-major-list-elements column-major-eref column-major-ref-slice ~
   row-major-set-elements)"
  (dotimes (counter (number-of-elements object) object)
    (setf (column-major-eref object counter)
          (elt list counter))))




;;;---------------------------------------------------------------------------
;;;
;;; Column-major-ref-slice
;;;
;;; Note definition inside a lexical closure.
;;;
;;;---------------------------------------------------------------------------


(let* ((array nil)
       (dimensions nil)
       (f-slices nil)
       (r-slices nil)
       (f-dims nil)
       (subscripts nil)
       (index nil)
       (result nil))
  (defun column-major-ref-slice (a slices i)
    "Returns the i-th slice of the refable object a, indexing by column major order.  ~
     Like column-major-eref in this way except that slices of a can be identified as ~
     fixed by giving a list of their dimension numbers as the slices argument.~
     It can be used with setf to set slices to new values.~
     (:required ~
     (:arg A The object whose i'th slice is to be returned.) ~
     (:arg slices The integer or list of fixed dimensions that identifies a slice.~
     For example if slices is 0 then a slice is defined to be the set of all ~
     elements of A having a common value of the zeroth index. ~
     There will be as many slices as the size of the zeroth dimension of A. ~
     Similarly, if slices is '(0 1) then a slice of A is the set of all elements ~
     of A having common zeroth and first index and there will be as many slices ~
     as there are pairs of zeroth and first indices.  ~
     If NIL then the whole of the object a is taken to be the slice. ~
     Slices may also be the keyword :elements in which case the elements ~
     are accessed directly as opposed to a ref of them.  In this special ~
     case, column-major-ref-slice is identical to column-major-eref)~
     (:arg i The column major index of the slice to be returned.) ~
     )~
     (:see-also column-major-eref row-major-ref-slice)"
    (cond
     ((eq :elements slices)
      ;; Treat elements as slices
      (column-major-eref a i))
     ((null slices)
      ;;(or (null slices)
      ;;    (and (= (length slices) 1)
      ;;         (null (first slices))))
      ;; if no slices are fixed just do a column-major-eref
      (ref a))
     (t (if (and (eq a array) (equal slices f-slices))
          (case (- i index)
            (-1 (setf subscripts 
                      (prior-subscripts subscripts f-dims :col)))
            ( 0 result)
            ( 1 (setf subscripts 
                      (next-subscripts subscripts f-dims :col)))
            ( t (setf subscripts 
                      (index-to-subscripts i f-dims :col))))
          (progn
            (setf array a)
            (setf dimensions (dimensions-of a))
            (setf f-slices (cl:sort
                            (remove-duplicates slices)
                            #'cl:<))
            (setf r-slices
                  (row-major-list-elements
                   (ref
                    (iseq (length dimensions))
                    (cons :c f-slices)
                    )))
            (setf f-dims
                  (row-major-list-elements
                   (ref
                    dimensions
                    f-slices
                    )))
            (setf subscripts (index-to-subscripts i f-dims :col))))
        (setf index i)
        (setf result
              (apply #'ref a 
                     (expand-subscripts subscripts r-slices)))
        )
     )
    )
  )

;;;---------------------------------------------------------------------------
;;;
;;; Column-major-set-slice
;;;
;;; Note definition inside a lexical closure.
;;;
;;;---------------------------------------------------------------------------

(let* ((array nil)
       (dimensions nil)
       (f-slices nil)
       (r-slices nil)
       (f-dims nil)
       (subscripts nil)
       (index nil))
  (defun column-major-set-slice (a slices i new-value)
    "Sets the i-th element of the refable object a, counting by column major order.  ~
     Like column-major-eset in this way except that slices of a can be identified as ~
     fixed by giving their dimension number (0, 1, 2, ...) as further arguments."
    (cond
       ((eq :elements slices)
        ;; Treat elements as slices
        (setf (column-major-eref a i) new-value))
       ((null slices)
        ;;(or (null slices)
        ;;    (and (= (length slices) 1)
        ;;         (null (first slices))))
        ;; if no slices are fixed just do a column-major-eref
        (setf (ref a) new-value))
       (t
        (if (and (eq a array) (equal slices f-slices))
          (case (- i index)
            (-1 (setf subscripts 
                      (prior-subscripts subscripts f-dims :col)))
            ( 1 (setf subscripts 
                      (next-subscripts subscripts f-dims :col)))
            ( t (setf subscripts 
                      (index-to-subscripts i f-dims :col))))
          (progn
            (setf array a)
            (setf dimensions (dimensions-of a))
            (setf f-slices (cl:sort
                            (remove-duplicates slices)
                            #'cl:<))
            (setf r-slices
                  (column-major-list-elements
                   (ref
                    (iseq (length dimensions))
                    (cons :c f-slices)
                    )))
            (setf f-dims
                  (column-major-list-elements
                   (ref
                    dimensions
                    f-slices
                    )))
            (setf subscripts (index-to-subscripts i f-dims :col))))
        (setf index i)
        (apply *setf-ref* new-value a 
               (expand-subscripts subscripts r-slices))))))

;;;--------------------------------------------------------------------------------
;;; column-major-ref-slice setf macro
;;;--------------------------------------------------------------------------------

(defsetf column-major-ref-slice column-major-set-slice)
