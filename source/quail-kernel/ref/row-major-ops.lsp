;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                           row-major-ops.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(row-major-eref
          row-major-list-elements
          row-major-set-elements
          row-major-ref-slice)))


;;;---------------------------------------------------------------------------
;;;
;;; Row-major-eref
;;;
;;; Note definition inside a lexical closure.
;;;
;;;---------------------------------------------------------------------------

(let* ((array nil)
       (dimensions nil)
       (subscripts nil)
       (index nil)
       (result nil))
  (defun row-major-eref (a i)
    "Returns the i-th element of the argument a, counting by row major order.  ~
     It can be used with setf to set elements of its argument.~
     (:required ~
     (:arg A The object whose i'th element is to be returned.) ~
     (:arg i The row major index of the element to be returned.) ~
     )~
     (:see-also column-major-eref row-major-ref-slice)"
    (if (eq a array)
      (case (- i index)
        (-1 (setf subscripts 
                  (prior-subscripts subscripts dimensions :row)))
        ( 0 result)
        ( 1 (setf subscripts 
                  (next-subscripts subscripts dimensions :row)))
        ( t (setf subscripts 
                  (index-to-subscripts i dimensions :row))))
      (progn
        (setf array a)
        (setf dimensions (dimensions-of a))
        (setf subscripts (index-to-subscripts i dimensions :row))))
    (setf index i)
    (setf result (apply #'eref a subscripts))
    result))


;;;---------------------------------------------------------------------------
;;;
;;; Row-major-eset
;;;
;;; Note definition inside a lexical closure.
;;;
;;;---------------------------------------------------------------------------

(let* ((array nil)
       (dimensions nil)
       (subscripts nil)
       (index nil)
       (result nil))
  (defun row-major-eset (a i new-value)
    "Sets the value of the i-th element of the argument a, ~
     counting by row major order."
    (declare (special *setf-eref*))
    (if (eq a array)
      (case (- i index)
        (-1 (setf subscripts 
                  (prior-subscripts subscripts dimensions :row)))
        ( 0 result)
        ( 1 (setf subscripts 
                  (next-subscripts subscripts dimensions :row)))
        ( t (setf subscripts 
                  (index-to-subscripts i dimensions :row))))
      (progn
        (setf array a)
        (setf dimensions (dimensions-of a))
        (setf subscripts (index-to-subscripts i dimensions :row))))
    (setf index i)
    (setf result (apply *setf-eref* new-value a subscripts))
    result))

;;;--------------------------------------------------------------------------------
;;; row-major-eref setf macro
;;;--------------------------------------------------------------------------------

(defsetf row-major-eref row-major-eset)


;;;---------------------------------------------------------------------------
;;;
;;; Row-major-list-elements
;;;
;;; Note definition inside a lexical closure.
;;;
;;;---------------------------------------------------------------------------

(defun row-major-list-elements (object)
  "Returns a list containing all elements of the object in row major order.~
   (:see-also row-major-set-elements row-major-eref row-major-ref-slice ~
   column-major-list-elements)"
  (do ((counter (- (number-of-elements object) 1)
                (- counter 1))
       (result nil 
               (push (row-major-eref object counter) result)))
      ((minusp counter) 
       result)
    ()))



;;;---------------------------------------------------------------------------
;;;
;;; Row-major-set-elements
;;;
;;; Note definition inside a lexical closure.
;;;
;;;---------------------------------------------------------------------------

(defun row-major-set-elements (object list)
  "Sets the elements in object to that of list, iterating over object's ~
   elements in row-major-order.~
   (:see-also row-major-list-elements row-major-eref row-major-ref-slice ~
   column-major-set-elements)"
  (dotimes (counter (number-of-elements object) object)
    (setf (row-major-eref object counter)
          (elt list counter))))




;;;---------------------------------------------------------------------------
;;;
;;; Row-major-ref-slice
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
  (defun row-major-ref-slice (a slices i)
    "Returns the i-th slice of the refable object a, indexing by row major order.  ~
     Like row-major-eref in this way except that slices of a can be identified as ~
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
     as there are pairs of zeroth and first indices. ~
     If NIL then the whole of the object a is taken to be the slice. ~
     Slices may also be the keyword :elements in which case the elements ~
     are accessed directly as opposed to a ref of them.  In this special ~
     case, row-major-ref-slice is identical to row-major-eref)~
     (:arg i The row major index of the slice to be returned.) ~
     )~
     (:see-also row-major-eref column-major-ref-slice)"
    (cond
     ((eq :elements slices)
      ;; Treat elements as slices
      (row-major-eref a i))
     ((null slices)
      ;;(or (null slices)
      ;;    (and (= (length slices) 1)
      ;;         (null (first slices))))
      ;; if no slices are fixed just do a row-major-eref
      (ref a))
     (t
      (if (and (eq a array) (equal slices f-slices))
        (case (- i index)
          (-1 (setf subscripts 
                    (prior-subscripts subscripts f-dims :row)))
          ( 0 result)
          ( 1 (setf subscripts 
                    (next-subscripts subscripts f-dims :row)))
          ( t (setf subscripts 
                    (index-to-subscripts i f-dims :row))))
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
          (setf subscripts (index-to-subscripts i f-dims :row))))
      (setf index i)
      (setf result
            (apply #'ref a 
                   (expand-subscripts subscripts r-slices)))
      result)
     ))
  )

;;;---------------------------------------------------------------------------
;;;
;;; Row-major-set-slice
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
  (defun row-major-set-slice (a slices i new-value)
    "Sets the i-th element of the refable object a, counting by row major order.  ~
     Like row-major-eset in this way except that slices of a can be identified as ~
     fixed by giving their dimension number (0, 1, 2, ...) as further arguments."
    (cond
       ((eq :elements slices)
        ;; Treat elements as slices
        (setf (row-major-eref a i) new-value))
       ((null slices)
        ;;(or (null slices)
        ;;    (and (= (length slices) 1)
        ;;         (null (first slices))))
        ;; if no slices are fixed just do a row-major-eref
        (setf (ref a) new-value))
       (t
        (if (and (eq a array) (equal slices f-slices))
          (case (- i index)
            (-1 (setf subscripts 
                      (prior-subscripts subscripts f-dims :row)))
            ( 1 (setf subscripts 
                      (next-subscripts subscripts f-dims :row)))
            ( t (setf subscripts 
                      (index-to-subscripts i f-dims :row))))
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
            (setf subscripts (index-to-subscripts i f-dims :row))))
        (setf index i)
        (apply *setf-ref* new-value a 
               (expand-subscripts subscripts r-slices))))))

;;;--------------------------------------------------------------------------------
;;; row-major-ref-slice setf macro
;;;--------------------------------------------------------------------------------

(defsetf row-major-ref-slice row-major-set-slice)


