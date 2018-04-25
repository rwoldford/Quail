;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               remove-slices.lisp                               
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(remove-slices remove-slices-if)))

;--------------------------------------------------------------------------------

(defun remove-slices (item ref-object
                           &key
                           (slice 0)
                           (test #'ref-eq))
  "Removes all slices from ref-object which match item when the function test ~
   is applied to item and the slice of ref-object.~
   It is non-destructive to ref-object and returns an appropriately tailored ~
   ref of ref-object.~
   (:see-also remove-slices-if remove doslices collect-slices)~
   (:required ~
   (:arg item The item to be compared to each slice in ref-object.)~
   (:arg ref-object The source of the slices which will be compared to item.) ~
   ) ~
   (:key ~
   (:arg slice 0 The dimension that defines the slices.  Because a slice is being ~
   completely removed, it only makes sense to specify a single dimension here. ~
   Note however that if the (number-of-dimensions ref-object) is <= 1 ~
   then the slicing is taken over all elements, not refs of them.) ~
   (:arg test #'ref-eq The function to be called on item and slice.  If it returns ~
   non-NIL then that slice is removed, otherwise it remains.)~
   )~
   (:returns If no slices are removed, ref-object is returned.  If all slices are ~
   removed it returns NIL. Otherwise ~
   a ref of the original ref-object is returned.) ~
   "
  (let ((slices (if (<= (number-of-dimensions ref-object) 1)
                  :elements
                  (list slice)))
        locs result)
    
    (setf locs
          (loop for i from 0 to (- (number-of-slices ref-object slice) 1)
                when
                (funcall test item (column-major-ref-slice ref-object slices i))
                collect i))
    (setf result
          (if locs
            (let ((indices (make-sequence 'list (length (dimensions-of ref-object))
                                          :initial-element T)))
              (setf (eref indices slice) (cons :c locs))
              (apply #'ref ref-object indices))
              ref-object))
    (if (zerop (number-of-elements result))
      NIL
      result)))
    


(defun remove-slices-if (pred ref-object
                           &key
                           (slice 0))
  "Removes every slice from ref-object which returns non-NIL ~
   when the function pred is applied to it.~
   It is non-destructive to ref-object and returns an appropriately tailored ~
   ref of ref-object.~
   (:see-also remove-slices remove-if doslices collect-slices)~
   (:required ~
   (:arg pred The predicate function to be evaluated at each slice ~
   in ref-object.)~
   (:arg ref-object The source of the slices which will be compared to item.) ~
   ) ~
   (:key ~
   (:arg slice 0 The dimension that defines the slices.  Because a slice is being ~
   completely removed, it only makes sense to specify a single dimension here. ~
   Note however that if the (number-of-dimensions ref-object) is <= 1 ~
   then the slicing is taken over all elements, not refs of them.) ~
   )~
   (:returns If no slices are removed, ref-object is returned.  ~
   If all slices are removed it returns NIL. Otherwise ~
   a ref of the original ref-object is returned.) ~
   "
  (let ((slices (if (<= (number-of-dimensions ref-object) 1)
                  :elements
                  (list slice)))
        locs result)
    
    (setf locs
          (loop for i from 0 to (- (number-of-slices ref-object slice) 1)
                when
                (funcall pred (column-major-ref-slice ref-object slices i))
                collect i))
    (setf result 
          (if locs
            (let ((indices (make-sequence 'list (length (dimensions-of ref-object))
                                          :initial-element T)))
              (setf (eref indices slice) (cons :c locs))
              (apply #'ref ref-object indices))
            ref-object))
    (if (zerop (number-of-elements result))
      NIL
      result)))
    

