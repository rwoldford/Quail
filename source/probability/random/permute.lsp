;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               permute.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1994 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1994
;;;
;;;
;;;--------------------------------------------------------------------------------
;;; 

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(permute permute-object)))

(defun permute (object
                &key 
                (slices  :elements)
                (copy?   nil)
                (type   :exterior))
  
  "Randomly permutes the elements or slices of object.~
   The default is that this is destructive to the original object.~
   (:required ~
   (:arg object The object whose contents are to be permuted.) ~
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
   are accessed directly as opposed to a ref of them.
   In any case the permutation applies over all possible slices.)~
   (:arg copy? NIL If non-NIL a copy is first made of object and the ~
   the copy is permuted. If NIL, the contents of the original object are ~
   destructively rearranged.)~
   (:arg type :exterior Two types of random permutation ~
   are possible: :interior and :exterior. ~
   The interior one permutes the order of ~
   the elements within the specified slices.
   The exterior one permutes the order of ~
   the slices and not the elements within each one.) ~
   )~
   (:see-also order permute-object random-discrete-uniform random-value) ~
   (:elaboration The method of permutation is a simple swapping algorithm based ~
   on as many discrete uniforms as there are slices.)~
   (:returns The randomly permuted object.)"
  
  
  (cond
   ((numberp slices) (setf slices (list slices)))
   ((and slices (listp slices))
    (setf slices (cl:sort (remove-duplicates slices) #'<))))
  
  (permute-object
   (if copy? (sel object) object)
   slices
   type)
  )

(defgeneric permute-object (object slices type)
  (:documentation
  "Randomly permutes the elements or slices of object.~
   This is destructive to the original object.~
   (:required ~
   (:arg object The object whose contents are to be permuted.) ~
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
   are accessed directly as opposed to a ref of them.~
   In any case the permutation applies over all possible slices.)~
   (:arg type :exterior Two types of random permutation ~
   are possible: :interior and :exterior. ~
   The interior one permutes the order of ~
   the elements within the specified slices.  ~
   The exterior one permutes the order of ~
   the slices and not the elements within each one.) ~
   )~
   (:elaboration The method of permutation is a simple swapping algorithm based ~
   on as many discrete uniforms as there are slices.)~
   (:returns The randomly permuted object.)"))
  

(defmethod permute-object (object
                           slices
                           type)
  "Missing method for all unclassified cases."
  (missing-method 'permute-object 
                  object
                  slices
                  type)
  )




(defmethod-multi permute-object ((object (symbol number character))
                                 slices
                                 type)
  "Returns object since permuting doesn't quite make sense here."
  (declare (ignore slices type))
  object)


(defmethod-multi permute-object ((object (dimensioned-ref-object sequence array))
                                 (slices T)
                                 (type (eql :interior)))
  "Implements the interior sort as an exterior sort for every slice. "
  (doslices (slice object slices object)
    (permute-object slice :elements :exterior)
    )
  object)

(defmethod-multi permute-object ((object (dimensioned-ref-object sequence array))
                                 (slices T)
                                 (type (eql :exterior)))
  "Implements the exterior sort by simple swapping algorithm."
  (with-CL-functions (+ * / - = )
    (let ((n-1 (- (number-of-slices object slices) 1))
          x
          swap)
      (do ((i n-1 (- i 1)))
          ((= i 0))
        (setf x (random-discrete-uniform :from 0 :to i))
        (setf swap (sel (column-major-ref-slice object  slices i)))
        (setf (column-major-ref-slice object slices i)
              (column-major-ref-slice object slices x))
        (setf (column-major-ref-slice object slices x) swap)
        )))
  object
  )
#| Too clever by half
   Robson's method stinks
"(:elaboration The method of permutation used depends on the number of slices ~
   to be permuted.  If this number is small, then the one-pass decoding algorithm ~
   of Robson is used.  If it is large then a simple swapping algorithm based ~
   on as many discrete uniforms as there are slices is used.)~
   (:references Devroye, Luc (1986) Non-Uniform Random Variate Generation, ~
   Springer-Verlag.) ~
"

  (with-CL-functions (+ * / - = floor log)
    (let* ((n (number-of-slices object slices))
           (log-max (log most-positive-fixnum))
           (n-log (* n (log (/ (1+ n) 2.0)))))
      ;; Test based on Jensen's inequality
      (if (< n-log log-max)
        ;; Then the number of slices is small enough to use the
        ;; decoding algorithm of Robson.
        (let ((x (random-discrete-uniform 
                  :from 1 
                  :to (factorial n)))
              z
              swap)
          (do ((i n (- i 1)))
              ((= i 1))
            (multiple-value-setq (x z) (floor x i))
            (setf z (+ z 1))
            (setf swap 
                  (sel (column-major-ref-slice object slices (- i 1))))
            (setf (column-major-ref-slice object slices (- i 1))
                  (column-major-ref-slice object slices (- z 1)))
            (setf (column-major-ref-slice object slices (- z 1)) swap)
            ))
        ;; Else do simple swapping.
        (let ((n-1 (- n 1))
              x
              swap)
          (do ((i n-1 (- i 1)))
              ((= i 0))
            (setf x (random-discrete-uniform :from 0 :to i))
            (setf swap (sel (column-major-ref-slice object  slices i)))
            (setf (column-major-ref-slice object slices i)
                  (column-major-ref-slice object slices x))
            (setf (column-major-ref-slice object slices x) swap)
            )))
      object
      ))
  )
|#
