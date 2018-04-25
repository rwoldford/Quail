;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               seq-utilities.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1990
;;;
;;;
;;;--------------------------------------------------------------------------------
;;;

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(seq-min seq-max seq-destructive-union general-seq-union seq-union
          type-of-seq seq-complement seq-difference seq-intersection
          seq-sym-difference)))


;;; ----------------------------------------------------------------------------
;;;   
;;;  A file full of useful operators on (primarily) numerical CL sequences.
;;;
;;; ----------------------------------------------------------------------------


(defun seq-min (sequence)
  (reduce #'min sequence))
                             
(defun seq-max (sequence)
  (reduce #'max sequence))


;;;----------------------------------------------------------------------------
;;;
;;; A general set-union operator for any CL sequence.
;;; The keyword argument is to be a comparison predicate function that
;;; operates as an appropriate "less-than" function.  Same as key for
;;; (merge ... ) in CLtL chapter on sequences.
;;;
;;;----------------------------------------------------------------------------

(defun seq-destructive-union
       (seq-1 seq-2 &key (increase NIL))
  
  (delete-duplicates
     (if increase
         ;; then
         (merge (type-of-seq seq-1) seq-1 seq-2 increase)
         ;; else
         (concatenate (type-of-seq seq-1) seq-1 seq-2))))


(defun general-seq-union
       (seq-1 seq-2 &key (increase NIL))

  (let ((copy-seq-1 (copy-seq seq-1))
        (copy-seq-2 (copy-seq seq-2)))
    (seq-destructive-union copy-seq-1 copy-seq-2 :increase increase)))

;;;----------------------------------------------------------------------------
;;;
;;; Set operations for CL sequences whose contents are numbers, arranged
;;; in STRICTLY INCREASING ORDER.
;;;
;;;----------------------------------------------------------------------------

(defun seq-union (seq-1 seq-2)
  (general-seq-union seq-1 seq-2 :increase #'<))


(defun type-of-seq (sequence)
  (if (listp sequence)
    'list
    (type-of sequence)))

(defun seq-complement (seq-1 seq-2)
  (seq-difference seq-2 seq-1))


(defun seq-difference (seq-1 seq-2)
  (remove-if #'(lambda (x) (find x seq-2 :test #'equal))  seq-1))


(defun seq-intersection (seq-1 seq-2)
  (seq-difference seq-1 (seq-difference seq-1 seq-2)))


(defun seq-sym-difference (seq-1 seq-2)
  (seq-union (seq-difference seq-2 seq-1) (seq-difference seq-1 seq-2)))
