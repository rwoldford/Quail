;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                           indices.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Bob White
;;;
;;;
;;;----------------------------------------------------------------------------

(in-package :quail-kernel)


(eval-when (:compile-toplevel :load-toplevel :execute) (export '(indices)))

(defgeneric indices (object predicate)
  (:documentation 
   "Applies the predicate to each element of object and returns the ~
    index of those elements which return T.  These are returned in an ~
    array which may in turn be passed as the index argument to ref.  ~
    If no elements satisfy the predicate then NIL is returned."))

;; should really do multiple methods for cases we can handle, but
;; good enough for now ...

(defmethod indices (object predicate)
  (let* ((dim (dimensions-of object))
         (current (make-list (length dim) :initial-element 0))
         (contents
          (loop for i from 1 to (number-of-elements object)
                append
                (prog1
                  (if (funcall predicate
                               (apply #'eref object current))
                    (list (copy-list current)))
                  (row-major-next-subscript current dim)))))
    (if contents
      (array contents
           :class 'ref-array
           :dimensions (list (length contents))))))
