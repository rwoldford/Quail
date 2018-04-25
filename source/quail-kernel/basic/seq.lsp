;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               seq.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1991 -1992, 1994.
;;;     M.E. Lewis 1991.
;;;
;;;
;;;----------------------------------------------------------------------------
;;;

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(iseq
          seq)))

 

;;;   functions that produce a sequence of numbers
;;;
;;;


(defun iseq (s &optional e)
  "Returns a list of integers from s to e.  ~
   If e is not specified, from 0 to s minus 1.  ~
   If e is less than s the count is down from s to e.  ~
   (:see-also seq)"
  (let ((start (if e s 0))
        (end (if e e (- s 1))))
    (if (< end start)
      (loop for i from start downto end collect i)
      (loop for i from start to end collect i))))


(defun seq (from to &optional (by 1))
  "Returns a ref-array whose elements are the sequence ~
   beginning at the first argument continuing to the second ~
   argument in step size specified by the optional third argument ~
   (default 1).  (:see-also iseq)"
  (let ((end (truncate (abs (/ (- to from) by)))))
    (when (< by 0)
      (quail-error "SEQ: Optional arg by = ~s must be positive!" by))
    (array
     (cond ((= to from)
            (list to))
           ((< to from)
            (if (= 1 by)
              (loop for i from 0 to end collect (- from i))
              (loop for i from 0 to end collect (- from (* i by)))))
           ((zerop from)
            (if (= by 1)
              (loop for i from 0 to end collect i)
              (loop for i from 0 to end collect (* i by))))
           (t
            (if (= by 1)
              (loop for i from 0 to end collect (+ i from))
              (loop for i from 0 to end collect (+ from (* i by)))))
           )
     :dimensions (list (+ 1 end)))))
