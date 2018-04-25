;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               ref-eq.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1991.
;;;
;;;
;;;--------------------------------------------------------------------------------

(in-package :qk)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(ref-eq)))

(defgeneric ref-eq (x y))

(defmethod ref-eq ((x t) (y t))
  (eq x y))

(defmethod ref-eq ((x dimensioned-ref-object) (y dimensioned-ref-object))
  (or (call-next-method x y)
      (and (eq (ref-obj-of x) (ref-obj-of y))
           (equal (specs-mask-of x) (specs-mask-of y))
           (let ((xs (specs-of x))
                 (ys (specs-of y)))
             (or (equal xs ys)
                 (specs-the-same-p xs
                                   ys
                                   (dimensions-of (ref-obj-of x))))))))

;;; Does this work when it's supposed to?

(defmethod ref-eq ((x quail-open-mixin) (y quail-open-mixin))
  (and (not (quail-open-p-object x))
       (not (quail-open-p-object y))
       (call-next-method x y)))

;;;  What this really needs to do is carefully analyze the specs and 
;;;  determine whether they look at the same part of the (shared)
;;;  ref-object of x and y.  This'll be a real problem if the specs
;;;  of either x or y is not a list ... for now this case returns nil.
;;;
;;;  takes xs = specs of x
;;;        ys = specs of y
;;;         d = dimensions of shared ref-object
;;;
;;;  One can modify specs of x or y before passing to this routine if
;;;  desired  (eg. sort and remove duplicates, to check if same stuff is
;;;  referred to, but maybe somewhat differently in each object)

(defun specs-the-same-p (xs ys d)
  (and (listp xs)
       (listp ys)
       (let ((the-same t))
         (loop for xi in xs
               as yi in ys
               as di in d
               while the-same
               do
               (setf the-same 
                     (or (eq xi yi)
                         (and (listp xi) (listp yi)
                              (equal xi yi))
                         (let ((xii
                                (cond
                                 ((eq xi t) (iseq di))
                                 ((numberp xi) (list xi))
                                 ((and (listp xi)
                                       (eq (first xi) :c))
                                  (set-difference (iseq di)
                                                  (cdr xi)
                                                  :test #'=))
                                 (T xi)))
                               (yii
                                (cond
                                 ((eq yi t) (iseq di))
                                 ((numberp yi) (list yi))
                                 ((and (listp yi)
                                       (eq (first yi) :c))
                                  (set-difference (iseq di)
                                                  (cdr yi)
                                                  :test #'=))
                                 (T yi))))
                           (equal xii yii)))))
         the-same)))
                                 
