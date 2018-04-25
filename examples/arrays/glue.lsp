;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                   Putting arrays together
;;;
;;;
;;;  Authors:
;;;     D.G. Anglin 1994.

(in-package :quail-user)

(setf a (array '((1.2 3 4.5) (6.7 8.9 0.1))
                :dimensions '(2 3)))

(setf b (vector 100 200))

(setf c (array 7 :dimensions '(2 2)))

;;;----------------------------------------------------------------------------

;;; cglue glues objects with conformable columns together
;;; here a has columns of height 2, and the 1d objects b are treated as columns

(cglue a b)

(cglue 1 2 3)

(cglue '(1 2) b c)

;; (cglue a 1) ;; an error

(cglue a (array 1 :dimensions 2))

(cglue b c)

;;; rglue glues objects with conformable columns together
;;; here, by default, 1d objects are treated as rows

(rglue 1 2 3)

(rglue '(1 2) b c)

(rglue (array "foo" :deconstruct t) a)  ;; note that the return class is ref-array

;;; It *is* possible to override the default treatment of 1d objects
(setf cc (ref c T 1))
(rglue '(1 2) cc)
(rglue '(1 2) cc :1d :col)
(cglue '(1 2) cc)
(cglue '(1 2) cc :1d :row)

;;;----------------------------------------------------------------------------

;;;  There is a generalization called glue.  Objects can be glued together along
;;;  a given axis if the dimensions-of each object, with that axis removed,
;;;  match exactly.  In the following example, d and e can be glued along axis 1,
;;;  since the other axes match in length.

(setf d (array 1 :dimensions '(2 2 4)))

(setf e (array 2 :dimensions '(2 3 4)))

(setf f (glue d e :at 1))

(dimensions-of f)

;;; The axis to attempt defaults to 0, hence the following bombs ...

(glue d e)    ;; an error

