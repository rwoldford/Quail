;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                   Advanced array creation
;;;
;;;
;;;  Authors:
;;;     D.G. Anglin 1994.

(in-package :q-user)

(array '(1 2 3 4 5 6) :dimensions '(3 2))

(array '(1 2 3 4 5 6) :dimensions '(3 2) :fill :col)

(array '((1 2) (1 1)) :dimensions '(2))

;; (array '((1 2) (3 4) (5 6)) :dimensions '(6))    ;; an error

(array (vector 0 1 2)
       :dimensions '(3 4)) ;; note orientation of fill !!

(array (make-array '(3 1) :initial-contents '((0) (1) (2)))
       :dimensions '(3 4)
       :fill :col)

(array '(1 2 3) :dimensions '(4))   ;; a warning !!

(array '(1 2 3 4) :dimensions '(3)) ;; another warning ..

;; a simple implementation of matrix transpose

(defun my-transpose (x)
  (array x 
         :dimensions (reverse (matrix-dimensions-of x))
         :fill :col))

(my-transpose (array '((1 2) (3 4))))

(my-transpose '(1 2 3 4))   ;; 1d things are by default columns; so, result is 1x4

;; The next is an error since the contents are an incorrectly shaped
;; nested list.

;; (array '((1 2 3) (4 5 6)) :dimensions '(3 2))  ;; an error

;; Do this instead to deal with nested lists

(array (flatten '((1 2 3) (4 5 6))) :dimensions '(3 2))  ;; works

;; suppose you want a ref-array full of a single list

(array '(1 2 3) :deconstruct nil :dimensions '(2 2))

;; but each element is eq

(setf a1 '(1 2 3))
(setf a2 (array a1 :deconstruct nil :dimensions '(2 2)))
(setf (eref (eref a2 0 1) 2) 300)
a1
a2

;; can avoid this problem, though

(setf a4 (array a1 :deconstruct nil :dimensions '(2 2) :element-copy #'copy-list))
(setf (eref (eref a4 0 0) 1) 2000)
a1
