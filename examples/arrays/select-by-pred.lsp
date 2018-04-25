;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                   Selecting elements by predicate
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1994.

(in-package :quail-user)
;;;
;;;
(setf a (array '((1.2 3 4.5) (6.7 8.9 0.1))
                :dimensions '(2 3)))
;;;
;;;  For large arrays, it is often more convenient to do
;;;  assignment with <- instead of setf.  No value is returned
;;;  (and hence no value printed!) with <-

(<- b (array (random-gaussian :n 100) :dimensions '(5 4 5)))
(<- c (array (random-cauchy :n 100) :dimensions '(5 4 5)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;       INDICES
;;;
;;; find the indices of a whose values are greater than 2.

(defun 2< (x) (< 2 x))

(indices a #'2<)

;;;
;;;  Look for some outliers
;;;

(defun outlier (x) (> (abs x) 3))

;;; Indices of those from a cauchy sample
;;;

(indices c #'outlier)

;;; From a gaussian sample

(indices b #'outlier)

;;;
;;; Now select those observations
;;;

(ref b (indices b #'outlier))
(ref c (indices c #'outlier))

(ref a (indices a #'2<))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    REF-IF
;;;

;;; The above behaviour has been put together as a single function
;;;

