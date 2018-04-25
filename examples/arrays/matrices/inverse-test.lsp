(<- n 3)
(<- iden (identity-matrix n))

(defun print-mat (mat)
  (let* ((dims (dimensions-of mat))
         (dim-1 (first dims))
         (dim-2 (second dims)))
    (loop for i from 0 below dim-1
          do
          (format *terminal-io*
                  "~&~%Row ~s~&~5T" i)
          (loop for j from 0 below dim-2
                do (format *terminal-io*
                           "  ~s" (eref mat i j))
                )
          )))

(print-mat iden)
(setf i2 (ref iden '(0 1) '(0 1)))
(print-mat i2)
(ref i2 0 1)
(setf i~0 (ref iden '(:c 0) '(:c 0)))
(print-mat i~0)
(setf not-square (ref iden T '(:c 0)))
(class-of not-square)
(print-mat not-square)
(ref iden)

(setf si (sel iden))
si
(print-mat si)

(setf si (sel iden '(0 1) '(0 1)))
(print-mat si)

(setf si (sel iden '(:c 0 1) '(1) ))
(print-mat si)
(setf si (sel iden '(:c 0 1) '(0 1) :shape t))
(print-mat si)
(setf si (sel iden '(0) '(0 1) :shape t))
(inspect si)
(print-mat si)

(inspect iden)
(ref iden T '(:c 0))
(setf ri (ref iden))

(sel iden)
(sel ri)



(<- x (array '(1 2 3 4 5 6) :dimensions '(3 2)))
(<- xtx (.* (tp x) x))
(<- y (+ (.* X (array '(5 7)))
         (random-gaussian :n 3 :scale .1)))
(<- xty (.* (tp x) y))

(<- ixtx (inverse xtx))
(inspect ixtx)
(.* ixtx xtx)
(slot-value ixtx 'q::inverse-object)
(slot-value xtx 'q::inverse-object)
(eref xtx 0 0)
(eref ixtx 0 0)
;; problem is that lu-of makes a copy of the inverse-matrix first inside
;; calculate-inverse
(step (eref ixtx 0 0))
(force-evaluation ixtx)
(trace force-evaluation change-class sel eref ref)
