;;; first problem ... solved ?

(setf m (array '(1 3) :initial-contents '((0 1 2))))

(setf a (ref m 0 (list :c 1) :shape t))

(sel a 0)

;;; first problem ... _the day after_

(setf x (make-array '(1 2 2) :initial-contents '(((0 0) (0 2)))))

(setf b (ref m x :shape t))

(sel b 0)                ; fails

;;; second problem ... not solved

(setf gg (array '(1) :initial-element 2))

(setf hh (array '(1 1) :initial-element 3))

(Q+ hh gg)