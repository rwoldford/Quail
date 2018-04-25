(in-package :wb)

(defun make-positive-data (n &key (x 100) (y 100) (z 100))
  (make-array (list n 3) :initial-contents
         (loop for i from 1 to n
               collect (list (abs (- (/ x 2) (random x)))
                             (abs (- (/ y 2) (random y)))
                             (abs (- (/ z 2) (random z)))))))

(defun make-structured-data (n &key (x 50) (y 50) (z 50))
  (make-array (list n 3) :initial-contents
         (loop for i from 1 to n
               collect (list (round (* x (cos (/ (* 2 pi i) n))))
                             (round (* y (sin (/ (* 4 pi i) n))))
                             (round (* z (cos (/ (* 20 pi i) n))))))))

(defun make-data (n &key (x 100) (y 100) (z 100))
  (let* ((a (+ 2 (random (- 200 2)))) (b (+ 2 (random (- 200 2)))) (c (min a b)) (d (max a b)))
    (make-array (list n 3) :initial-contents
                (loop for i from 1 to n
                      collect (list (round (if (< i c) (+ (/ x 4) (random (/ x 4)))
                                        (- (random (/ x 4)) (* x .75))))
                                    (round (if (< i d) (+ (/ x 4) (random (/ y 4)))
                                        (- (random (/ x 4)) (* y .75))))
                                   (round (- (/ z 2) (random b))))))))

(defun make-spherical-data (n r)
  (make-array (list n 3) :initial-contents
              (loop for i from 1 to n
                    collect (let ((theta (- (* (random 1000) (/ pi 1000)) (/ pi 2)))
                                  (alpha (* (random 1000) (/ pi 500))))
                              (list
                               (round (* r (cos theta) (cos alpha)))
                               (round (* r (cos theta) (sin alpha)))
                               (round (* r (sin theta))))))))

(defun make-half-spherical-data (n r)
  (make-array (list n 3) :initial-contents
              (loop for i from 1 to n
                    collect (let ((theta (* (random 1000) (/ pi 1000)))
                                  (alpha (* (random 1000) (/ pi 500))))
                              (list
                               (round (* r (cos theta) (cos alpha)))
                               (round (* r (cos theta) (sin alpha)))
                               (round (* r (sin theta))))))))

(defun make-cube-data (n l)
  (make-array (list n 3) :initial-contents
              (loop for i from 1 to n
                    collect (let ((choice (random 3))
                                  (c1 (random l))
                                  (c2 (random l))
                                  (c3 (random l)))
                              (list
                               (if (eq choice 0) (* (random 2) l)
                                   c1)
                               (if (eq choice 1) (* (random 2) l)
                                   c2)
                               (if (eq choice 2) (* (random 2) l)
                                   c3))))))

(defun make-data-on-axis (n x y z)
  (align-z-axis-to-axis 
   (make-array (list n 3) :initial-contents
               (loop for i from 1 to n
                     collect (list 0 0 (random (round (sqrt (+ (* x x) (* y y) (* z z))))))))
   x y z))
  