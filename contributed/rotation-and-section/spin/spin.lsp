(in-package wb)

(defun make-data (n &key (x 100) (y 100) (z 100))
  (make-array (list n 3) :initial-contents
         (loop for i from 1 to n
               collect (list (- (/ x 2) (random x))
                             (- (/ y 2) (random y))
                             (- (/ z 2) (random z))))))

(defun make-structured-data (n &key (x 50) (y 50) (z 50))
  (make-array (list n 3) :initial-contents
         (loop for i from 1 to n
               collect (list (* x (cos (/ (* 2 pi i) n)))
                             (* y (sin (/ (* 4 pi i) n)))
                             (* z (cos (/ (* 20 pi i) n)))))))

(defun make-data (n &key (x 100) (y 100) (z 100))
  (let* ((a (+ 2 (random (- n 2)))) (b (+ 2 (random (- n 2)))) (c (min a b)) (d (max a b)))
    (make-array (list n 3) :initial-contents
                (loop for i from 1 to n
                      collect (list (if (< i c) (+ (/ x 4) (random (/ x 4)))
                                        (- (random (/ x 4)) (* x .75)))
                                    (if (< i d) (+ (/ x 4) (random (/ y 4)))
                                        (- (random (/ x 4)) (* y .75)))
                                    (- (/ z 2) (random b)))))))

(defun zrot (a angle)
  (let ((new-array (make-array (array-dimensions a))))
    (loop for i from 0 to (- (first (array-dimensions a)) 1)
          do (let ((x (aref a i 0))
                   (y (aref a i 1))
                   (c (cos angle))
                   (s (sin angle)))
               (setf (aref new-array i 0)
                     (- (* x c) (* y s)))
               (setf (aref new-array i 1)
                     (+ (* y c) (* x s)))))
    new-array))

(defun yrot (a angle)
  (let ((new-array (make-array (array-dimensions a))))
    (loop for i from 0 to (- (first (array-dimensions a)) 1)
          do (let ((x (aref a i 0))
                   (z (aref a i 2))
                   (c (cos angle))
                   (s (sin angle)))
               (setf (aref new-array i 0)
                     (+ (* x c) (* z s)))
               (setf (aref new-array i 1)
                     (aref a i 1))))
    new-array))

(defun xrot (a angle)
  (let ((new-array (make-array (array-dimensions a))))
    (loop for i from 0 to (- (first (array-dimensions a)) 1)
          do (let ((y (aref a i 1))
                   (z (aref a i 2))
                   (c (cos angle))
                   (s (sin angle)))
               (setf (aref new-array i 0)
                     (aref a i 0))
               (setf (aref new-array i 1)
                     (- (* y c) (* z s)))))
    new-array))

(defun make-current-trans (angle direction)
  (make-array '(3 3)
              :initial-contents
              (cond
               ((eq direction :z)
                (list (list (cos angle) (sin angle) 0)
                        (list (- (sin angle)) (cos angle) 0)
                        (list 0 0 1)))
               ((eq direction :y)
                (list (list (cos angle) 0 (- (sin angle)))
                        (list 0 1 0)
                        (list (sin angle) 0 (cos angle))))
               ((eq direction :x)
                (list (list 1 0 0)
                        (list 0 (cos angle) (sin angle))
                        (list 0 (- (sin angle)) (cos angle)))))))
                  

(defun user-select-direction ()
  (let ((r (random 3)))
    (cond ((eq r 0) :x)
          ((eq r 1) :y)
          (t :x))))

(defun rotate-point-cloud (data c &key (increment (/ pi 30)))
  (let* ((temp-data data)
         (old-data temp-data)
         (new-data temp-data)
         (direction (user-select-direction))
         direction-fn
         (angle 0))
    (canvas-invert c)
    (set-canvas-pen-mode c :patxor)
    (with-focused-canvas c
      (loop for i from 0 to (- (first (array-dimensions old-data)) 1)
            do (canvas-draw-point (round (+ 100 (aref old-data i 0)))
                                  (round (+ 100 (aref old-data i 1)))
                                  c)))
    (if (listen) (read))
    (loop until (read-char-no-hang)
          do (progn
               (setf direction-fn (cond ((eq direction :x) #'xrot)
                                        ((eq direction :y) #'yrot)
                                        ((eq direction :z) #'zrot)))
               (loop until (eq (random 20) 1)
                     do (progn
                          (setf angle (+ angle increment))
                          (setf new-data (funcall direction-fn temp-data angle))
                          (with-focused-view c
                            (loop for i from 0 to (- (first (array-dimensions old-data)) 1)
                                do (progn
                                     (canvas-draw-point (round (+ 100 (aref old-data i 0)))
                                                        (round (+ 100 (aref old-data i 1)))
                                                        c)
                                     (canvas-draw-point (round (+ 100 (aref new-data i 0)))
                                                        (round (+ 100 (aref new-data i 1)))
                                                        c)
                                     ))
                           )
                          (setf old-data new-data)))
               (setf temp-data (quail-user::z.* temp-data
                                                (make-current-trans angle direction)))
               (setf angle 0)
               (setf direction (user-select-direction))
               ))
    (canvas-clear c)))


