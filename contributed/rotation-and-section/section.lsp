(in-package wb)

(defun align-z-axis-to-axis (data x y z)
  (let* ((length (sqrt (+ (* x x) (* y y) (* z z))))
         (phi (acos (/ z length)))
         (theta (if (eq (round (* phi 1000)) 0)
                  0 (asin (/ y (* length (sin phi))))))
         (zrot (make-instance '3d-z-rotate :angle theta))
         (const (/ (* x x) (+ (* x x) (* y y))))
         (axisrot (make-instance '3d-axis-rotate
                                 :x-component (sqrt (- 1 const))
                                 :y-component (sqrt const)
                                 :z-component 0
                                 :angle phi)))
    (print (list "phi" phi "theta" theta))
    (apply-transform axisrot (apply-transform zrot data))))

(defun sort-data-by-z (array)
  (let* ((the-list
          (loop for i from 0 to (1- (first (array-dimensions array)))
                collect (list i (aref array i 2)))))
    (setf the-list (sort the-list #'< :key #'second))
    (qk::sel array (mapcar #'first the-list) :shape t)))

(defun z-section (data canvas &key (steps 10)
                                  (plot-rgn (make-region
                                             0 0
                                             (canvas-width canvas)
                                             (canvas-height canvas))))
  (let* ((temp-data (scale-data-for-window (sort-data-by-z data)
                                           (min (aref plot-rgn 2)
                                                (aref plot-rgn 3))))
         (last (1- (first (array-dimensions temp-data))))
         (min (aref temp-data 0 2))
         (max (aref temp-data last 2))
         (inc (/ (- max min) steps))
         oldmin oldmax
         (newmin 0)
         (newmax (loop for i from 0 to last
                       while (< (aref temp-data i 2) (+ min inc))
                       finally (return i))))
    ;(canvas-clear canvas)
    ;(canvas-invert canvas)
    (set-canvas-pen-mode canvas :patxor)
    (apply-transform! (make-instance '2d-shift
                         :x-shift (round (+ (aref plot-rgn 0)
                                            (/ (aref plot-rgn 2) 2)))
                         :y-shift (round (+ (aref plot-rgn 1)
                                            (/ (aref plot-rgn 3) 2))))
                      temp-data)
    (with-focused-canvas canvas
      (loop for i from 2 to steps do
            (progn 
              (loop for j from newmin to (1- newmax) do
                    (canvas-draw-point
                     (aref temp-data j 0)
                     (aref temp-data j 1)
                     canvas))
              (setf oldmin newmin oldmax newmax)
              (setf newmin (1+ oldmax)
                    newmax (loop for j from newmin to last
                                 while (< (aref temp-data j 2)
                                          (+ min (* i inc)))
                                 finally (return j)))
              (loop for i from 0 to 300000 do ())
              (loop for j from oldmin to (1- oldmax) do
                    (canvas-draw-point
                     (aref temp-data j 0)
                     (aref temp-data j 1)
                     canvas)))))))

(defun z-section-persp (data canvas &key (steps 30)
                                  (plot-rgn (make-region
                                             0 0
                                             (canvas-width canvas)
                                             (canvas-height canvas))))
  (let* ((temp-data (scale-data-for-window (sort-data-by-z data)
                                           (min (aref plot-rgn 2)
                                                (aref plot-rgn 3))))
         (last (1- (first (array-dimensions temp-data))))
         (min (aref temp-data 0 2))
         (max (aref temp-data last 2))
         (inc (/ (- max min) steps))
         (l0 0) (l1 0) (l2 0) (l3 0)
         (l4 (loop for i from 0 to last
                   while (< (aref temp-data i 2) (+ min inc))
                   finally (return i))))
    ;(canvas-clear canvas)
    ;(canvas-invert canvas)
    (set-canvas-pen-mode canvas :patxor)
    (apply-transform! (make-instance '2d-shift
                         :x-shift (round (+ (aref plot-rgn 0)
                                            (/ (aref plot-rgn 2) 2)))
                         :y-shift (round (+ (aref plot-rgn 1)
                                            (/ (aref plot-rgn 3) 2))))
                      temp-data)
    (with-focused-canvas canvas
      (loop for i from 2 to steps do
            (progn 
              (loop for j from l1 to (1- l2) do
                    (canvas-draw-point
                     (aref temp-data j 0)
                     (aref temp-data j 1)
                     canvas))
              (loop for j from l2 to (1- l3) do
                    (canvas-draw-filled-circle
                     (aref temp-data j 0)
                     (aref temp-data j 1)
                     1
                     canvas))
              (loop for j from l3 to (1- l4) do
                    (canvas-draw-filled-circle
                     (aref temp-data j 0)
                     (aref temp-data j 1)
                     2
                     canvas))
              (setf l0 l1
                    l1 l2
                    l2 l3
                    l3 l4
                    l4 (loop for j from l3 to last
                             while (< (aref temp-data j 2)
                                      (+ min (* i inc)))
                             finally (return j)))
              ;(print (list l0 l1 l2 l3 l4))
              (loop for wait from 0 to 200000 do ())
              (loop for j from l0 to (1- l1) do
                    (canvas-draw-point
                     (aref temp-data j 0)
                     (aref temp-data j 1)
                     canvas))
              (loop for j from l1 to (1- l2) do
                    (canvas-draw-filled-circle
                     (aref temp-data j 0)
                     (aref temp-data j 1)
                     1
                     canvas))
              (loop for j from l2 to (1- l3) do
                    (canvas-draw-filled-circle
                     (aref temp-data j 0)
                     (aref temp-data j 1)
                     2
                     canvas)))))))