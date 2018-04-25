
(defun sort-data-by-col (array col)
  (let* ((the-list
          (loop for i from 0 to (1- (first (array-dimensions array)))
                collect (list i (aref array i col)))))
    (setf the-list (sort the-list #'< :key #'second))
    (qk::sel array (mapcar #'first the-list) :shape t)))

(defun array-mean (array col)
  (/
   (loop for i from 0 to (- (first (array-dimensions array)) 1)
         sum (aref array i col))
   (first (array-dimensions array))))

(defun array-median (array col)
  (let* ((length (first (array-dimensions array)))
         (sorted-col (sort
                      (loop for i from 0 to (1- length)
                            collect (aref array i col)) #'<)))
    (print (list length sorted-col))
    (if (oddp length)
      (elt sorted-col (/ (1- length) 2))
      (/ (+ (elt sorted-col (/ length 2))
            (elt sorted-col (1- (/ length 2))))
         2))))

(defun array-range (array col)
  (let ((min (aref array 0 col))
        (max (aref array 0 col)))
    (loop for i from 0 to (1- (first (array-dimensions array)))
          do (let ((num (aref array i col)))
               (if (> min num) (setq min num))
               (if (< man num) (setq max num))))
    (- max min)))

(defun array-variance (array col)
  (let ((mean (array-mean array col))
        (n (1- (first (array-dimensions array)))))
    (/
     (loop for i from 0 to n
          sum (expt (- (aref array i col) mean) 2))
     n)))

(defun scale-array-column! (array col scale-factor)
  (loop for i from 0 to (1- (first (array-dimensions array)))
        do (setf (aref array i col) (* (aref array i col) scale-factor))))

(defun scale-array-column-by-variance! (array col)
  (scale-array-column! array col (/ 1 (sqrt (array-variance array col)))))

(defun scale-array-columns-by-variance! (array)
  (loop for col from 0 to (1- (second (array-dimensions array)))
        do (scale-array-column! array col (/ 1 (sqrt (array-variance array col))))))
          

(defun scale-data-for-window (data size)
  "scales 3-d data so that when rotated it will always fit in a window~
   with minimum dimension SIZE"
  (let* ((dim (array-dimensions data))
         (centered-data (make-array dim))
         d)
    (copy-contents data centered-data)
    (apply-transform! (make-instance '3d-shift
                                        :x-shift (- (array-mean data 0))
                                        :y-shift (- (array-mean data 1))
                                        :z-shift (- (array-mean data 2)))
                         centered-data)
    (setf d (/ size
               (sqrt (apply #'max
                            (loop for i from 0 to (- (first dim) 1)
                                  collect (loop for j from 0 to (- (second dim) 1)
                                                sum (expt (aref centered-data i j) 2))
                                  )))
               2))
    (loop for i from 0 to (- (first dim) 1) do
          (loop for j from 0 to (- (second dim) 1) do
                (setf (aref centered-data i j) (round (* (aref centered-data i j) d)))))
      centered-data))

(defun copy-contents (source dest)
  (loop for i from 0 to (- (first (array-dimensions source)) 1)
        do (loop for j from 0 to (- (second (array-dimensions source)) 1)
                 do (setf (aref dest i j) (aref source i j)))))
