(in-package wb)

(defun user-select-direction (a b c)
  (let ((r (random 3)))
    (cond ((eq r 0) a)
          ((eq r 1) b)
          (t c))))


(defun rotate-point-cloud (data c
                                &key (increment (/ pi 30))
                                (plot-rgn (make-region
                                           0 0
                                           (- (canvas-width c) (mod (canvas-width c) 8))
                                           (canvas-height c))))
  "rotates a point-cloud using bitmaps. The point cloud is in array form~
   with each row an x,y,z observation. This array must be integer!!"
  (let* ((scaled-data (scale-data-for-window
                     data
                     (min (aref plot-rgn 2) (aref plot-rgn 3))))
         (old-data (make-array (array-dimensions data)))
         (new-data (make-array (array-dimensions data)))
         (location-trans (make-instance '2d-shift
                                        :x-shift (round (+ (aref plot-rgn 0) (/ (aref plot-rgn 2) 2)))
                                        :y-shift (round (+ (aref plot-rgn 1) (/ (aref plot-rgn 3) 2)))))
         (xrot (make-instance '3d-x-rotate :angle 0))
         (yrot (make-instance '3d-y-rotate :angle 0))
         (zrot (make-instance '3d-z-rotate :angle 0))
         rot
         temp
         (bm (make-bitmap :width (- (canvas-width c) (mod (canvas-width c) 8))
                      :height (canvas-height c)
                      :raw-bitmap
                      (let ((bm (qd::make-bitmap
                                   0 0 
                                   (- (canvas-width c) (mod (canvas-width c) 8) 1)
                                   (- (canvas-height c) 1))))
                        (with-bitmap-as-window bm w)
                        bm)
                        )))
    (canvas-clear c)
    ;(canvas-invert c)
    ;(set-canvas-pen-mode c :patxor)
    (setf rot (user-select-direction xrot yrot zrot))
    (copy-contents scaled-data old-data)
    (apply-transform! location-trans old-data)
    (plot-points-on-bitmap old-data bm)
    (canvas-bitblt bm c)
    (erase-points-on-bitmap old-data bm)
    (if (listen) (read))
    (loop until (read-char-no-hang)
          do (progn
               (loop until (or (eq (random 20) 1) (read-char-no-hang))
                     do (progn
                          (setf (angle-of rot) (+ (angle-of rot) increment))
                          (copy-contents scaled-data new-data)
                          (apply-transform! rot new-data)
                          (apply-transform! location-trans new-data)
                          (plot-points-on-bitmap new-data bm)
                          (canvas-bitblt bm c)
                          (erase-points-on-bitmap new-data bm)
                          (setf temp old-data)
                          (setf old-data new-data)
                          (setf new-data temp)))
               (apply-transform! rot scaled-data)
               (setf rot (user-select-direction xrot yrot zrot))
               (setf (angle-of rot) 0)
               ))))