(in-package :wb)
;;; The following lines are just for test purposes, and are consequently 
;;;  commented out
#|
(setf bm (make-box-bitmap (- (canvas-width c) (mod (canvas-width c) 8))
                          (canvas-height c)))

(setf bm (make-bitmap :width (1+ (- (canvas-width c) (mod (canvas-width c) 8)))
                      :height (1+ (canvas-height c))
                      :raw-bitmap
                      (let ((bm (qd::make-bitmap
                                   0 0 
                                   (- (canvas-width c) (mod (canvas-width c) 8))
                                   (canvas-height c))))
                        (with-bitmap-as-window bm w)
                        bm)
                        ))
|#

(defun plot-points-on-bitmap (array bm)
  "Plot points onto a bitmap by putting bytes into memory. This can be VERY~
   DANGEROUS if you miss. points outside the bitmap's ploting region should~
   be caught by the mod function, but will wrap around. A bitmap with a~
   width divisible by 8 is required, as each byte is 8 adjacent points.~
   The array must have observations in the rows, and the first two~
   columns are plotted."
  (let* ((w (rref (bitmap-raw-bitmap bm) bitmap.rowbytes))
         (h (bitmap-height bm))
         (hminus2 (- h 2))
         (max (1- (* w h)))
         (ptr (rref (bitmap-raw-bitmap bm) bitmap.baseaddr))
         x number offset)
    (loop for i from 0 to (- (first (array-dimensions array)) 1)
          do (progn 
               (setf x (aref array i 0))
               (setf number (choose-number (mod x 8)))
               (setf offset (mod (+  (* (- hminus2 (aref array i 1)) w)
                                     (ash x -3)) max))
               ;(print (list "plot" (aref array i 0) (aref array i 1) w h ptr number offset max))
               (%put-byte ptr (logior (%get-unsigned-byte ptr offset) number) offset)
               ))))

(defun erase-points-on-bitmap (array bm)
  "erase points from a bitmap using a simiar method to plot-points-on-bitmap~
   The ENTIRE BYTE with the point is erased for added speed. The array must~
   have observations in the rows, and the first two columns are plotted."
  (let* ((w (rref (bitmap-raw-bitmap bm) bitmap.rowbytes))
         (h (bitmap-height bm))
         (hminus2 (- h 2))
         (max (1- (* w h)))
         (ptr (rref (bitmap-raw-bitmap bm) bitmap.baseaddr))
         x offset)
    (loop for i from 0 to (- (first (array-dimensions array)) 1)
          do (progn 
               (setf x (aref array i 0))
               (setf offset (mod (+  (* (- hminus2 (aref array i 1)) w)
                                     (ash x -3)) max))
               ;(print (list "erase" (aref array i 0) (aref array i 1) w h ptr 0 offset))
               (%put-byte ptr 0 offset)
               ))))

(defun choose-number (n)
  "the fastest way to calclate the byte representing a point in the ith~
   position. This is used by plot-points-on-bitmap"
  (cond
   ((eq n 0) 128)
   ((eq n 1) 64)
   ((eq n 2) 32)
   ((eq n 3) 16)
   ((eq n 4) 8)
   ((eq n 5) 4)
   ((eq n 6) 2)
   (t 1)))