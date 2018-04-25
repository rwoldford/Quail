;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               surface-update.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c)  1992
;;;                Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;
;;;  Authors:
;;;     P. Poirier 1992
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------

(in-package :window-basics)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '()))

(defun update-surface-data (x y z a xi yi old-x old-y ziv ax ay col dx dy dz  mx my
                      range ncol)
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline + * - aref truncate float))
  (let ((cfx (float (/ (min mx my) (* 2 range))))
        k
        minz
        (mz (make-array (list 4) :element-type 'single-float
             :initial-element 0.0))
        (myi (make-array (list 4) :element-type 'integer
             :initial-element 0))
        (cd (truncate (/ ncol 2)))
        (dmx (truncate (/ mx 2)))
        (dmy (truncate (/ my 2))))
    
    
    ; (loop for i from 0 to (- dx 1) do
    (do ((i 0 (incf i)))
        ((= i dx))
      (setf (aref ax i) (* (aref a 0) (aref x i)))
      (setf (aref ax (+ dx i)) (* (aref a 3) (aref x i))))
    ; (loop for j from 0 to (- dy 1) do
    (do ((j 0 (incf j)))
        ((= j dy))
      (setf (aref ay j) (* (aref a 1) (aref y j)))
      (setf (aref ay (+ dy j)) (* (aref a 4) (aref y j))))
    (setf k 0)
    ; (loop for i from 0 to (- dx 1) do
    (do ((i 0 (incf i)))
        ((= i dx))
      ; (loop for j from 0 to (- dy 1) do
      (do ((j 0 (incf j)))
          ((= j dy))
        (setf (aref old-x k) (aref xi k))
        (setf (aref xi k)
              (+ (round
                  (*
                   (+
                    (aref ax i)
                    (aref ay j)
                    (* (aref a 2) (aref z k)))
                   cfx))
                 dmx))
        (setf (aref old-y k) (aref yi k))
        (setf (aref yi k)
              (+ (round
                  (*
                   (+
                    (aref ax (+ dx i))
                    (aref ay (+ dy j))
                    (* (aref a 5) (aref z k)))
                   cfx))
                 dmy))
        (incf k)))
    (setf (aref mz 0)
          (+ (* (aref a 6) (aref x 0)) (* (aref a 7) (aref y 0))))
    (setf (aref mz 1)
          (+ (* (aref a 6) (aref x 0))
             (* (aref a 7) (aref y (- dy 1)))))
    (setf (aref mz 2)
          (+ (* (aref a 6) (aref x (- dx 1)))
             (* (aref a 7) (aref y (- dy 1)))))
    (setf (aref mz 3)
          (+ (* (aref a 6) (aref x (- dx 1)))
             (* (aref a 7) (aref y 0))))
    (setf (aref myi 0) (aref yi 0))
    (setf (aref myi 1) (aref yi (- dy 1)))
    (setf (aref myi 2) (aref yi (- dz 1)))
    (setf (aref myi 3) (aref yi (- dz dy)))
    (setf minz 0)
    ; (loop for i from 1 to 3 do
    (do ((i 1 (incf i)))
        ((> i 3))
      (if (or (> (aref mz minz) (aref mz i))
              (and (= (aref mz minz) (aref mz i))
                   (< (aref myi minz) (aref myi i))))
        (setf minz i)))
    (setf (aref col 0) (float (/ (* (- cd) (aref mz minz)) range)))
    (cond 
     ((= minz 0) 
      (cond ((or (> (aref mz 1) (aref mz 3))
                 (and (= (aref mz 1) (aref mz 3))
                      (< (aref myi 1) (aref myi 3))))
             (setf (aref col 1)
                   (+ (aref col 0)
                      (float (/ (* cd (aref mz 3)) range))))
             (setf (aref ziv 0) dx)
             (setf (aref ziv 1) dy)
             (setf (aref ziv 2) dy)
             (setf (aref ziv 3) 1)
             (setf (aref ziv 4) 0))
            (t
             (setf (aref col 1)
                   (+ (aref col 0)
                      (float (/ (* cd (aref mz 1)) range))))
             (setf (aref ziv 0) dy)
             (setf (aref ziv 1) dx)
             (setf (aref ziv 2) 1)
             (setf (aref ziv 3) dy)
             (setf (aref ziv 4) 0))))
     ((= minz 1) 
      (cond ((or (> (aref mz 0) (aref mz 2))
                 (and (= (aref mz 0) (aref mz 2))
                      (< (aref myi 0) (aref myi 2))))
             (setf (aref col 1)
                   (+ (aref col 0)
                      (float (/ (* cd (aref mz 2)) range))))
             (setf (aref ziv 0) dx)
             (setf (aref ziv 1) dy)
             (setf (aref ziv 2) dy)
             (setf (aref ziv 3) -1)
             (setf (aref ziv 4) (- dy 1)))
            (t
             (setf (aref col 1)
                   (+ (aref col 0)
                      (float (/ (* cd (aref mz 0)) range))))
             (setf (aref ziv 0) dy)
             (setf (aref ziv 1) dx)
             (setf (aref ziv 2) -1)
             (setf (aref ziv 3) dy)
             (setf (aref ziv 4) (- dy 1)))))
     ((= minz 2) 
      (cond ((or (> (aref mz 3) (aref mz 1))
                 (and (= (aref mz 3) (aref mz 1))
                      (< (aref myi 3) (aref myi 1))))
             (setf (aref col 1)
                   (+ (aref col 0)
                      (float (/ (* cd (aref mz 1)) range))))
             (setf (aref ziv 0) dx)
             (setf (aref ziv 1) dy)
             (setf (aref ziv 2) (- dy))
             (setf (aref ziv 3) -1)
             (setf (aref ziv 4) (- dz 1)))
            (t
             (setf (aref col 1)
                   (+ (aref col 0)
                      (float (/ (* cd (aref mz 3)) range))))
             (setf (aref ziv 0) dy)
             (setf (aref ziv 1) dx)
             (setf (aref ziv 2) -1)
             (setf (aref ziv 3) (- dy))
             (setf (aref ziv 4) (- dz 1)))))
     ((= minz 3) 
      (cond ((or (> (aref mz 2) (aref mz 0))
                 (and (= (aref mz 2) (aref mz 0))
                      (< (aref myi 2) (aref myi 0))))
             (setf (aref col 1)
                   (+ (aref col 0)
                      (float (/ (* cd (aref mz 0)) range))))
             (setf (aref ziv 0) dx)
             (setf (aref ziv 1) dy)
             (setf (aref ziv 2) (- dy))
             (setf (aref ziv 3) 1)
             (setf (aref ziv 4) (- dz dy)))
            (t
             (setf (aref col 1)
                   (+ (aref col 0)
                      (float (/ (* cd (aref mz 2)) range))))
             (setf (aref ziv 0) dy)
             (setf (aref ziv 1) dx)
             (setf (aref ziv 2) 1)
             (setf (aref ziv 3) (- dy))
             (setf (aref ziv 4) (- dz dy))))))
    (setf (aref col 2)
          (+ (aref col 0) (aref col 0) (- (aref col 1))))
    )
  )
