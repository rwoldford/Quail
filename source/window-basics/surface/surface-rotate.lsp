;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                              surface-rotate.lisp
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
;;;     R.W. Oldford 1992
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------

(in-package :window-basics)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '()))

(defun spin-surface-x (a cos-theta sin-theta &optional old-a)
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline CL:+ CL:* CL:- aref))
  (unless old-a
    (setf old-a (make-array (list 3) :element-type 'single-float
           :initial-element 0.0)))
  ; (loop for i from 0 to 2 do
  (do ((i 0 (incf i)))
      ((> i 2))
    (setf (aref old-a i) (aref a (CL:+ (CL:* i 3) 1)))
    )
  ; (loop for i from 0 to 2 do
  (do ((i 0 (incf i)))
      ((> i 2))
    (setf (aref a (CL:+ (CL:* i 3) 1)) 
          (CL:+ (CL:* (aref a (CL:+ (CL:* i 3) 1))
                      cos-theta)
                (CL:* (aref a (CL:+ (CL:* i 3) 2))
                      sin-theta)))
    (setf (aref a (CL:+ (CL:* i 3) 2)) 
          (CL:- (CL:* (aref a (CL:+ (CL:* i 3) 2))
                      cos-theta)
                (CL:* (aref old-a i)
                      sin-theta)))
    ))


(defun spin-surface-y (a cos-theta sin-theta &optional old-a)
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline CL:+ CL:* CL:- aref))
  (unless old-a
    (setf old-a (make-array (list 3) :element-type 'single-float
           :initial-element 0.0)))
  ; (loop for i from 0 to 2 do
  (do ((i 0 (incf i)))
      ((> i 2))
    (setf (aref old-a i) (aref a (CL:+ (CL:* i 3) 2)))
    )
  ; (loop for i from 0 to 2 do
  (do ((i 0 (incf i)))
      ((> i 2))
    (setf (aref a (CL:+ (CL:* i 3) 2)) 
          (CL:+ (CL:* (aref a (CL:+ (CL:* i 3) 2))
                      cos-theta)
                (CL:* (aref a (CL:* i 3))
                      sin-theta)))
    (setf (aref a (CL:* i 3)) 
          (CL:- (CL:* (aref a (CL:* i 3))
                      cos-theta)
                (CL:* (aref old-a i)
                      sin-theta)))
    ))

(defun spin-surface-z (a cos-theta sin-theta  &optional old-a)
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline CL:+ CL:* CL:- aref))
  (unless old-a
    (setf old-a (make-array (list 3) :element-type 'single-float
           :initial-element 0.0)))
  ; (loop for i from 0 to 2 do
  (do ((i 0 (incf i)))
      ((> i 2))
    (setf (aref old-a i) (aref a (CL:* i 3)))
    )
  ; (loop for i from 0 to 2 do
  (do ((i 0 (incf i)))
      ((> i 2))
    (setf (aref a (CL:* i 3)) 
          (CL:+ (CL:* (aref a (CL:* i 3))
                      cos-theta)
                (CL:* (aref a (CL:+ (CL:* i 3) 1))
                      sin-theta)))
    (setf (aref a (CL:+ (CL:* i 3) 1)) 
          (CL:- (CL:* (aref a (CL:+ (CL:* i 3) 1))
                      cos-theta)
                (CL:* (aref old-a i)
                      sin-theta)))
    ))

(defun spin-screen-x (a cos-theta sin-theta &optional old-a)
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline CL:+ CL:* CL:- aref))
  (unless old-a
    (setf old-a (make-array (list 3) :element-type 'single-float
           :initial-element 0.0)))
  ; (loop for i from 0 to 2 do
  (do ((i 0 (incf i)))
      ((> i 2))
    (setf (aref old-a i) (aref a (CL:+ i 3)))
    )
  ; (loop for i from 0 to 2 do
  (do ((i 0 (incf i)))
      ((> i 2))
    (setf (aref a (CL:+ i 3)) 
          (CL:- (CL:* (aref a (CL:+ i 3))
                      cos-theta)
                (CL:* (aref a (CL:+ i 6))
                      sin-theta)))
    (setf (aref a (CL:+ i 6)) 
          (CL:+ (CL:* (aref a (CL:+ i 6))
                      cos-theta)
                (CL:* (aref old-a i)
                      sin-theta)))
    ))


(defun spin-screen-y (a cos-theta sin-theta &optional old-a)
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline CL:+ CL:* CL:- aref))
  (unless old-a
    (setf old-a (make-array (list 3) :element-type 'single-float
           :initial-element 0.0)))
  ; (loop for i from 0 to 2 do
  (do ((i 0 (incf i)))
      ((> i 2))
    (setf (aref old-a i) (aref a (CL:+ i 6)))
    )
  ; (loop for i from 0 to 2 do
  (do ((i 0 (incf i)))
      ((> i 2))
    (setf (aref a (CL:+ i 6)) 
          (CL:- (CL:* (aref a (CL:+ i 6))
                      cos-theta)
                (CL:* (aref a i)
                      sin-theta)))
    (setf (aref a i) 
          (CL:+ (CL:* (aref a i)
                      cos-theta)
                (CL:* (aref old-a i)
                      sin-theta)))
    ))

(defun spin-screen-z (a cos-theta sin-theta &optional old-a)
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline CL:+ CL:* CL:- aref))
  (unless old-a
    (setf old-a (make-array (list 3) :element-type 'single-float
           :initial-element 0.0)))
  ; (loop for i from 0 to 2 do
  (do ((i 0 (incf i)))
      ((> i 2))
    (setf (aref old-a i) (aref a i))
    )
  ; (loop for i from 0 to 2 do
  (do ((i 0 (incf i)))
      ((> i 2))
    (setf (aref a i) 
          (CL:- (CL:* (aref a i)
                      cos-theta)
                (CL:* (aref a (CL:+ i 3))
                      sin-theta)))
    (setf (aref a (CL:+ i 3)) 
          (CL:+ (CL:* (aref a (CL:+ i 3))
                      cos-theta)
                (CL:* (aref old-a i)
                      sin-theta)))
    ))
    

