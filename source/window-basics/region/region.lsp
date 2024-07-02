;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               region.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;
;;;  Authors:
;;;     C.B. Hurley 1989-1991
;;;     J.A. McDonald 1988-89
;;;     R.W. Oldford 1989-1991
;;;     J.O. Pedersen 1988-89
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)


(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(make-region copy-region region-p region-left
           region-bottom region-width region-height region-right
           region-top region-bounds subregion-p intersect-regions inside-p
           same-region-p position-in-region-p)))
;;;============================================================
;;; Regionswidth
;;;============================================================
;;; changed representation of regions from lists to vectors, jam, 11-17-88.
;;; --makes it possible to implement some graphics operations
;;;   on the CM, because you can have array valued pvars,
;;;   but not cons valued pvars.

(defmacro make-region (left bottom width height)
  `(vector ,left ,bottom ,width ,height))

(defmacro copy-region (region) `(copy-seq ,region))

(defun region-p (object) (and (vectorp object) (= (length object) 4)))

;;;------------------------------------------------------------

(defmacro region-left   (r) `(aref ,r 0))
(defmacro region-bottom (r) `(aref ,r 1))
(defmacro region-width  (r) `(aref ,r 2))
(defmacro region-height (r) `(aref ,r 3))
(defun region-right (r) (+ -1 (region-left   r) (region-width  r)))
(defun region-top   (r) (+ -1 (region-bottom r) (region-height r)))

(defun region-bounds (region)
  (values (region-left region)
          (region-right region)
          (region-bottom region)
          (region-top region)))

;;;------------------------------------------------------------

(defun subregion-p (larger smaller)
  (let ((large-left (region-left larger))
	(large-bottom (region-bottom larger))
	(small-left (region-left smaller))
	(small-bottom (region-bottom smaller)))
    (and (<= large-bottom
	     small-bottom
	     (+ small-bottom (region-height smaller))
	     (+ large-bottom (region-height larger)))
	 (<= large-left
	     small-left
	     (+ small-left (region-width smaller))
	     (+ large-left (region-width larger))))))

(defun intersect-regions (&rest regions)
  (assert (not (null regions)))
  (let ((left   most-negative-fixnum)
	(bottom most-negative-fixnum)
        (right  most-positive-fixnum)
	(top most-positive-fixnum)
	)
    (dolist (region regions)
      (let ((r-left   (region-left   region))
	    (r-bottom (region-bottom region))
	    (r-right  (region-right  region))
	    (r-top (region-top region)))
	(when (> r-left   left)   (setq left   r-left))
	(when (> r-bottom bottom) (setq bottom r-bottom))
	(when (< r-right  right)  (setq right  r-right))
	(when (< r-top top) (setq top r-top))))
    (make-region left bottom (- right left -1) (- top bottom -1))))

(defun inside-p (region position)
  (let ((left (region-left region))
	(bottom (region-bottom region)))
    (and
      (<= left   (position-x position) (+ left   (region-width region)))
      (<= bottom (position-y position) (+ bottom (region-height region))))))


(defun same-region-p (region-1 region-2)
  "Test whether two regions have the same coordinates."
  (and
   (= (region-bottom region-1) (region-bottom region-2))
   (= (region-left region-1) (region-left region-2))
   (= (region-width region-1) (region-width region-2))
   (= (region-height region-1) (region-height region-2))))


(defun position-in-region-p (position region)
  "Returns T if the position is inside the region."
  (let ((x (position-x position))
        (y (position-y position)))
    (and (<= (region-left region) x)
         (> (+ (region-left region) (region-width region)) x)
         (<= (region-bottom region) y)
         (> (+ (region-bottom region) (region-height region)) y))))
