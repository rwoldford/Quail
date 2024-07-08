;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               data-prob.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1993 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1993
;;;
;;;--------------------------------------------------------------------------------
;;; 

(in-package :quail)


(defmethod-multi lower-bound-of
  ((distribution  ((eql -infinity) (eql +infinity)
                   number sequence array dimensioned-ref-object)))
  (min distribution))


(defmethod-multi upper-bound-of
  ((distribution  ((eql -infinity) (eql +infinity)
                   number sequence array dimensioned-ref-object)))
  (max distribution))

(defmethod-multi cdf-at ((distribution ((eql -infinity) (eql +infinity)
                                        sequence array dimensioned-ref-object))
                         (value number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  (with-CL-functions (/ float <= - )
    (let* ((sorted-x (sort (sel distribution) #'CL:<))
           (i 0)
           (n (number-of-elements sorted-x)))
      ; (loop for j from 0 to (- n 1)
      ;     while (<= (row-major-eref sorted-x j) value) do
      (do ((j 0 (incf j)))
          ((or (= j n) (> (row-major-eref sorted-x j) value)))
        (incf i))
      
      (float (/ i n)))))

(defmethod-multi cdf-at :around ((distribution ((eql -infinity) (eql +infinity)
                                                sequence array dimensioned-ref-object))
                                 (value (sequence array dimensioned-ref-object)))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  (let* ((result (make-dimensioned-result (dimensions-of value) value))
         (n-result (number-of-elements value))
         (sorted-x (sort (sel distribution) #'CL:<))
         (n (number-of-elements sorted-x)))
    (with-CL-functions (/ float <= - )
      ; (loop for i from 0 to (- n-result 1) do
      (do ((i 0 (incf i)))
          ((= i n-result))
        (let ((index 0)
              (this-val (column-major-eref value i)))
          ; (loop for j from 0 to (- n 1)
          ;     while (<= (row-major-eref sorted-x j) this-val) do
          (do ((j 0 (incf j)))
              ((or (= j n) (> (row-major-eref sorted-x j) this-val)))
            (incf index))
          (setf (column-major-eref result i)
                (float (/ index n)))))
      result)))

(defmethod-multi pdf-at ((distribution ((eql -infinity) (eql +infinity)
                                        sequence array dimensioned-ref-object))
                         (value number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  (let* ((sorted-x (sort (sel distribution) #'CL:<))
         (i 0)
         (n (number-of-elements sorted-x)))
    (with-CL-functions (/ float <= - =)
      (loop for j from 0 to (- n 1)
            when (= (row-major-eref sorted-x j) value)
            do (incf i))
      (float (/ i n)))))

(defmethod-multi pdf-at :around ((distribution ((eql -infinity) (eql +infinity)
                                                sequence array dimensioned-ref-object))
                                 (value (sequence array dimensioned-ref-object)))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  (let* ((result (make-dimensioned-result (dimensions-of value) value))
         (n-result (number-of-elements value))
         (sorted-x (sort (sel distribution) #'cl::<))
         (n (number-of-elements sorted-x)))
    (with-CL-functions (- = / float)
      ; (loop for i from 0 to (- n-result 1) do
      (do ((i 0 (incf i)))
          ((= i n-result))
        (let ((index 0)
              (this-val (column-major-eref value i)))
          (loop for j from 0 to (- n 1)
                when (= (row-major-eref sorted-x j) this-val)
                do (incf index))
          (setf (column-major-eref result i)
                (float (/ index n)))))
      result)))


(defmethod-multi quantile-at ((distribution ((eql -infinity)
                                             (eql +infinity)
                                             (eql NaN)
                                             number))
                              (value number) &key start)
  (declare (ignorable start value)) ;(declare (ignore start value)) ; 31JUL2023
  distribution)

(defmethod-multi quantile-at ((distribution (sequence array dimensioned-ref-object))
                              (value number) &key start)
  (declare (ignore start)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  (let* ((sorted-x (sort (sel distribution) #'qk::ext_<))
         (n (number-of-elements sorted-x))
         (n*val (* (- n 1) value))
         index frac)
    (multiple-value-setq (index frac) (truncate n*val))
    (if (zerop frac)
      (row-major-eref sorted-x index)
      (qk::ext_+ 
       (qk::ext_*
        (cl::- 1.0 value)
        (row-major-eref sorted-x index))
       (qk::ext_*
        value (row-major-eref sorted-x
                             (cl::+ index 1)))))))


(defmethod-multi quantile-at ((distribution ((eql -infinity)
                                             (eql +infinity)
                                             (eql NaN)
                                             number))
                              (value (sequence array dimensioned-ref-object))
                              &key start)
  (declare (ignore start))
  (let* ((result (make-dimensioned-result (dimensions-of value) value))
         (n (number-of-elements result)))
    ; (loop for i from 0 to (- n 1) do
    (do ((i 0 (incf i)))
        ((= i n))
      (setf (row-major-eref result i)
            (quantile-at distribution
                         (row-major-eref value i))))
    result))

(defmethod-multi quantile-at ((distribution (sequence
                                             array
                                             dimensioned-ref-object))
                              (value (sequence
                                      array
                                      dimensioned-ref-object))
                              &key start)
  (declare (ignore start)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (let* ((result (make-dimensioned-result (dimensions-of value) value))
         (n-result (number-of-elements result))
         (sorted-x (sort (sel distribution) #'qk::ext_<))
         (n (number-of-elements sorted-x))
         val n*val index frac)
    ; (loop for i from 0 to (- n-result 1) do
    (do ((i 0 (incf i)))
        ((= i n-result))
      (setf val (row-major-eref value i))
      (setf n*val (* (- n 1) val))
      (multiple-value-setq (index frac) (truncate n*val))
      (setf (row-major-eref result i)
            (if (zerop frac)
              (row-major-eref sorted-x index)
              (qk::ext_+ 
               (qk::ext_*
                (cl::- 1.0 val)
                (row-major-eref sorted-x index))
               (qk::ext_*
                val (row-major-eref sorted-x
                                    (cl::+ index 1)))))))
    result))


(defmethod-multi random-value ((distribution ((eql -infinity)
                                              (eql +infinity)
                                              (eql NaN)
                                              number))
                               &optional (n 1))
  
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (array (list distribution) :dimensions (list n))
  )

(defmethod-multi random-value ((distribution (sequence array dimensioned-ref-object))
                               &optional (n 1))
  "Uses a generalized probability integral transform from a uniform (0,1).~
   Does not depend on quantile-at."
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (let* ((proportions (random-uniform :n n))
         (result (make-dimensioned-result
                  (dimensions-of proportions)
                  distribution))
         (sorted-x (sort (sel distribution) #'qk::ext_<))
         (big-n (number-of-elements sorted-x))
         )
    ; (loop for index from 0 to (- n 1) do
    (do ((index 0 (incf index)))
        ((= index n))
      (let ((i 0)
            (n*val (* big-n (row-major-eref proportions index)))
            )
        (loop for jump from 1 to big-n
              while (> n*val jump)
              do (incf i))
        (setf (row-major-eref result index)
              (row-major-eref sorted-x i))))
             result))
