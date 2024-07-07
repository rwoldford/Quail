(in-package :q)

(defun dpbdi-hook (abd lda n m det)
  ;; (declare (type (simple-array double-float (*)) det))
  (declare (type fixnum m))
  (declare (type fixnum n))
  (declare (type fixnum lda))
  ;; (declare (type (simple-array double-float (* *)) abd))
  (prog ((s 0.0d0) (i 0))
        (declare (type fixnum i))
        (declare (type double-float s))
        ;; 
        ;;      dpbdi computes the determinant
        ;;      of a double precision symmetric positive definite band matrix
        ;;      using the factors computed by dpbco or dpbfa.
        ;;      if the inverse is needed, use dpbsl  n  times.
        ;; 
        ;;      on entry
        ;; 
        ;;         abd     double precision(lda, n)
        ;;                 the output from dpbco or dpbfa.
        ;; 
        ;;         lda     integer
        ;;                 the leading dimension of the array  abd .
        ;; 
        ;;         n       integer
        ;;                 the order of the matrix  a .
        ;; 
        ;;         m       integer
        ;;                 the number of diagonals above the main diagonal.
        ;; 
        ;;      on return
        ;; 
        ;;         det     double precision(2)
        ;;                 determinant of original matrix in the form
        ;;                 determinant = det(1) * 10.0**det(2)
        ;;                 with  1.0 .le. det(1) .lt. 10.0
        ;;                 or  det(1) .eq. 0.0 .
        ;; 
        ;;      linpack.  this version dated 08/14/78 .
        ;;      cleve moler, university of new mexico, argonne national lab.
        ;; 
        ;;      subroutines and functions
        ;; 
        ;; 
        ;;      internal variables
        ;; 
        ;; 
        ;;      compute determinant
        ;; 
        (setf (fref det 1) 1.0)
        (setf (fref det 2) 0.0)
        (setf s 10.0d0)
        (fdo (i 1 (1+ i))
             ((> i n) nil)
             (tagbody (setf (fref det 1)
                            (* (expt (fref abd (1+ m) i) 2) (fref det 1)))
                      ;;      ...exit
                      (if (= (fref det 1) 0.0) (go label60))
              label10 (if (>= (fref det 1) 1.0) (go label20))
                      (setf (fref det 1) (* s (fref det 1)))
                      (setf (fref det 2) (- (fref det 2) 1.0))
                      (go label10)
              label20
              label30 (if (< (fref det 1) s) (go label40))
                      (setf (fref det 1) (f2cl/ (fref det 1) s))
                      (setf (fref det 2) (+ (fref det 2) 1.0))
                      (go label30)
              label40))
   label60 (return (values abd lda n m det))))

