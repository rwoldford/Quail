(in-package :q)

(defun dgbdi-hook (abd lda n ml mu ipvt det)
  ;; (declare (type (simple-array double-float (*)) det))
  ;; (declare (type (simple-array fixnum (*)) ipvt))
  (declare (type fixnum mu))
  (declare (type fixnum ml))
  (declare (type fixnum n))
  (declare (type fixnum lda))
  ;; (declare (type (simple-array double-float (* *)) abd))
  (prog ((m 0) (i 0))
        (declare (type fixnum i))
        (declare (type fixnum m))
        ;; 
        ;;      dgbdi computes the determinant of a band matrix
        ;;      using the factors computed by dgbco or dgbfa.
        ;;      if the inverse is needed, use dgbsl  n  times.
        ;; 
        ;;      on entry
        ;; 
        ;;         abd     double precision(lda, n)
        ;;                 the output from dgbco or dgbfa.
        ;; 
        ;;         lda     integer
        ;;                 the leading dimension of the array  abd .
        ;; 
        ;;         n       integer
        ;;                 the order of the original matrix.
        ;; 
        ;;         ml      integer
        ;;                 number of diagonals below the main diagonal.
        ;; 
        ;;         mu      integer
        ;;                 number of diagonals above the main diagonal.
        ;; 
        ;;         ipvt    integer(n)
        ;;                 the pivot vector from dgbco or dgbfa.
        ;; 
        ;;      on return
        ;; 
        ;;         det     double precision(2)
        ;;                 determinant of original matrix.
        ;;                 determinant = det(1) * 10.0**det(2)
        ;;                 with  1.0 .le. dabs(det(1)) .lt. 10.0
        ;;                 or  det(1) = 0.0 .
        ;; 
        ;;      linpack. this version dated 08/14/78 .
        ;;      cleve moler, university of new mexico, argonne national lab.
        ;; 
        ;;      subroutines and functions
        ;; 
        ;;      fortran dabs
        ;; 
        ;;      internal variables
        ;; 
        ;; 
        ;; 
        (setf m (+ ml mu 1))
        (setf (fref det 1) 1.0)
        (setf (fref det 2) 0.0)
        (fdo (i 1 (1+ i))
             ((> i n) nil)
             (tagbody (if (/= (fref ipvt i) i)
                          (setf (fref det 1) (- (fref det 1))))
                      (setf (fref det 1) (* (fref abd m i) (fref det 1)))
                      ;;      ...exit
                      (if (= (fref det 1) 0.0) (go label60))
              label10 (if (>= (dabs (fref det 1)) 1.0) (go label20))
                      (setf (fref det 1) (* 10.0 (fref det 1)))
                      (setf (fref det 2) (1- (fref det 2)))
                      (go label10)
              label20 label30 (if (< (dabs (fref det 1)) 10.0) (go label40))
                      (setf (fref det 1) (f2cl/ (fref det 1) 10.0))
                      (setf (fref det 2) (1+ (fref det 2)))
                      (go label30)
              label40))
   label60 (return (values abd lda n ml mu ipvt det))))

