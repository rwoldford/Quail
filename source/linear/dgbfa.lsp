(in-package :q)

(defun dgbfa-hook (abd lda n ml mu ipvt info)
  (declare (type fixnum info))
  ;; (declare (type (simple-array fixnum (*)) ipvt))
  (declare (type fixnum mu))
  (declare (type fixnum ml))
  (declare (type fixnum n))
  (declare (type fixnum lda))
  ;; (declare (type (simple-array double-float (* *)) abd))
  (prog ((t_ 0.0) (nm1 0) (mm 0) (m 0) (lm 0) (l 0) (kp1 0) (k 0)
         (j1 0) (j0 0) (jz 0) (ju 0) (j 0) (i0 0) (i 0))
        (declare (type fixnum i))
        (declare (type fixnum i0))
        (declare (type fixnum j))
        (declare (type fixnum ju))
        (declare (type fixnum jz))
        (declare (type fixnum j0))
        (declare (type fixnum j1))
        (declare (type fixnum k))
        (declare (type fixnum kp1))
        (declare (type fixnum l))
        (declare (type fixnum lm))
        (declare (type fixnum m))
        (declare (type fixnum mm))
        (declare (type fixnum nm1))
        (declare (type double-float t_))
        (declare (special *dummy_var*))
        ;; 
        ;;      dgbfa factors a double precision band matrix by elimination.
        ;; 
        ;;      dgbfa is usually called by dgbco, but it can be called
        ;;      directly with a saving in time if  rcond  is not needed.
        ;; 
        ;;      on entry
        ;; 
        ;;         abd     double precision(lda, n)
        ;;                 contains the matrix in band storage.  the columns
        ;;                 of the matrix are stored in the columns of  abd  and
        ;;                 the diagonals of the matrix are stored in rows
        ;;                 ml+1 through 2*ml+mu+1 of  abd .
        ;;                 see the comments below for details.
        ;; 
        ;;         lda     integer
        ;;                 the leading dimension of the array  abd .
        ;;                 lda must be .ge. 2*ml + mu + 1 .
        ;; 
        ;;         n       integer
        ;;                 the order of the original matrix.
        ;; 
        ;;         ml      integer
        ;;                 number of diagonals below the main diagonal.
        ;;                 0 .le. ml .lt. n .
        ;; 
        ;;         mu      integer
        ;;                 number of diagonals above the main diagonal.
        ;;                 0 .le. mu .lt. n .
        ;;                 more efficient if  ml .le. mu .
        ;;      on return
        ;; 
        ;;         abd     an upper triangular matrix in band storage and
        ;;                 the multipliers which were used to obtain it.
        ;;                 the factorization can be written  a = l*u  where
        ;;                 l  is a product of permutation and unit lower
        ;;                 triangular matrices and  u  is upper triangular.
        ;; 
        ;;         ipvt    integer(n)
        ;;                 an integer vector of pivot indices.
        ;; 
        ;;         info    integer
        ;;                 = 0  normal value.
        ;;                 = k  if  u(k,k) .eq. 0.0 .  this is not an error
        ;;                      condition for this subroutine, but it does
        ;;                      indicate that dgbsl will divide by zero if
        ;;                      called.  use  rcond  in dgbco for a reliable
        ;;                      indication of singularity.
        ;; 
        ;;      band storage
        ;; 
        ;;            if  a  is a band matrix, the following program segment
        ;;            will set up the input.
        ;; 
        ;;                    ml = (band width below the diagonal)
        ;;                    mu = (band width above the diagonal)
        ;;                    m = ml + mu + 1
        ;;                    do 20 j = 1, n
        ;;                       i1 = max0(1, j-mu)
        ;;                       i2 = min0(n, j+ml)
        ;;                       do 10 i = i1, i2
        ;;                          k = i - j + m
        ;;                          abd(k,j) = a(i,j)
        ;;                 10    continue
        ;;                 20 continue
        ;; 
        ;;            this uses rows  ml+1  through  2*ml+mu+1  of  abd .
        ;;            in addition, the first  ml  rows in  abd  are used for
        ;;            elements generated during the triangularization.
        ;;            the total number of rows needed in  abd  is  2*ml+mu+1 .
        ;;            the  ml+mu by ml+mu  upper left triangle and the
        ;;            ml by ml  lower right triangle are not referenced.
        ;; 
        ;;      linpack. this version dated 08/14/78 .
        ;;      cleve moler, university of new mexico, argonne national lab.
        ;; 
        ;;      subroutines and functions
        ;; 
        ;;      blas daxpy,dscal,idamax
        ;;      fortran max0,min0
        ;; 
        ;;      internal variables
        ;; 
        ;; 
        ;; 
        (setf m (+ ml mu 1))
        (setf info 0)
        ;; 
        ;;      zero initial fill-in columns
        ;; 
        (setf j0 (+ mu 2))
        (setf j1 (1- (min0 n m)))
        (if (< j1 j0) (go label30))
        (fdo (jz j0 (1+ jz))
             ((> jz j1) nil)
             (tagbody (setf i0 (- (1+ m) jz))
                      (fdo (i i0 (1+ i))
                           ((> i ml) nil)
                           (tagbody (setf (fref abd i jz) 0.0)))))
label30 (setf jz j1)
        (setf ju 0)
        ;; 
        ;;      gaussian elimination with partial pivoting
        ;; 
        (setf nm1 (1- n))
        (if (< nm1 1) (go label130))
        (fdo (k 1 (1+ k))
             ((> k nm1) nil)
             (tagbody (setf kp1 (1+ k))
                      ;; 
                      ;;         zero next fill-in column
                      ;; 
                      (setf jz (1+ jz))
                      (if (> jz n) (go label50))
                      (if (< ml 1) (go label50))
                      (fdo (i 1 (1+ i))
                           ((> i ml) nil)
                           (tagbody (setf (fref abd i jz) 0.0)))
              label50 ;; 
                      ;;         find l = pivot index
                      ;; 
                      (setf lm (min0 ml (- n k)))
                      (setf l (1- (+ (idamax (1+ lm) (fref abd m k) 1) m)))
                      (setf (fref ipvt k) (- (+ l k) m))
                      ;; 
                      ;;         zero pivot implies this column already triangularized
                      ;; 
                      (if (= (fref abd l k) 0.0) (go label100))
                      ;; 
                      ;;            interchange if necessary
                      ;; 
                      (if (= l m) (go label60))
                      (setf t_ (fref abd l k))
                      (setf (fref abd l k) (fref abd m k))
                      (setf (fref abd m k) t_)
              label60 ;; 
                      ;;            compute multipliers
                      ;; 
                      (setf t_ (f2cl/ -1.0 (fref abd m k)))
                      (dscal-hook lm t_ (vec-ref abd (1+ m) k) 1)
                      ;; 
                      ;;            row elimination with column indexing
                      ;; 
                      (setf ju (min0 (max0 ju (+ mu (fref ipvt k))) n))
                      (setf mm m)
                      (if (< ju kp1) (go label90))
                      (fdo (j kp1 (1+ j))
                           ((> j ju) nil)
                           (tagbody (setf l (1- l))
                                    (setf mm (1- mm))
                                    (setf t_ (fref abd l j))
                                    (if (= l mm) (go label70))
                                    (setf (fref abd l j) (fref abd mm j))
                                    (setf (fref abd mm j) t_)
                            label70 (daxpy-hook lm t_ (vec-ref abd (1+ m) k) 1
                                       (vec-ref abd (1+ mm) j) 1)))
              label90 (go label110)
              label100 (setf info k)
              label110))
   label130 (setf (fref ipvt n) n)
        (if (= (fref abd m n) 0.0) (setf info n))
        (return (values abd lda n ml mu ipvt info))))

