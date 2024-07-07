(in-package :q)

(defun dpbco-hook (abd lda n m rcond z info)
  (declare (type fixnum info))
  ;; (declare (type (simple-array double-float (*)) z))
  (declare (type double-float rcond))
  (declare (type fixnum m))
  (declare (type fixnum n))
  (declare (type fixnum lda))
  ;; (declare (type (simple-array double-float (* *)) abd))
  (prog ((t_ 0.0d0) (wkm 0.0d0) (wk 0.0d0) (ek 0.0d0) (ynorm 0.0d0) (sm 0.0d0)
         (s 0.0d0) (anorm 0.0d0) (mu 0) (lm 0) (lb 0) (la 0) (l 0) (kp1 0) (kb 0)
         (k 0) (j2 0) (j 0) (i 0))
        (declare (type fixnum i))
        (declare (type fixnum j))
        (declare (type fixnum j2))
        (declare (type fixnum k))
        (declare (type fixnum kb))
        (declare (type fixnum kp1))
        (declare (type fixnum l))
        (declare (type fixnum la))
        (declare (type fixnum lb))
        (declare (type fixnum lm))
        (declare (type fixnum mu))
        (declare (type double-float anorm))
        (declare (type double-float s))
        (declare (type double-float sm))
        (declare (type double-float ynorm))
        (declare (type double-float ek))
        (declare (type double-float t_))
        (declare (type double-float wk))
        (declare (type double-float wkm))
        (declare (special *dummy_var*))
        ;; 
        ;;      dpbco factors a double precision symmetric positive definite
        ;;      matrix stored in band form and estimates the condition of the
        ;;      matrix.
        ;; 
        ;;      if  rcond  is not needed, dpbfa is slightly faster.
        ;;      to solve  a*x = b , follow dpbco by dpbsl.
        ;;      to compute  inverse(a)*c , follow dpbco by dpbsl.
        ;;      to compute  determinant(a) , follow dpbco by dpbdi.
        ;; 
        ;;      on entry
        ;; 
        ;;         abd     double precision(lda, n)
        ;;                 the matrix to be factored.  the columns of the upper
        ;;                 triangle are stored in the columns of abd and the
        ;;                 diagonals of the upper triangle are stored in the
        ;;                 rows of abd .  see the comments below for details.
        ;; 
        ;;         lda     integer
        ;;                 the leading dimension of the array  abd .
        ;;                 lda must be .ge. m + 1 .
        ;; 
        ;;         n       integer
        ;;                 the order of the matrix  a .
        ;; 
        ;;         m       integer
        ;;                 the number of diagonals above the main diagonal.
        ;;                 0 .le. m .lt. n .
        ;; 
        ;;      on return
        ;; 
        ;;         abd     an upper triangular matrix  r , stored in band
        ;;                 form, so that  a = trans(r)*r .
        ;;                 if  info .ne. 0 , the factorization is not complete.
        ;; 
        ;;         rcond   double precision
        ;;                 an estimate of the reciprocal condition of  a .
        ;;                 for the system  a*x = b , relative perturbations
        ;;                 in  a  and  b  of size  epsilon  may cause
        ;;                 relative perturbations in  x  of size  epsilon/rcond .
        ;;                 if  rcond  is so small that the logical expression
        ;;                            1.0 + rcond .eq. 1.0
        ;;                 is true, then  a  may be singular to working
        ;;                 precision.  in particular,  rcond  is zero  if
        ;;                 exact singularity is detected or the estimate
        ;;                 underflows.  if info .ne. 0 , rcond is unchanged.
        ;; 
        ;;         z       double precision(n)
        ;;                 a work vector whose contents are usually unimportant.
        ;;                 if  a  is singular to working precision, then  z  is
        ;;                 an approximate null vector in the sense that
        ;;                 norm(a*z) = rcond*norm(a)*norm(z) .
        ;;                 if  info .ne. 0 , z  is unchanged.
        ;; 
        ;;         info    integer
        ;;                 = 0  for normal return.
        ;;                 = k  signals an error condition.  the leading minor
        ;;                      of order  k  is not positive definite.
        ;; 
        ;;      band storage
        ;; 
        ;;            if  a  is a symmetric positive definite band matrix,
        ;;            the following program segment will set up the input.
        ;; 
        ;;                    m = (band width above diagonal)
        ;;                    do 20 j = 1, n
        ;;                       i1 = max0(1, j-m)
        ;;                       do 10 i = i1, j
        ;;                          k = i-j+m+1
        ;;                          abd(k,j) = a(i,j)
        ;;                 10    continue
        ;;                 20 continue
        ;; 
        ;;            this uses  m + 1  rows of  a , except for the  m by m
        ;;            upper left triangle, which is ignored.
        ;; 
        ;;      example..  if the original matrix is
        ;; 
        ;;            11 12 13  0  0  0
        ;;            12 22 23 24  0  0
        ;;            13 23 33 34 35  0
        ;;             0 24 34 44 45 46
        ;;             0  0 35 45 55 56
        ;;             0  0  0 46 56 66
        ;; 
        ;;      then  n = 6 , m = 2  and  abd  should contain
        ;; 
        ;;             *  * 13 24 35 46
        ;;             * 12 23 34 45 56
        ;;            11 22 33 44 55 66
        ;; 
        ;;      linpack.  this version dated 08/14/78 .
        ;;      cleve moler, university of new mexico, argonne national lab.
        ;; 
        ;;      subroutines and functions
        ;; 
        ;;      linpack dpbfa
        ;;      blas daxpy,ddot,dscal,dasum
        ;;      fortran dabs,dmax1,max0,min0,dreal,dsign
        ;; 
        ;;      internal variables
        ;; 
        ;; 
        ;; 
        ;;      find norm of a
        ;; 
        (fdo (j 1 (+ j 1))
             ((> j n) nil)
             (tagbody
               (setf l (min0 j (1+ m)))
               (setf mu (max0 (- (+ m 2) j) 1))
               (setf (fref z j) (dasum-hook l (vec-ref abd mu j) 1))
               (setf k (- j l))
               (if (< m mu) (go label20))
               (fdo (i mu (+ i 1))
                    ((> i m) nil)
                    (tagbody
                      (setf k (1+ k))
                      (setf (fref z k)
                            (+
                             (fref z k)
                             (dabs (fref abd i j))))))
               label20))
        (setf anorm 0.0d0)
        (fdo (j 1 (+ j 1))
             ((> j n) nil)
             (tagbody (setf anorm (dmax1 anorm (fref z j)))))
        ;; 
        ;;      factor
        ;; 
        (multiple-value-setq (*dummy_var* lda n m info)
          (dpbfa-hook abd lda n m info))
        (if (/= info 0) (go label180))
        ;; 
        ;;         rcond = 1/(norm(a)*(estimate of norm(inverse(a)))) .
        ;;         estimate = norm(z)/norm(y) where  a*z = y  and  a*y = e .
        ;;         the components of  e  are chosen to cause maximum local
        ;;         growth in the elements of w  where  trans(r)*w = e .
        ;;         the vectors are frequently rescaled to avoid overflow.
        ;; 
        ;;         solve trans(r)*w = e
        ;; 
        (setf ek 1.0d0)
        (fdo (j 1 (+ j 1))
             ((> j n) nil)
             (tagbody (setf (fref z j) 0.0)))
        (fdo (k 1 (+ k 1))
             ((> k n) nil)
             (tagbody (if (/= (fref z k) 0.0)
                          (setf ek (dsign ek (- (fref z k)))))
                      (if (<= (dabs (- ek (fref z k)))
                              (fref abd (+ m 1) k))
                          (go label60))
                      (setf s
                            (f2cl/ (fref abd (+ m 1) k)
                                   (dabs (- ek (fref z k)))))
                      (dscal-hook n s z 1)
                      (setf ek (* s ek))
              label60 (setf wk (- ek (fref z k)))
                      (setf wkm (- (- ek) (fref z k)))
                      (setf s (dabs wk))
                      (setf sm (dabs wkm))
                      (setf wk (f2cl/ wk (fref abd (+ m 1) k)))
                      (setf wkm (f2cl/ wkm (fref abd (+ m 1) k)))
                      (setf kp1 (1+ k))
                      (setf j2 (min0 (+ k m) n))
                      (setf i (+ m 1))
                      (if (> kp1 j2) (go label100))
                      (fdo (j kp1 (+ j 1))
                           ((> j j2) nil)
                           (tagbody (setf i (1- i))
                                    (setf sm
                                          (+ sm (dabs
                                                 (+ (fref z j) (* wkm (fref abd i j))))))
                                    (setf (fref z j)
                                          (+ (fref z j) (* wk (fref abd i j))))
                                    (setf s (+ s (dabs (fref z j))))))
                      (if (>= s sm) (go label90))
                      (setf t_ (- wkm wk))
                      (setf wk wkm)
                      (setf i (1+ m))
                      (fdo (j kp1 (+ j 1))
                           ((> j j2) nil)
                           (tagbody (setf i (1- i))
                                    (setf (fref z j)
                                          (+
                                           (fref z j)
                                           (* t_ (fref abd i j))))))
              label90 label100 (setf (fref z k) wk)))
        (setf s (f2cl/ 1.0d0 (dasum-hook n z 1)))
        (dscal-hook n s z 1)
        ;; 
        ;;         solve  r*y = w
        ;; 
        (fdo (kb 1 (1+ kb))
             ((> kb n) nil)
             (tagbody (setf k (- (1+ n) kb))
                      (if (<= (dabs (fref z k)) (fref abd (1+ m) k))
                          (go label120))
                      (setf s (f2cl/ (fref abd (1+ m) k) (dabs (fref z k))))
                      (dscal-hook n s z 1)
              label120 (setf (fref z k)
                             (f2cl/ (fref z k) (fref abd (1+ m) k)))
                      (setf lm (min0 (1- k) m))
                      (setf la (- (+ m 1) lm))
                      (setf lb (- k lm))
                      (setf t_ (- (fref z k)))
                      (daxpy-hook lm t_ (vec-ref abd la k) 1 (vec-ref z lb) 1)))
        (setf s (f2cl/ 1.0d0 (dasum-hook n z 1)))
        (dscal-hook n s z 1)
        ;; 
        (setf ynorm 1.0d0)
        ;; 
        ;;         solve trans(r)*v = y
        ;; 
        (fdo (k 1 (+ k 1))
             ((> k n) nil)
             (tagbody (setf lm (min0 (1- k) m))
                      (setf la (- (1+ m) lm))
                      (setf lb (- k lm))
                      (setf (fref z k)
                            (- (fref z k)
                               (ddot-hook lm (vec-ref abd la k) 1 (vec-ref z lb) 1)))
                      (if (<= (dabs (fref z k)) (fref abd (+ m 1) k))
                          (go label140))
                      (setf s (f2cl/ (fref abd (1+ m) k) (dabs (fref z k))))
                      (dscal-hook n s z 1)
                      (setf ynorm (* s ynorm))
             label140 (setf (fref z k)
                            (f2cl/ (fref z k) (fref abd (1+ m) k)))))
        (setf s (f2cl/ 1.0d0 (dasum-hook n z 1)))
        (dscal-hook n s z 1)
        (setf ynorm (* s ynorm))
        ;; 
        ;;         solve  r*z = w
        ;; 
        (fdo (kb 1 (1+ kb))
             ((> kb n) nil)
             (tagbody (setf k (- (1+ n) kb))
                      (if (<= (dabs (fref z k)) (fref abd (1+ m) k))
                          (go label160))
                      (setf s (f2cl/ (fref abd (1+ m) k) (dabs (fref z k))))
                      (dscal-hook n s z 1)
                      (setf ynorm (* s ynorm))
             label160 (setf (fref z k)
                             (f2cl/ (fref z k) (fref abd (1+ m) k)))
                      (setf lm (min0 (1- k) m))
                      (setf la (- (1+ m) lm))
                      (setf lb (- k lm))
                      (setf t_ (- (fref z k)))
                      (daxpy-hook lm t_ (vec-ref abd la k) 1 (vec-ref z lb) 1)))
        ;;         make znorm = 1.0
        (setf s (f2cl/ 1.0d0 (dasum-hook n z 1)))
        (dscal-hook n s z 1)
        (setf ynorm (* s ynorm))
        ;; 
        (if (/= anorm 0.0d0)
          (setf rcond (f2cl/ ynorm anorm))
          (setf rcond 0.0d0))
  label180 (return (values abd lda n m rcond z info))))
