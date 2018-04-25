(in-package :q)

(defun dgbco-hook (abd lda n ml mu ipvt rcond z)
  ;; (declare (type (simple-array double-float (*)) z))
  (declare (type double-float rcond))
  ;; (declare (type (simple-array fixnum (*)) ipvt))
  (declare (type fixnum mu))
  (declare (type fixnum ml))
  (declare (type fixnum n))
  (declare (type fixnum lda))
  ;; (declare (type (simple-array double-float (* *)) abd))
  (prog ((wkm 0.0) (wk 0.0) (t_ 0.0) (ek 0.0) (ynorm 0.0)
         (sm 0.0) (s 0.0) (anorm 0.0) (mm 0) (m 0) (lz 0) (lm 0)
         (la 0) (l 0) (kp1 0) (kb 0) (k 0) (ju 0) (j 0) (info 0) (is 0))
        (declare (type fixnum is))
        (declare (type fixnum info))
        (declare (type fixnum j))
        (declare (type fixnum ju))
        (declare (type fixnum k))
        (declare (type fixnum kb))
        (declare (type fixnum kp1))
        (declare (type fixnum l))
        (declare (type fixnum la))
        (declare (type fixnum lm))
        (declare (type fixnum lz))
        (declare (type fixnum m))
        (declare (type fixnum mm))
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
        ;;      dgbco factors a double precision band matrix by gaussian
        ;;      elimination and estimates the condition of the matrix.
        ;; 
        ;;      if  rcond  is not needed, dgbfa is slightly faster.
        ;;      to solve  a*x = b , follow dgbco by dgbsl.
        ;;      to compute  inverse(a)*c , follow dgbco by dgbsl.
        ;;      to compute  determinant(a) , follow dgbco by dgbdi.
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
        ;; 
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
        ;;                 underflows.
        ;; 
        ;;         z       double precision(n)
        ;;                 a work vector whose contents are usually unimportant.
        ;;                 if  a  is close to a singular matrix, then  z  is
        ;;                 an approximate null vector in the sense that
        ;;                 norm(a*z) = rcond*norm(a)*norm(z) .
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
        ;;      example..  if the original matrix is
        ;; 
        ;;            11 12 13  0  0  0
        ;;            21 22 23 24  0  0
        ;;             0 32 33 34 35  0
        ;;             0  0 43 44 45 46
        ;;             0  0  0 54 55 56
        ;;             0  0  0  0 65 66
        ;; 
        ;;       then  n = 6, ml = 1, mu = 2, lda .ge. 5  and abd should contain
        ;; 
        ;;             *  *  *  +  +  +  , * = not used
        ;;             *  * 13 24 35 46  , + = used for pivoting
        ;;             * 12 23 34 45 56
        ;;            11 22 33 44 55 66
        ;;            21 32 43 54 65  *
        ;; 
        ;;      linpack. this version dated 08/14/78 .
        ;;      cleve moler, university of new mexico, argonne national lab.
        ;; 
        ;;      subroutines and functions
        ;; 
        ;;      linpack dgbfa
        ;;      blas daxpy,ddot,dscal,dasum
        ;;      fortran dabs,dmax1,max0,min0,dsign
        ;; 
        ;;      internal variables
        ;; 
        ;; 
        ;; 
        ;;      compute 1-norm of a
        ;; 
        (setf anorm 0.0)
        (setf l (1+ ml))
        (setf is (+ l mu))
        (fdo (j 1 (1+ j))
             ((> j n) nil)
             (tagbody (setf anorm (dmax1 anorm (dasum-hook l (vec-ref abd is j) 1)))
                      (if (> is (1+ ml)) (setf is (1- is)))
                      (if (<= j mu) (setf l (1+ l)))
                      (if (>= j (- n ml)) (setf l (1- l)))))
        ;; 
        ;;      factor
        ;; 
        (multiple-value-setq (*dummy_var* lda n ml mu *dummy_var* info)
          (dgbfa-hook abd lda n ml mu ipvt info))
        ;; 
        ;;      rcond = 1/(norm(a)*(estimate of norm(inverse(a)))) .
        ;;      estimate = norm(z)/norm(y) where  a*z = y  and  trans(a)*y = e .
        ;;      trans(a)  is the transpose of a .  the components of  e  are
        ;;      chosen to cause maximum local growth in the elements of w  where
        ;;      trans(u)*w = e .  the vectors are frequently rescaled to avoid
        ;;      overflow.
        ;; 
        ;;      solve trans(u)*w = e
        ;; 
        (setf ek 1.0)
        (fdo (j 1 (1+ j)) ((> j n) nil)
             (tagbody (setf (fref z j) 0.0)))
        (setf m (+ ml mu 1))
        (setf ju 0)
        (fdo (k 1 (1+ k))
             ((> k n) nil)
             (tagbody (if (/= (fref z k) 0.0)
                          (setf ek (dsign ek (- (fref z k)))))
                      (if (<= (dabs (- ek (fref z k)))
                              (dabs (fref abd m k)))
                          (go label30))
                      (setf s
                            (f2cl/ (dabs (fref abd m k))
                                   (dabs (- ek (fref z k)))))
                      (dscal-hook n s z 1)
                      (setf ek (* s ek))
              label30 (setf wk (- ek (fref z k)))
                      (setf wkm (- (- ek) (fref z k)))
                      (setf s (dabs wk))
                      (setf sm (dabs wkm))
                      (if (= (fref abd m k) 0.0) (go label40))
                      (setf wk (f2cl/ wk (fref abd m k)))
                      (setf wkm (f2cl/ wkm (fref abd m k)))
                      (go label50)
              label40 (setf wk 1.0)
                      (setf wkm 1.0)
              label50 (setf kp1 (1+ k))
                      (setf ju (min0 (max0 ju (+ mu (fref ipvt k))) n))
                      (setf mm m)
                      (if (> kp1 ju) (go label90))
                      (fdo (j kp1 (1+ j))
                           ((> j ju) nil)
                           (tagbody (setf mm (1- mm))
                                    (setf sm
                                          (+
                                           sm
                                           (dabs
                                            (+
                                             (fref z j)
                                             (* wkm (fref abd mm j))))))
                                    (setf (fref z j)
                                          (+
                                           (fref z j)
                                           (* wk (fref abd mm j))))
                                    (setf s (+ s (dabs (fref z j))))))
                      (if (>= s sm) (go label80))
                      (setf t_ (- wkm wk))
                      (setf wk wkm)
                      (setf mm m)
                      (fdo (j kp1 (1+ j))
                           ((> j ju) nil)
                           (tagbody (setf mm (1- mm))
                                    (setf (fref z j)
                                          (+
                                           (fref z j)
                                           (* t_ (fref abd mm j))))))
              label80 label90 (setf (fref z k) wk)))
        (setf s (f2cl/ 1.0 (dasum-hook n z 1)))
        (dscal-hook n s z 1)
        ;; 
        ;;      solve trans(l)*y = w
        ;; 
        (fdo (kb 1 (+ kb 1))
             ((> kb n) nil)
             (tagbody (setf k (+ n 1 (- kb)))
                      (setf lm (min0 ml (- n k)))
                      (if (< k n)
                          (setf (fref z k)
                                (+ (fref z k)
                                   (ddot-hook lm (vec-ref abd (1+ m) k) 1
                                    (vec-ref z (1+ k)) 1))))
                      (if (<= (dabs (fref z k)) 1.0) (go label110))
                      (setf s (f2cl/ 1.0 (dabs (fref z k))))
                      (dscal-hook n s z 1)
             label110 (setf l (fref ipvt k))
                      (setf t_ (fref z l))
                      (setf (fref z l) (fref z k))
                      (setf (fref z k) t_)))
        (setf s (f2cl/ 1.0 (dasum-hook n z 1)))
        (dscal-hook n s z 1)
        ;; 
        (setf ynorm 1.0)
        ;; 
        ;;      solve l*v = y
        ;; 
        (fdo (k 1 (1+ k))
             ((> k n) nil)
             (tagbody (setf l (fref ipvt k))
                      (setf t_ (fref z l))
                      (setf (fref z l) (fref z k))
                      (setf (fref z k) t_)
                      (setf lm (min0 ml (- n k)))
                      (if (< k n)
                          (daxpy-hook lm t_ (vec-ref abd (1+ m) k) 1
                             (fref z (1+ k)) 1))
                      (if (<= (dabs (fref z k)) 1.0) (go label130))
                      (setf s (f2cl/ 1.0 (dabs (fref z k))))
                      (dscal-hook n s z 1)
                      (setf ynorm (* s ynorm))
              label130))
        (setf s (f2cl/ 1.0 (dasum-hook n z 1)))
        (dscal-hook n s z 1)
        (setf ynorm (* s ynorm))
        ;; 
        ;;      solve  u*z = w
        ;; 
        (fdo (kb 1 (+ kb 1))
             ((> kb n) nil)
             (tagbody (setf k (+ n 1 (- kb)))
                      (if (<= (dabs (fref z k)) (dabs (fref abd m k)))
                          (go label150))
                      (setf s (f2cl/ (dabs (fref abd m k)) (dabs (fref z k))))
                      (dscal-hook n s z 1)
                      (setf ynorm (* s ynorm))
              label150 (if (/= (fref abd m k) 0.0)
                           (setf (fref z k) (f2cl/ (fref z k) (fref abd m k))))
                      (if (= (fref abd m k) 0.0) (setf (fref z k) 1.0))
                      (setf lm (1- (min0 k m)))
                      (setf la (- m lm))
                      (setf lz (- k lm))
                      (setf t_ (- (fref z k)))
                      (daxpy-hook lm t_ (vec-ref abd la k) 1 (vec-ref z lz) 1)))
        ;;      make znorm = 1.0
        (setf s (f2cl/ 1.0 (dasum-hook n z 1)))
        (dscal-hook n s z 1)
        (setf ynorm (* s ynorm))
        ;; 
        (if (/= anorm 0.0)
          (setf rcond (f2cl/ ynorm anorm))
          (setf rcond 0.0))
        (return (values abd lda n ml mu ipvt rcond z))))

