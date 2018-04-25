(in-package :q)

(defun dgbsl-hook (abd lda n ml mu ipvt b job)
  (declare (type fixnum job))
  ;; (declare (type (simple-array double-float (*)) b))
  ;; (declare (type (simple-array fixnum (*)) ipvt))
  (declare (type fixnum mu))
  (declare (type fixnum ml))
  (declare (type fixnum n))
  (declare (type fixnum lda))
  ;; (declare (type (simple-array double-float (* *)) abd))
  (prog ((t_ 0.0) (nm1 0) (m 0) (lm 0) (lb 0) (la 0) (l 0)
         (kb 0) (k 0))
        (declare (type fixnum k))
        (declare (type fixnum kb))
        (declare (type fixnum l))
        (declare (type fixnum la))
        (declare (type fixnum lb))
        (declare (type fixnum lm))
        (declare (type fixnum m))
        (declare (type fixnum nm1))
        (declare (type double-float t_))
        (declare (special *dummy_var*))
        ;; 
        ;;      dgbsl solves the double precision band system
        ;;      a * x = b  or  trans(a) * x = b
        ;;      using the factors computed by dgbco or dgbfa.
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
        ;;         b       double precision(n)
        ;;                 the right hand side vector.
        ;; 
        ;;         job     integer
        ;;                 = 0         to solve  a*x = b ,
        ;;                 = nonzero   to solve  trans(a)*x = b , where
        ;;                             trans(a)  is the transpose.
        ;; 
        ;;      on return
        ;; 
        ;;         b       the solution vector  x .
        ;; 
        ;;      error condition
        ;; 
        ;;         a division by zero will occur if the input factor contains a
        ;;         zero on the diagonal.  technically this indicates singularity
        ;;         but it is often caused by improper arguments or improper
        ;;         setting of lda .  it will not occur if the subroutines are
        ;;         called correctly and if dgbco has set rcond .gt. 0.0
        ;;         or dgbfa has set info .eq. 0 .
        ;; 
        ;;      to compute  inverse(a) * c  where  c  is a matrix
        ;;      with  p  columns
        ;;            call dgbco(abd,lda,n,ml,mu,ipvt,rcond,z)
        ;;            if (rcond is too small) go to ...
        ;;            do 10 j = 1, p
        ;;               call dgbsl(abd,lda,n,ml,mu,ipvt,c(1,j),0)
        ;;         10 continue
        ;; 
        ;;      linpack. this version dated 08/14/78 .
        ;;      cleve moler, university of new mexico, argonne national lab.
        ;; 
        ;;      subroutines and functions
        ;; 
        ;;      blas daxpy,ddot
        ;;      fortran min0
        ;; 
        ;;      internal variables
        ;; 
        ;; 
        (setf m (+ mu ml 1))
        (setf nm1 (1- n))
        (if (/= job 0) (go label50))
        ;; 
        ;;         job = 0 , solve  a * x = b
        ;;         first solve l*y = b
        ;; 
        (if (= ml 0) (go label30))
        (if (< nm1 1) (go label30))
        (fdo (k 1 (1+ k))
             ((> k nm1) nil)
             (tagbody (setf lm (min0 ml (- n k)))
                      (setf l (fref ipvt k))
                      (setf t_ (fref b l))
                      (if (= l k) (go label10))
                      (setf (fref b l) (fref b k))
                      (setf (fref b k) t_)
              label10 (daxpy-hook lm t_ (vec-ref abd (1+ m) k) 1 (vec-ref b (1+ k))
                         1)))
label30 ;; 
        ;;         now solve  u*x = y
        ;; 
        (fdo (kb 1 (1+ kb))
             ((> kb n) nil)
             (tagbody (setf k (- (1+ n) kb))
                      (setf (fref b k) (f2cl/ (fref b k) (fref abd m k)))
                      (setf lm (1- (min0 k m)))
                      (setf la (- m lm))
                      (setf lb (- k lm))
                      (setf t_ (- (fref b k)))
                      (daxpy-hook lm t_ (vec-ref abd la k) 1 (vec-ref b lb) 1)))
        (go label100)
label50 ;; 
        ;;         job = nonzero, solve  trans(a) * x = b
        ;;         first solve  trans(u)*y = b
        ;; 
        (fdo (k 1 (1+ k))
             ((> k n) nil)
             (tagbody (setf lm (1- (min0 k m)))
                      (setf la (- m lm))
                      (setf lb (- k lm))
                      (setf t_ (ddot-hook lm (vec-ref abd la k) 1 (vec-ref b lb) 1))
                      (setf (fref b k)
                            (f2cl/ (- (fref b k) t_) (fref abd m k)))))
        ;; 
        ;;         now solve trans(l)*x = y
        ;; 
        (if (= ml 0) (go label90))
        (if (< nm1 1) (go label90))
        (fdo (kb 1 (1+ kb))
             ((> kb nm1) nil)
             (tagbody (setf k (- n kb))
                      (setf lm (min0 ml (- n k)))
                      (setf (fref b k)
                            (+ (fref b k)
                               (ddot-hook lm (vec-ref abd (1+ m) k) 1 (vec-ref b (1+ k))
                                1)))
                      (setf l (fref ipvt k))
                      (if (= l k) (go label70))
                      (setf t_ (fref b l))
                      (setf (fref b l) (fref b k))
                      (setf (fref b k) t_)
              label70))
   label90 label100 (return (values abd lda n ml mu ipvt b job))))

