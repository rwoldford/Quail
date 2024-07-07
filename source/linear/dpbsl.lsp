(in-package :q)

(defun dpbsl-hook (abd lda n m b)
  ;; (declare (type (simple-array double-float (*)) b))
  (declare (type fixnum m))
  (declare (type fixnum n))
  (declare (type fixnum lda))
  ;; (declare (type (simple-array double-float (* *)) abd))
  (prog ((t_ 0.0d0) (lm 0) (lb 0) (la 0) (kb 0) (k 0))
        (declare (type fixnum k))
        (declare (type fixnum kb))
        (declare (type fixnum la))
        (declare (type fixnum lb))
        (declare (type fixnum lm))
        (declare (type double-float t_))
        (declare (special *dummy_var*))
        ;; 
        ;;      dpbsl solves the double precision symmetric positive definite
        ;;      band system  a*x = b
        ;;      using the factors computed by dpbco or dpbfa.
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
        ;;         b       double precision(n)
        ;;                 the right hand side vector.
        ;; 
        ;;      on return
        ;; 
        ;;         b       the solution vector  x .
        ;; 
        ;;      error condition
        ;; 
        ;;         a division by zero will occur if the input factor contains
        ;;         a zero on the diagonal.  technically this indicates
        ;;         singularity but it is usually caused by improper subroutine
        ;;         arguments.  it will not occur if the subroutines are called
        ;;         correctly and  info .eq. 0 .
        ;; 
        ;;      to compute  inverse(a) * c  where  c  is a matrix
        ;;      with  p  columns
        ;;            call dpbco(abd,lda,n,rcond,z,info)
        ;;            if (rcond is too small .or. info .ne. 0) go to ...
        ;;            do 10 j = 1, p
        ;;               call dpbsl(abd,lda,n,c(1,j))
        ;;         10 continue
        ;; 
        ;;      linpack.  this version dated 08/14/78 .
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
        ;;      solve trans(r)*y = b
        ;; 
        (fdo (k 1 (+ k 1))
             ((> k n) nil)
             (tagbody (setf lm (min0 (1- k) m))
                      (setf la (- (1+ m) lm))
                      (setf lb (- k lm))
                      (setf t_ (ddot-hook lm (vec-ref abd la k) 1 (vec-ref b lb) 1))
                      (setf (fref b k)
                            (f2cl/ (- (fref b k) t_)
                                   (fref abd (1+ m) k)))))
        ;; 
        ;;      solve r*x = y
        ;; 
        (fdo (kb 1 (+ kb 1))
             ((> kb n) nil)
             (tagbody (setf k (- (1+ n) kb))
                      (setf lm (min0 (1- k) m))
                      (setf la (- (1+ m) lm))
                      (setf lb (- k lm))
                      (setf (fref b k) (f2cl/ (fref b k) (fref abd (1+ m) k)))
                      (setf t_ (- (fref b k)))
                      (daxpy-hook lm t_ (vec-ref abd la k) 1 (vec-ref b lb) 1)))
        (return (values abd lda n m b))))

