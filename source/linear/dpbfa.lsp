(in-package :q)

(defun dpbfa-hook (abd lda n m info)
  (declare (type fixnum info))
  (declare (type fixnum m))
  (declare (type fixnum n))
  (declare (type fixnum lda))
  ;; (declare (type (simple-array double-float (* *)) abd))
  (prog ((t_ 0.0d0) (s 0.0d0) (mu 0) (k 0) (jk 0) (j 0) (ik 0))
        (declare (type fixnum ik))
        (declare (type fixnum j))
        (declare (type fixnum jk))
        (declare (type fixnum k))
        (declare (type fixnum mu))
        (declare (type double-float s))
        (declare (type double-float t_))
        ;; 
        ;;      dpbfa factors a double precision symmetric positive definite
        ;;      matrix stored in band form.
        ;; 
        ;;      dpbfa is usually called by dpbco, but it can be called
        ;;      directly with a saving in time if  rcond  is not needed.
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
        ;; 
        ;;         info    integer
        ;;                 = 0  for normal return.
        ;;                 = k  if the leading minor of order  k  is not
        ;;                      positive definite.
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
        ;;      linpack.  this version dated 08/14/78 .
        ;;      cleve moler, university of new mexico, argonne national lab.
        ;; 
        ;;      subroutines and functions
        ;; 
        ;;      blas ddot
        ;;      fortran max0,dsqrt
        ;; 
        ;;      internal variables
        ;; 
        ;;      begin block with ...exits to 40
        ;; 
        (fdo (j 1 (+ j 1))
             ((> j n) nil)
             (tagbody (setf info j)
                      (setf s 0.0d0)
                      (setf ik (1+ m))
                      (setf jk (max0 (- j m) 1))
                      (setf mu (max0 (- (+ m 2) j) 1))
                      (if (< m mu) (go label20))
                      (fdo (k mu (1+ k))
                           ((> k m) nil)
                           (tagbody (setf t_
                                          (-
                                           (fref abd k j)
                                           (ddot-hook
                                            (- k mu)
                                            (vec-ref abd ik jk)
                                            1
                                            (vec-ref abd mu j)
                                            1)))
                                    (setf t_ (f2cl/ t_ (fref abd (1+ m) jk)))
                                    (setf (fref abd k j) t_)
                                    (setf s (+ s (* t_ t_)))
                                    (setf ik (1- ik))
                                    (setf jk (1+ jk))))
              label20 (setf s (- (fref abd (1+ m) j) s))
                      ;;      ......exit
                      (if (<= s 0.0d0) (go label40))
                      (setf (fref abd (1+ m) j) (dsqrt s))))
        (setf info 0)
   label40 (return (values abd lda n m info))))

