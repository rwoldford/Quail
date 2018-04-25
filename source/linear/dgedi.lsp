(in-package :q)

(defun dgedi-hook (a lda n ipvt det work job)
  (declare (type fixnum job))
  ;;(declare (type (simple-array double-float (*)) work))
  ;;(declare (type (simple-array double-float (*)) det))
  ;;(declare (type (simple-array fixnum (*)) ipvt))
  (declare (type fixnum n))
  (declare (type fixnum lda))
  ;;(declare (type (simple-array double-float (* *)) a))
  (prog ((t_ 0.0) (ten 0.0) (nm1 0) (l 0) (kp1 0) (kb 0) (k 0) (j 0)
         (i 0))
        (declare (type fixnum i))
        (declare (type fixnum j))
        (declare (type fixnum k))
        (declare (type fixnum kb))
        (declare (type fixnum kp1))
        (declare (type fixnum l))
        (declare (type fixnum nm1))
        (declare (type double-float ten))
        (declare (type double-float t_))
        (if (= (f2cl/ job 10) 0) (go label70))
        (setf (fref det 1) 1.0)
        (setf (fref det 2) 0.0)
        (setf ten 10.0)
        (fdo (i 1 (+ i 1))
             ((> i n) nil)
             (tagbody (if (/= (fref ipvt i) i) (setf (fref det 1) (- (fref det 1))))
                      (setf (fref det 1) (* (fref a i i) (fref det 1)))
                      (if (= (fref det 1) 0.0) (go label60))
                      label10 (if (>= (dabs (fref det 1)) 1.0) (go label20))
                      (setf (fref det 1) (* ten (fref det 1)))
                      (setf (fref det 2) (- (fref det 2) 1.0))
                      (go label10)
                      label20 label30 (if (< (dabs (fref det 1)) ten) (go label40))
                      (setf (fref det 1) (f2cl/ (fref det 1) ten))
                      (setf (fref det 2) (+ (fref det 2) 1.0))
                      (go label30)
                      label40))
   label60 label70 (if (= (mod job 10) 0) (go label150))
        (fdo (k 1 (+ k 1))
             ((> k n) nil)
             (tagbody (setf (fref a k k) (f2cl/ 1.0 (fref a k k)))
                      (setf t_ (- (fref a k k)))
                      (dscal-hook (- k 1) t_ (vec-ref a 1 k) 1)
                      (setf kp1 (+ k 1))
                      (if (< n kp1) (go label90))
                      (fdo (j kp1 (+ j 1))
                           ((> j n) nil)
                           (tagbody (setf t_ (fref a k j))
                                    (setf (fref a k j) 0.0)
                                    (daxpy-hook k t_ (vec-ref a 1 k) 1 (vec-ref a 1 j)
                                       1)))
              label90))
        (setf nm1 (+ n (- 1)))
        (if (< nm1 1) (go label140))
        (fdo (kb 1 (+ kb 1))
             ((> kb nm1) nil)
             (tagbody (setf k (- n kb))
                      (setf kp1 (+ k 1))
                      (fdo (i kp1 (+ i 1))
                           ((> i n) nil)
                           (tagbody (setf (fref work i) (fref a i k))
                                    (setf (fref a i k) 0.0)))
                      (fdo (j kp1 (+ j 1))
                           ((> j n) nil)
                           (tagbody (setf t_ (fref work j))
                                    (daxpy-hook n t_ (vec-ref a 1 j) 1 (vec-ref a 1 k)
                                       1)))
                      (setf l (fref ipvt k))
                      (if (/= l k)
                          (dswap-hook n (vec-ref a 1 k) 1 (vec-ref a 1 l) 1))))
   label140 label150 (return (values a lda n ipvt det work job))))

