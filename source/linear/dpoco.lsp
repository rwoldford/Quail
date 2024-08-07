(in-package :q)

(defun dpoco-hook (a lda n rcond z info)
  ;;(declare (type fixnum info))
  ;;(declare (type (simple-array double-float (*)) z))
  ;;(declare (type double-float rcond))
  (declare (type fixnum n))
  (declare (type fixnum lda))
  ;;(declare (type (simple-array double-float (* *)) a))
  (prog ((t_ 0.0d0) (wkm 0.0d0) (wk 0.0d0) (ek 0.0d0) (ynorm 0.0d0)
         (sm 0.0d0) (s 0.0d0) (anorm 0.0d0) (kp1 0) (kb 0) (k 0) (jm1 0)
         (j 0) (i 0))
        ;(declare (type single-float t_)) ;; declared double-float below and in other files where it is used
        (declare (type fixnum i))
        (declare (type fixnum j))
        (declare (type fixnum jm1))
        (declare (type fixnum k))
        (declare (type fixnum kb))
        (declare (type fixnum kp1))
        (declare (type double-float anorm))
        (declare (type double-float s))
        (declare (type double-float sm))
        (declare (type double-float ynorm))
        (declare (type double-float ek))
        (declare (type double-float t_))
        (declare (type double-float wk))
        (declare (type double-float wkm))
        (declare (special *dummy_var*))
        (fdo (j 1 (+ j 1))
             ((> j n) nil)
             (tagbody (setf (fref z j) (dasum-hook j (vec-ref a 1 j) 1))
                      (setf jm1 (- j 1))
                      (if (< jm1 1) (go label20))
                      (fdo (i 1 (+ i 1))
                           ((> i jm1) nil)
                           (tagbody (setf (fref z i) (+ (fref z i) (dabs (fref a i j))))))
              label20))
        (setf anorm 0.0d0)
        (fdo (j 1 (+ j 1))
             ((> j n) nil)
             (tagbody (setf anorm (dmax1 anorm (fref z j)))))
        (dpofa-hook a lda n info)
        (if (/= (fref info 1) 0) (go label180))
        (setf ek 1.0d0)
        (fdo (j 1 (+ j 1))
             ((> j n) nil)
             (tagbody (setf (fref z j) 0.0d0)))
        (fdo (k 1 (+ k 1))
             ((> k n) nil)
             (tagbody 
                      (if (/= (fref z k) 0.0)
                          (setf ek (dsign ek (- (fref z k)))))
                      (if (<= (dabs (- ek (fref z k))) (fref a k k))
                          (go label60))
                      (setf s
                            (f2cl/ (fref a k k) (dabs (- ek (fref z k)))))
                      (dscal-hook n s z 1)
                      (setf ek (* s ek))
              label60 (setf wk (- ek (fref z k)))
                      (setf wkm (- (- ek) (fref z k)))
                      (setf s (dabs wk))
                      (setf sm (dabs wkm))
                      (setf wk (f2cl/ wk (fref a k k)))
                      (setf wkm (f2cl/ wkm (fref a k k)))
                      (setf kp1 (+ k 1))
                      (if (> kp1 n) (go label100))
                      (fdo (j kp1 (+ j 1))
                           ((> j n) nil)
                           (tagbody (setf sm (+ sm (dabs
                                                    (+ (fref z j)
                                                       (* wkm (fref a k j))))))
                                    (setf (fref z j)
                                          (+ (fref z j) (* wk (fref a k j))))
                                    (setf s (+ s (dabs (fref z j))))))
                      (if (>= s sm) (go label90))
                      (setf t_ (- wkm wk))
                      (setf wk wkm)
                      (fdo (j kp1 (+ j 1))
                           ((> j n) nil)
                           (tagbody (setf (fref z j)
                                          (+ (fref z j)
                                             (* t_ (fref a k j))))))
              label90 label100 (setf (fref z k) wk)))
        (setf s (f2cl/ 1.0
                       (dasum-hook n z 1)))
        (dscal-hook n s z 1)
        (fdo (kb 1 (+ kb 1))
             ((> kb n) nil)
             (tagbody (setf k (- (+ n 1) kb))
                      (if (<= (dabs (fref z k)) (fref a k k)) (go label120))
                      (setf s (f2cl/ (fref a k k) (dabs (fref z k))))
                      (dscal-hook n s z 1)
              label120 (setf (fref z k) (f2cl/ (fref z k) (fref a k k)))
                       (setf t_ (- (fref z k)))
                       (daxpy-hook (- k 1) t_ (vec-ref a 1 k) 1 (vec-ref z 1) 1)))
        (setf s (f2cl/ 1.0
                       (dasum-hook n z 1)))
        (dscal-hook n s z 1)
        (setf ynorm 1.0d0)
        (fdo (k 1 (+ k 1))
             ((> k n) nil)
             (tagbody (setf (fref z k) 
                            (- (fref z k) 
                               (ddot-hook (- k 1) (vec-ref a 1 k) 1 (vec-ref z 1) 1)))
                      (if (<= (dabs (fref z k)) (fref a k k)) (go label140))
                      (setf s (f2cl/ (fref a k k) (dabs (fref z k))))
                      (dscal-hook n s z 1)
                      (setf ynorm (* s ynorm))
              label140 (setf (fref z k) (f2cl/ (fref z k) (fref a k k)))))
        (setf s (f2cl/ 1.0 (dasum-hook n z 1)))
        (dscal-hook n s z 1)
        (setf ynorm (* s ynorm))
        (fdo (kb 1 (+ kb 1))
             ((> kb n) nil)
             (tagbody (setf k (- (+ n 1) kb))
                      (if (<= (dabs (fref z k)) (fref a k k)) (go label160))
                      (setf s (f2cl/ (fref a k k) (dabs (fref z k))))
                      (dscal-hook n s z 1)
                      (setf ynorm (* s ynorm))
              label160 (setf (fref z k) (f2cl/ (fref z k) (fref a k k)))
                       (setf t_ (- (fref z k)))
                       (daxpy-hook (- k 1) t_ (vec-ref a 1 k) 1 (vec-ref z 1) 1)))
        (setf s (f2cl/ 1.0
                       (dasum-hook n z 1)))
        (dscal-hook n s z 1)
        (setf ynorm (* s ynorm))
        (if (/= anorm 0.0) (setf (fref rcond 1) (f2cl/ ynorm anorm)))
        (if (= anorm 0.0) (setf (fref rcond 1) 0.0))
   label180 (return (values a lda n rcond z info))))

