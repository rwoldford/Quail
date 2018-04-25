;;; not yet tested. CW.

(in-package :q)

(defun dsisl-hook (a lda n kpvt b)
  ;(declare (type (simple-array double-float (*)) b))
  ;(declare (type (simple-array fixnum (*)) kpvt))
  (declare (type fixnum n))
  (declare (type fixnum lda))
  ;(declare (type (simple-array double-float (* *)) a))
  (prog ((temp 0.0) (denom 0.0) (bkm1 0.0) (bk 0.0) (akm1 0.0)
         (ak 0.0) (kp 0) (k 0))
        (declare (type fixnum k))
        (declare (type fixnum kp))
        (declare (type double-float ak))
        (declare (type double-float akm1))
        (declare (type double-float bk))
        (declare (type double-float bkm1))
        (declare (type double-float denom))
        (declare (type double-float temp))
        (setf k n)
   label10 (if (= k 0) (go label80))
        (if (< (fref kpvt k) 0) (go label40))
        (if (= k 1) (go label30))
        (setf kp (fref kpvt k))
        (if (= kp k) (go label20))
        (setf temp (fref b k))
        (setf (fref b k) (fref b kp))
        (setf (fref b kp) temp)
   label20 (daxpy-hook (- k 1) (fref b k) (vec-ref a 1 k) 1 (vec-ref b 1) 1)
           (setf (fref b k) (f2cl/ (fref b k) (fref a k k)))
   label30 (setf k (- k 1))
        (go label70)
   label40 (if (= k 2) (go label60))
        (setf kp (iabs (fref kpvt k)))
        (if (= kp (- k 1)) (go label50))
        (setf temp (fref b (- k 1)))
        (setf (fref b (- k 1)) (fref b kp))
        (setf (fref b kp) temp)
   label50 (daxpy-hook (- k 2) (fref b k) (vec-ref a 1 k) 1 (vec-ref b 1) 1)
        (daxpy-hook (- k 2) (fref b (- k 1)) (vec-ref a 1 (- k 1)) 1
           (vec-ref b 1) 1)
   label60 (setf ak (f2cl/ (fref a k k) (fref a (- k 1) k)))
        (setf akm1
              (f2cl/ (fref a (- k 1) (- k 1)) (fref a (- k 1) k)))
        (setf bk (f2cl/ (fref b k) (fref a (- k 1) k)))
        (setf bkm1 (f2cl/ (fref b (- k 1)) (fref a (- k 1) k)))
        (setf denom (- (* ak akm1) 1.0))
        (setf (fref b k) (f2cl/ (- (* akm1 bk) bkm1) denom))
        (setf (fref b (- k 1)) (f2cl/ (- (* ak bkm1) bk) denom))
        (setf k (- k 2))
   label70 (go label10)
   label80 (setf k 1)
   label90 (if (> k n) (go label160))
        (if (< (fref kpvt k) 0) (go label120))
        (if (= k 1) (go label110))
        (setf (fref b k) (+ (fref b k)
                            (ddot (- k 1) (vec-ref a 1 k) 1 (vec-ref b 1) 1)))
        (setf kp (fref kpvt k))
        (if (= kp k) (go label100))
        (setf temp (fref b k))
        (setf (fref b k) (fref b kp))
        (setf (fref b kp) temp)
   label100 label110 (setf k (+ k 1))
        (go label150)
   label120 (if (= k 1) (go label140))
        (setf (fref b k) (+ (fref b k)
                            (ddot (- k 1) (vec-ref a 1 k) 1 (vec-ref b 1) 1)))
        (setf (fref b (+ k 1)) (+ (fref b (+ k 1))
                                  (ddot (- k 1) (vec-ref a 1 (+ k 1)) 1 (vec-ref b 1) 1)))
        (setf kp (iabs (fref kpvt k)))
        (if (= kp k) (go label130))
        (setf temp (fref b k))
        (setf (fref b k) (fref b kp))
        (setf (fref b kp) temp)
   label130 label140 (setf k (+ k 2))
   label150 (go label90)
   label160 (return (values a lda n kpvt b))))

