;;; not yet tested. CW.

(in-package :q)

(defun dsifa-hook (a lda n kpvt info)
  ;(declare (type fixnum info))
  ;(declare (type (simple-array fixnum (*)) kpvt))
  (declare (type fixnum n))
  (declare (type fixnum lda))
  ;(declare (type (simple-array double-float (* *)) a))
  (prog ((t_ 0.0d0) (mulkm1 0.0d0) (mulk 0.0d0) (denom 0.0d0) (bkm1 0.0d0)
         (bk 0.0d0) (akm1 0.0d0) (ak 0.0d0) (rowmax 0.0d0) (colmax 0.0d0) (alpha 0.0d0)
         (absakk 0.0d0) (kstep 0) (km2 0) (km1 0) (k 0) (jmax 0)
         (jj 0) (j 0) (imaxp1 0) (imax 0) (swap nil))
        (declare (type t swap))
        (declare (type fixnum imax))
        (declare (type fixnum imaxp1))
        (declare (type fixnum j))
        (declare (type fixnum jj))
        (declare (type fixnum jmax))
        (declare (type fixnum k))
        (declare (type fixnum km1))
        (declare (type fixnum km2))
        (declare (type fixnum kstep))
        (declare (type double-float absakk))
        (declare (type double-float alpha))
        (declare (type double-float colmax))
        (declare (type double-float rowmax))
        (declare (type double-float ak))
        (declare (type double-float akm1))
        (declare (type double-float bk))
        (declare (type double-float bkm1))
        (declare (type double-float denom))
        (declare (type double-float mulk))
        (declare (type double-float mulkm1))
        (declare (type double-float t_))

        (setf alpha (f2cl/ (+ 1.0d0 (dsqrt 17d0)) 8.0d0))
        (setf (fref info 1) 0)
        (setf k n)
   label10 (if (= k 0) (go label200))
        (if (> k 1) (go label20))
        (setf (fref kpvt 1) 1)
        (if (= (fref a 1 1) 0.0) (setf (fref info 1) 1))
        (go label200)
   label20 (setf km1 (+ k (- 1)))
        (setf absakk (dabs (fref a k k)))
        (setf imax (idamax (- k 1) (vec-ref a 1 k) 1))
        (setf colmax (dabs (fref a imax k)))
        (if (< absakk (* alpha colmax)) (go label30))
        (setf kstep 1)
        (setf swap nil)
        (go label90)
   label30 (setf rowmax 0.0d0)
        (setf imaxp1 (+ imax 1))
        (fdo (j imaxp1 (+ j 1))
             ((> j k) nil)
             (tagbody (setf rowmax (dmax1 rowmax (dabs (fref a imax j))))))
        (if (= imax 1) (go label50))
        (setf jmax (idamax (- imax 1) (vec-ref a 1 imax) 1))
        (setf rowmax (dmax1 rowmax (dabs (fref a jmax imax))))
   label50 (if (< (dabs (fref a imax imax)) (* alpha rowmax)) (go label60))
        (setf kstep 1)
        (setf swap t)
        (go label80)
   label60 (if (< absakk (* alpha colmax (f2cl/ colmax rowmax)))
               (go label70))
        (setf kstep 1)
        (setf swap nil)
        (go label80)
   label70 (setf kstep 2)
        (setf swap (/= imax km1))
   label80 label90 (if (/= (dmax1 absakk colmax) 0.0) (go label100))
        (setf (fref kpvt k) k)
        (setf (fref info 1) k)
        (go label190)
   label100 (if (= kstep 2) (go label140))
        (if (not swap) (go label120))
        (dswap-hook imax (vec-ref a 1 imax) 1 (vec-ref a 1 k) 1)
        (fdo (jj imax (+ jj 1))
             ((> jj k) nil)
             (tagbody (setf j (+ k imax (- jj)))
                      (setf t_ (fref a j k))
                      (setf (fref a j k) (fref a imax j))
                      (setf (fref a imax j) t_)))
   label120 (fdo (jj 1 (+ jj 1))
                 ((> jj km1) nil)
                 (tagbody (setf j (+ k (- jj)))
                          (setf mulk (f2cl/ (* -1 (fref a j k)) (fref a k k)))
                          (setf t_ mulk)
                          (daxpy-hook j t_ (vec-ref a 1 k) 1 (vec-ref a 1 j) 1)
                          (setf (fref a j k) mulk)))
        (setf (fref kpvt k) k)
        (if swap (setf (fref kpvt k) imax))
        (go label190)
   label140 (if (not swap) (go label160))
        (dswap-hook imax (vec-ref a 1 imax) 1 (vec-ref a 1 (+ k (- 1))) 1)
        (fdo (jj imax (+ jj 1))
             ((> jj km1) nil)
             (tagbody (setf j (+ (+ km1 imax) (- jj)))
                      (setf t_ (fref a j (- k 1)))
                      (setf (fref a j (- k 1)) (fref a imax j))
                      (setf (fref a imax j) t_)))
        (setf t_ (fref a (- k 1) k))
        (setf (fref a (- k 1) k) (fref a imax k))
        (setf (fref a imax k) t_)
   label160 (setf km2 (- k 2))
        (if (= km2 0) (go label180))
        (setf ak (f2cl/ (fref a k k) (fref a (- k 1) k)))
        (setf akm1
              (f2cl/ (fref a (- k 1) (- k 1)) (fref a (- k 1) k)))
        (setf denom (+ 1.0d0 (* (* -1 ak) akm1)))
        (fdo (jj 1 (+ jj 1))
             ((> jj km2) nil)
             (tagbody (setf j (+ km1 (- jj)))
                      (setf bk (f2cl/ (fref a j k) (fref a (- k 1) k)))
                      (setf bkm1
                            (f2cl/ (fref a j (+ k (- 1)))
                                   (fref a (+ k (- 1)) k)))
                      (setf mulk (f2cl/ (- (* akm1 bk) bkm1) denom))
                      (setf mulkm1 (f2cl/ (- (* ak bkm1) bk) denom))
                      (setf t_ mulk)
                      (daxpy-hook j t_ (vec-ref a 1 k) 1 (vec-ref a 1 j) 1)
                      (setf t_ mulkm1)
                      (daxpy-hook j t_ (vec-ref a 1 (- k 1)) 1 (vec-ref a 1 j) 1)
                      (setf (fref a j k) mulk)
                      (setf (fref a j (- k 1)) mulkm1)))
   label180 (setf (fref kpvt k) (- 1 k))
            (if swap (setf (fref kpvt k) (- imax)))
            (setf (fref kpvt (- k 1)) (fref kpvt k))
   label190 (setf k (- k kstep))
        (go label10)
   label200 (return (values a lda n kpvt info))))

