;;; not yet tested. CW.

(in-package :q)

(defun dtrco-hook (t_ ldt n rcond z job)
  (declare (type fixnum job))
  ;(declare (type (simple-array double-float (*)) z))
  ;(declare (type double-float rcond))
  (declare (type fixnum n))
  (declare (type fixnum ldt))
  ;(declare (type (simple-array double-float (* *)) t_))
  (prog ((ek 0.0d0) (wkm 0.0d0) (wk 0.0d0) (w 0.0d0) (sm 0.0d0) (s 0.0d0)
         (ynorm 0.0d0) (tnorm 0.0d0) (l 0) (kk 0) (k 0) (j2 0) (j1 0) (j 0) (i1 0)
         (lower nil))
        (declare (type t lower))
        (declare (type fixnum i1))
        (declare (type fixnum j))
        (declare (type fixnum j1))
        (declare (type fixnum j2))
        (declare (type fixnum k))
        (declare (type fixnum kk))
        (declare (type fixnum l))
        (declare (type double-float tnorm))
        (declare (type double-float ynorm))
        (declare (type double-float s))
        (declare (type double-float sm))
        (declare (type double-float w))
        (declare (type double-float wk))
        (declare (type double-float wkm))
        (declare (type double-float ek))
        (setf lower (= job 0))
        (setf tnorm 0.0d0)
        (fdo (j 1 (+ j 1))
             ((> j n) nil)
             (tagbody (setf l j)
                      (if lower (setf l (+ n 1 (- j))))
                      (setf i1 1)
                      (if lower (setf i1 j))
                      (setf tnorm (dmax1 tnorm (dasum-hook l (vec-ref t_ i1 j) 1)))))
        (setf ek 1.0d0)
        (fdo (j 1 (+ j 1))
             ((> j n) nil)
             (tagbody (setf (fref z j) 0.0)))
        (fdo (kk 1 (+ kk 1))
             ((> kk n) nil)
             (tagbody (setf k kk)
                      (if lower (setf k (+ n 1 (- kk))))
                      (if (/= (fref z k) 0.0)
                          (setf ek (dsign ek (- (fref z k)))))
                      (if (<= (dabs (+ ek (- (fref z k)))) (dabs (fref t_ k k)))
                          (go label30))
                      (setf s
                            (f2cl/ (dabs (fref t_ k k))
                                   (dabs (- ek (fref z k)))))
                      (dscal-hook n s z 1)
                      (setf ek (* s ek))
              label30 (setf wk (- ek (fref z k)))
                      (setf wkm (- (- ek) (fref z k)))
                      (setf s (dabs wk))
                      (setf sm (dabs wkm))
                      (if (= (fref t_ k k) 0.0) (go label40))
                      (setf wk (f2cl/ wk (fref t_ k k)))
                      (setf wkm (f2cl/ wkm (fref t_ k k)))
                      (go label50)
              label40 (setf wk 1.0d0)
                      (setf wkm 1.0d0)
              label50 (if (= kk n) (go label90))
                      (setf j1 (+ k 1))
                      (if lower (setf j1 1))
                      (setf j2 n)
                      (if lower (setf j2 (- k 1)))
                      (fdo (j j1 (+ j 1))
                           ((> j j2) nil)
                           (tagbody (setf sm
                                          (+ sm (dabs (+ (fref z j)
                                                         (* wkm (fref t_ k j))))))
                                    (setf (fref z j) (+ (fref z j) (* wk (fref t_ k j))))
                                    (setf s (+ s (dabs (fref z j))))))
                      (if (>= s sm) (go label80))
                      (setf w (- wkm wk))
                      (setf wk wkm)
                      (fdo (j j1 (+ j 1))
                           ((> j j2) nil)
                           (tagbody (setf (fref z j) (+ (fref z j) (* w (fref t_ k j))))))
              label80 label90 (setf (fref z k) wk)))
        (setf s (f2cl/ 1.0d0 (dasum-hook n z 1)))
        (dscal-hook n s z 1)
        (setf ynorm 1.0d0)
        (fdo (kk 1 (+ kk 1))
             ((> kk n) nil)
             (tagbody (setf k (+ n 1 (- kk)))
                      (if lower (setf k kk))
                      (if (<= (dabs (fref z k)) (dabs (fref t_ k k)))
                          (go label110))
                      (setf s (f2cl/ (dabs (fref t_ k k)) (dabs (fref z k))))
                      (dscal-hook n s z 1)
                      (setf ynorm (* s ynorm))
              label110 (if (/= (fref t_ k k) 0.0) (setf (fref z k) (/ (fref z k) (fref t_ k k))))
                      (if (= (fref t_ k k) 0.0) (setf (fref z k) 1.0))
                      (setf i1 1)
                      (if lower (setf i1 (+ k 1)))
                      (if (>= kk n) (go label120))
                      (setf w (- (fref z k)))
                      (daxpy-hook (- n kk) w (vec-ref t_ i1 k) 1 (vec-ref z i1) 1)
              label120))
        (setf s (f2cl/ 1.0d0 (dasum-hook n z 1)))
        (dscal-hook n s z 1)
        (setf ynorm (* s ynorm))
        (if (/= tnorm 0.0d0) (setf (fref rcond 1) (f2cl/ ynorm tnorm)))
        (if (= tnorm 0.0d0) (setf (fref rcond 1) 0.0))
  (return (values t_ ldt n rcond z job))))
