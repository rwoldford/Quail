(in-package :q)

(defun dtrsl-hook (t_ ldt n b job info)
  ;;(declare (type fixnum info))
  (declare (type fixnum job))
  ;;(declare (type (simple-array double-float (*)) b))
  (declare (type fixnum n))
  (declare (type fixnum ldt))
  ;;(declare (type (simple-array double-float (* *)) t_))
  (prog ((temp 0.0d0) (jj 0) (j 0) (case_ 0) (tmpinfo 0))
         ;(ddot 0.0d0)) ; 31JUL2023
    (declare (type fixnum tmpinfo))
    (declare (type fixnum j))
    (declare (type fixnum j))
    (declare (type fixnum jj))
    ;(declare (type double-float ddot))
    (declare (type double-float temp))
    (fdo (tmpinfo 1 (+ tmpinfo 1))
         ((> tmpinfo n) nil)
         (tagbody (if (= (fref t_ tmpinfo tmpinfo) 0.0)
                    (prog ()
                      (setf (fref info 1) tmpinfo)
                      (go label150)))))
    (setf (fref info 1) 0)
    (setf case_ 1)
    (if (/= (mod job 10) 0) (setf case_ 2))
    (if (/= (f2cl/ (mod job 100) 10) 0) (setf case_ (+ case_ 2)))
    (case case_
      (1 (go label20))
      (2 (go label50))
      (3 (go label80))
      (4 (go label110)))
    label20 (setf (fref b 1) (f2cl/ (fref b 1) (fref t_ 1 1)))
    (if (< n 2) (go label40))
    (fdo (j 2 (+ j 1))
         ((> j n) nil)
         (tagbody (setf temp (- (fref b (- j 1))))
                  (daxpy-hook (+ (- n j) 1) temp (vec-ref t_ j (- j 1)) 1
                           (vec-ref b j) 1)
                  (setf (fref b j) (f2cl/ (fref b j) (fref t_ j j)))))
    label40 (go label140)
    label50 (setf (fref b n) (f2cl/ (fref b n) (fref t_ n n)))
    (if (< n 2) (go label70))
    (fdo (jj 2 (+ jj 1))
         ((> jj n) nil)
         (tagbody (setf j (+ (- n jj) 1))
                  (setf temp (- (fref b (+ j 1))))
                  (daxpy-hook j temp (vec-ref t_ 1 (+ j 1)) 1 (vec-ref b 1) 1)
                  (setf (fref b j) (f2cl/ (fref b j) (fref t_ j j)))))
    label70 (go label140)
    label80 (setf (fref b n) (f2cl/ (fref b n) (fref t_ n n)))
    (if (< n 2) (go label100))
    (fdo (jj 2 (+ jj 1))
         ((> jj n) nil)
         (tagbody (setf j (+ (- n jj) 1))
                  (setf (fref b j)
                        (- (fref b j)
                           (ddot-hook (- jj 1) (vec-ref t_ (+ j 1) j) 1 (vec-ref b (+ j 1)) 1)))
                  (setf (fref b j) (f2cl/ (fref b j) (fref t_ j j)))))
    label100 (go label140)
    label110 (setf (fref b 1) (f2cl/ (fref b 1) (fref t_ 1 1)))
    (if (< n 2) (go label130))
    (fdo (j 2 (+ j 1)) ((> j n) nil)
         (tagbody (setf (fref b j)
                        (- (fref b j)
                           (ddot-hook (- j 1) (vec-ref t_ 1 j) 1 (vec-ref b 1) 1)))
                  (setf (fref b j) (f2cl/ (fref b j) (fref t_ j j)))))
    label130 label140 label150 (return (values t_ ldt n b job info))))

