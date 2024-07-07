(in-package :q)

(defun dgesl-hook (a lda n ipvt b job)
  (declare (type fixnum job))
  ;;(declare (type (simple-array double-float (*)) b))
  ;;(declare (type (simple-array fixnum (*)) ipvt))
  (declare (type fixnum n))
  (declare (type fixnum lda))
  ;;(declare (type (simple-array double-float (* *)) a))
  (prog ((t_ 0.0d0) (nm1 0) (l 0) (kb 0) (k 0))
    (declare (type fixnum k))
    (declare (type fixnum kb))
    (declare (type fixnum l))
    (declare (type fixnum nm1))
    (declare (type double-float t_))
    (setf nm1 (- n 1))
    (if (/= job 0) (go label50))
    (if (< nm1 1) (go label30))
    (fdo (k 1 (+ k 1))
         ((> k nm1) nil)
         (tagbody (setf l (fref ipvt k))
                  (setf t_ (fref b l))
                  (if (= l k) (go label10))
                  (setf (fref b l) (fref b k))
                  (setf (fref b k) t_)
                  label10 (daxpy-hook (- n k) t_ (vec-ref a (+ k 1) k) 1
                                      (vec-ref b (+ k 1)) 1)))
    label30 (fdo (kb 1 (+ kb 1))
                 ((> kb n) nil)
                 (tagbody (setf k (- (+ n 1) kb))
                          (setf (fref b k) (f2cl/ (fref b k) (fref a k k)))
                          (setf t_ (- (fref b k)))
                          (daxpy-hook (- k 1) t_ (vec-ref a 1 k) 1 (vec-ref b 1)
                                      1)))
    (go label100)
    label50 (fdo (k 1 (+ k 1))
                 ((> k n) nil)
                 (tagbody (setf t_
                                (ddot-hook (- k 1) (vec-ref a 1 k) 1 (vec-ref b 1)
                                           1))
                          (setf (fref b k) (f2cl/ (- (fref b k) t_) (fref a k k)))))
    (if (< nm1 1) (go label90))
    (fdo (kb 1 (+ kb 1))
         ((> kb nm1) nil)
         (tagbody (setf k (- n kb))
                  (setf (fref b k)
                        (+ (fref b k)
                           (ddot-hook (- n k) (vec-ref a (+ k 1) k) 1 (vec-ref b (+ k 1))
                                      1)))
                  (setf l (fref ipvt k))
                  (if (= l k) (go label70))
                  (setf t_ (fref b l))
                  (setf (fref b l) (fref b k))
                  (setf (fref b k) t_)
                  label70))
    label90 label100 (return (values a lda n ipvt b job))))

