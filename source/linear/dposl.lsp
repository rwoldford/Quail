;;; not yet tested. CW.

(in-package :q)

(defun dposl-hook (a lda n b)
  ;(declare (type (simple-array double-float (*)) b))
  (declare (type fixnum n))
  (declare (type fixnum lda))
  ;(declare (type (simple-array double-float (* *)) a))
  (prog ((t_ 0.0) (kb 0) (k 0))
        (declare (type fixnum k))
        (declare (type fixnum kb))
        (declare (type double-float t_))
        (fdo (k 1 (+ k 1))
             ((> k n) nil)
             (tagbody (setf t_ (ddot-hook (- k 1) (vec-ref a 1 k) 1 (vec-ref b 1) 1))
                      (setf (fref b k) (f2cl/ (- (fref b k) t_) (fref a k k)))))
        (fdo (kb 1 (+ kb 1))
             ((> kb n) nil)
             (tagbody (setf k (+ n 1 (- kb)))
                      (setf (fref b k) (f2cl/ (fref b k) (fref a k k)))
                      (setf t_ (- (fref b k)))
                      (daxpy-hook (- k 1) t_ (vec-ref a 1 k) 1 (vec-ref b 1) 1)))
        (return (values a lda n b))))

