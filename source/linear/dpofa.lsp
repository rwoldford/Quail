(in-package :q)

(defun dpofa-hook (a lda n info)
  ;;(declare (type fixnum info))
  (declare (type fixnum n))
  (declare (type fixnum lda))
  ;;(declare (type (simple-array double-float (* *)) a))
  (prog ((t_ 0.0d0) (s 0.0d0) (k 0) (jm1 0) (j 0))
    (declare (type fixnum j))
    (declare (type fixnum jm1))
    (declare (type fixnum k))
    (declare (type double-float s))
    (declare (type double-float ddot))
    (declare (type double-float t_))
    (fdo (j 1 (+ j 1))
         ((> j n) nil)
         (tagbody (setf (fref info 1) j)
                  (setf s 0.0d0)
                  (setf jm1 (- j 1))
                  (if (< jm1 1) (go label20))
                  (fdo (k 1 (+ k 1))
                       ((> k jm1) nil)
                       (tagbody 
                         (setf t_
                               (- (fref a k j)
                                  (ddot-hook (- k 1) (vec-ref a 1 k)
                                        1 (vec-ref a 1 j) 1)))
                         (setf t_ (f2cl/ t_ (fref a k k)))
                         (setf (fref a k j) t_)
                         (setf s (+ s (* t_ t_)))
                         ))
                  label20 
                          (setf s (- (fref a j j) s))
                  (if (<= s 0.0d0) (go label40))
                  (setf (fref a j j) (dsqrt s))))
    (setf (fref info 1) 0)
  label40 (return (values a lda n info))))
