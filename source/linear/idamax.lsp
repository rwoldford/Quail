(in-package :q)

(defun idamax-hook (n dx incx)
  (declare (type fixnum incx))
  ;;(declare (type (simple-array double-float (*)) dx))
  (declare (type fixnum n))
  (prog ((idamax 0) (dmax 0.0d0) (ix 0) (i 0))
        (declare (type fixnum i))
        (declare (type fixnum ix))
        (declare (type double-float dmax))
        (declare (type fixnum idamax))
        (setf idamax 0)
        (if (< n 1) (go end_label))
        (setf idamax 1)
        (if (= n 1) (go end_label))
        (if (= incx 1) (go label20))
        (setf ix 1)
        (setf dmax (dabs (fref dx 1)))
        (setf ix (+ ix incx))
        (fdo (i 2 (+ i 1))
             ((> i n) nil)
             (tagbody (if (<= (dabs (fref dx ix)) dmax) (go label5))
                      (setf idamax i)
                      (setf dmax (dabs (fref dx ix)))
              label5  (setf ix (+ ix incx))))
        (go end_label)
   label20 (setf dmax (dabs (fref dx 1)))
        (fdo (i 2 (+ i 1))
             ((> i n) nil)
             (tagbody (if (<= (dabs (fref dx i)) dmax) (go label30))
                      (setf idamax i)
                      (setf dmax (dabs (fref dx i)))
              label30))
        (go end_label)
   end_label (return idamax)))

