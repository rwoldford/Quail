(defgeneric qqplot (dist1 dist2 &optional n)
  (:documentation
   "Produce a quantile-quantile plot."))

(defmethod-multi qqplot ((dist1 (sequence array dimensioned-ref-object))
                         (dist2 T)
                         &optional (n NIL))
  (let (p)
    (cond
     ((numberp n)
      (setf p (seq 0.0 1.0 (/ 1.0 n)))
      (lines-plot :x (quantile-at dist1 p)
                  :y (quantile-at dist2 p)
                  :title "Quantile-Quantile Plot"))
     (T (setf p (cdf-at dist1 dist1))
