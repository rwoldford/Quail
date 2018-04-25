(in-package :quail)

(defun contrasts (x &optional (method :default))
  (calc-contrasts x method))

(defmethod calc-contrasts ((x matrix) (method (eql :default)))
  x)

(defmethod calc-contrasts ((x factor-object) (method (eql :default)))
  (calc-contrasts x :helmert))
  
(defmethod calc-contrasts ((x factor-object) (method (eql :helmert)))
  (let* ((num-levels (first (dimensions-of (levels-of x))))
         (contrasts (array :empty
                            :dimensions (list num-levels (- num-levels 1)))))
    ; (loop for i from 0 to (- num-levels 1) do
    (do ((i 0 (incf i)))
        ((= i num-levels))
      (progn
        ; (loop for j from 0 to (- i 2) do
        (do ((j 0 (incf j)))
            ((> j (- i 2)))
          (setf (eref contrasts i j) 0))
        (if (> i 0) (setf (eref contrasts i (- i 1)) i))
        ; (loop for j from i to (- num-levels 2) do
        (do ((j i (incf j)))
            ((> j (- num-levels 2)))
          (setf (eref contrasts i j) -1))))
    contrasts))

(defmethod calc-contrasts ((x ordinal-factor) (method (eql :default)))
  (calc-contrasts x :poly))

(defmethod calc-contrasts ((x ordinal-factor) (method (eql :poly)))
  (warn "calc-contrasts :poly not implemented yet ... using :helmert")
  (calc-contrasts x :helmert))
                            
  
