;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               stats.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is part of the Views system, a toolkit for interactive statistical graphics.
;;;  
;;;
;;;  Authors:
;;;     C.B. Hurley 1988-1991 George Washington University
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(equisize-bins equicount-bins)))

(defvar *projections* nil)
;;;----------------------------------------------------------------------------------




(defun equisize-bins(values nbins &optional label-fn)
  (if label-fn
    (setq label-fn (get-function  label-fn)))
  (if (> nbins 0)
    (let (vmin vmax bin-id )
      (loop for v in values
            maximize v into max
            minimize v into min
            finally (setq vmax max vmin min))
      (setq bin-id
            (loop with d = (- vmax vmin)
                  for v in values
                  collect (if (= v vmax)
                            (- nbins 1)
                            (truncate (* (- v vmin) nbins) d))))
      (if (null label-fn)
        bin-id
        (let ((ordered-values (make-list nbins))
              levels)
          (loop for v in values
                for b in bin-id
                do
                (push v (nth b ordered-values)))
        (setq levels 
              (loop for v in ordered-values 
                    collect (funcall label-fn v)))
        
        (loop for x in bin-id
              collect (nth x levels)))))))


(defun equicount-bins(values nbins &optional label-fn)
  (if label-fn
    (setq label-fn (get-function  label-fn)))
  (if (> nbins 0)
 
    (let* ((values+id (loop for v in values
                            for i upfrom 0
                            collect (cons v i)))
           (svalues+id  (if (some #'numberp values)
                          (sort values+id #'(lambda(x y)
                                           (if (and (numberp x) (numberp y))
                                             (< x y)
                                             (numberp x)))
                             :key #'car )
                          values+id))
           (n (length values))
           (binsizes
            (loop with n1 = n
                  for i from 0 below nbins
                  for size = (truncate n1 (- nbins i))
                  collect size
                  do (decf n1 size)))
           (svalues (mapcar #'car svalues+id))
           
           (bin-labels (cond ((eq label-fn (get-function 'median))
                              (loop  with start = 0
                                    for size in binsizes
                                    collect (eltf svalues (+ start (/ (- size 1) 2))) 
                                    do (incf start size)))
                             (label-fn
                              (loop with start = 0
                                    for size in binsizes
                                    collect (funcall label-fn (subseq  svalues start (+ start size)))
                                    do (incf start size)))
                             (t (loop for i from 0 below nbins collect i))))
                              
           (rep-bin-labels
            (loop for s in binsizes
                  for b in bin-labels
                  append
                  (make-list s :initial-element b))))
      
      (loop with bins = (make-list n)
            for b in rep-bin-labels
             for (s . i) in svalues+id
             do
             (setf (nth i bins) b)
             finally (return bins)))

      ))


(defun map-args (fn values more)
  (if more (mapcar fn (cons values more))
      (funcall fn values)))

(defun sum (values &rest more)
  (map-args #'(lambda (v) (reduce #'+ v)) values more))

(defun eltf(x i)
             (if (integerp i)
               (nth i x)
               (multiple-value-bind(i1 f) (truncate i)
                 (let ((i2 (+ i1 1))
                       (x1 (nth i1 x)))
                   (+ x1 (* f (- (nth i2 x) x1)))))))


#|
(defun mean (values &rest more)  
  (map-args #'(lambda (v) (if v (/ (reduce #'+ v) (length v))))
            values more))

(defun median (values &rest more)
  (flet ((med (values)
           (if values
              (let* ((vals (sort values #'<)))
                (eltf vals (/ (- (length vals) 1) 2))))))
    (map-args #'med values more)))

(defun var (values &rest more)
  (flet ((var-fn (values) 
                 (let ((n (length values)) )
                   (case n
                     (0 nil)
                     (1 0.0)
                     (t
                      (loop 
                        with mean = (/ (reduce #'+ values) n)
                        for v in values 
                        sum (* (- v mean) (- v mean)) into ssq
                        finally (return (/ ssq (1- n)))))))))
    (map-args #'var-fn values more)))


(defun sd (values &rest more)
  (map-args #'(lambda (x) (let ((v (var x)))
                            (if v (sqrt v) nil))) values more))





(defun iqr(values &rest more)
  (labels ((iqr1(vals)
             (if vals
           (let ((five-num (five-num vals)))
             (- (nth 3 five-num)
                (nth 1 five-num))))))
    (map-args #'iqr1 values more)))
    
|#

(defun five-num (values &rest more)
  (labels ((fun5 (values)
             (if values
               (let* ((r (sort values #'<)) 
                      (n (- (length r) 1)))
                 (list
                  (elt r 0) (eltf r (/ n 4 ))  (eltf r (/ n 2 ))
                  (eltf r (/ (* 3 n) 4 )) (elt r n))))))
    (map-args #'fun5 values more)))


(defun bp-stats (values &rest more)
  (labels ( (fun (values)
              (let* ((n5 (five-num values))
                     (lq (second n5))
                     (uq (fourth n5))
                     (spread (- uq lq))
                     (lf (- lq (* 1.5 spread)))
                     (uf (+ uq (* 1.5 spread)))
                     lav uav)
          (loop for x in values do
                (if (and (>= x lf) (null lav))
                  (setq lav x))
                (if (<= x uf) (setf uav x)))
          (setf (first n5) lav)
          (setf (fifth n5) uav)
          n5)))
    (map-args #'fun values more)))


(defun ssq (x &optional (y x))
  (let ((ybar (mean y)  ) (xbar (mean x)  ) )
    (loop for xi in x for yi in y
          sum (* (- xi xbar) (- yi ybar)))))
  
(defun cov (x y)
  ;; returns the covariance of x and y
  (/ (ssq x y)
     (1- (min (length x) (length y)))))


(defun corr (x y)
  ;; returns the correlation of x and y
  
    (/ (cov x y) (sqrt (* (var x) (var y)))))

(defun sreg (x y)
  ;; returns intercept slope sd and se(slope) of simple regresion of y on x

  (let* ((sxx (ssq x ))
         (b (/ (ssq x y) sxx)) 
         (a (- (mean y) (* b (mean x))))
         (res (loop for xi in x for yi in y
                     collect (- yi (+ a (* b xi)))))
         (sd (sqrt (/ (ssq res) (- (length res) 2)))))
    (list a b sd (/ sd (sqrt sxx)) )))




;;;----------------------------------------------------------------------------
  

(defun copy-matrix (a)
  (let* ((m (array-dimension a 0))
      (n (array-dimension a 1))
      (copy-a (make-array (list m n))))
    ; (loop for i from 0 below m do
    (do ((i 0 (incf i)))
        ((= i m))
      ; (loop for j from 0 below n do
      (do ((j 0 (incf j)))
          ((= j n))
        (setf (aref copy-a i j) (aref a i j))))
    copy-a))

(defun transpose-matrix (a)
  (let* ((m (array-dimension a 0))
      (n (array-dimension a 1))
      (trans-a (make-array (list n m))))
    ; (loop for i from 0 below m do
    (do ((i 0 (incf i)))
        ((= i m))
      ; (loop for j from 0 below n do
      (do ((j 0 (incf j)))
          ((= j n))
        (setf (aref trans-a j i) (aref a i j))))
    trans-a))

(defun mult-matrix (a b)
  (let* ((m (array-dimension a 0))
      (n (array-dimension a 1))
      (p (array-dimension b 1))
      (res (make-array (list m p))))
    ; (loop for i from 0 below m do
    (do ((i 0 (incf i)))
        ((= i m))
      ; (loop for j from 0 below p do
      (do ((j 0 (incf j)))
          ((= j p))
        (setf (aref res i j)
              ; (loop for k from 0 below n sum
              (do ((sum-a 0)
                   (k 0 (incf k)))
                  ((= k n) sum-a)
                (incf sum-a (* (aref a i k) (aref b k j)))))))
    res))
          
  
(defun qr (a)
  ;; returns q orthogonal and r upper-triang
  (let* ((m (array-dimension a 0))
         (n (array-dimension a 1))
         (r (make-array (list n n) :initial-element 0))
         (q (copy-matrix a)))
    ; (loop for k from 0 below n do
    (do ((k 0 (incf k)))
        ((= k n))
      (setf (aref r k k)
            (sqrt
             ; (loop for i from 0 below m sum
             (do ((sum-q 0)
                  (i 0 (incf i)))
                 ((= i m) sum-q)
               (incf sum-q (* (aref q i k) (aref q i k))))))
      ; (loop for i from 0 below m do
      (do ((i 0 (incf i)))
          ((= i m))
        (setf (aref  q i k) (/ (aref  q i k) (aref r k k))))
      ; (loop for j from (+ 1 k)  below n do
      (do ((j (1+ k) (incf j)))
          ((= j n))
        (setf (aref r k j)
              ; (loop for i from 0 below m sum
              (do ((sum-q 0)
                   (i 0 (incf i)))
                  ((= i m) sum-q)
                (* (aref q i k) (aref q i j))) )
        ; (loop for i from 0 below m do
        (do ((i 0 (incf i)))
            ((= i m))
          (decf (aref q i j) 
                (* (aref q i k) (aref r k j))))))
    (values q r)))




;;====================
#|
;; a, aq ar

;; test orthog of aq
(multiple-value-setq (aq ar) (qr a))
(setq qtq (mult-matrix (transpose-matrix aq) aq))
(setq q-r (mult-matrix aq ar))


(loop for i from 0 below 8 do (quail-print (aref qtq i i)))
(loop for i from 0 below 8
      maximize (loop for j from 0 below 8
                     unless (= i j) 
                     maximize (aref qtq i j)))
(loop for i from 0 below 10
      maximize (loop for j from 0 below 8
                     maximize (abs (- (aref a i j)
                                      (aref q-r i j)))))
|#


;;===============================================================


(defun q-matrix (x &optional cols rows )
  ;; X is a list  of lists (rows)

  ;; returns an orthog matrix whose cols span col-space of
  ;; X 

  (if (null cols)
    (setq cols
          (loop for i from 0 below (length (first x))
                collect i)))
  (if rows
    (setq x (if (and (not (null rows)) (listp rows))
              (loop for xi in  x 
                    for ri in rows when ri collect xi)
              (loop for xi in  x 
                     when (funcall rows xi) collect xi))))
  (let ((n (length x))
        (p (length cols))
        x-mat  )
    (setq x-mat (make-array (list n p) ))
    (loop for i from 0 below n 
          for xi in x do
          (loop for j from 0 below p
                for k in cols do
                (setf (aref x-mat i j) (elt xi k))))

  (qr x-mat)))
  



(defun orthog-comp (x)
  (let* ((n (array-dimension x 0))
         (x-comp (make-array (list n n))))

  (loop for i from 0 below n do
        (loop for j from 0 below n do
              (setf (aref x-comp i j)
                    (if (= i j)
                      (- 1 (aref x i j))
                      (- (aref x i j))))))
  x-comp))

(defun make-projector (mat &optional name inverse?)
  (unless name 
    (setq name "PROJECTOR"))
  (let ((p (list mat name inverse?)))
    (push p *projections*)
    p))



(defun make-orthog-projector (mat &optional name )
  (unless name 
    (setq name "PROJECTOR"))
  (setq name (concatenate 'string "ORTH " name))
  (make-projector mat name 'orthog))

(defun projector-matrix(p) (car p))
(defun projector-name(p) (second p))
(defun projector-dim(p) (array-dimension (car p) 0))

(defun orthog-p (p ) (third p))


(defun apply-projector (p seq)
  (let* ((mat (projector-matrix p))
         (n (array-dimension mat 0))
         (m (array-dimension mat 1))
         pt*seq result)
  (if (not (= (length seq) n)) seq
      (progn
      (setq pt*seq 
            (loop for j from 0 below m
                    collect
                    (loop for i from 0 below n
                          sum (* (elt seq i) (aref mat i j)))))
      (setq result 
            (loop for i from 0 below n
                    collect
                    (loop for j from 0 below m
                          for sj in pt*seq 
                          sum (* sj (aref mat i j)))))
      (if (orthog-p p)
        (setq result
              (loop for r in result for i from 0 below n
                    collect (- (elt seq i) r))))
      (if (vectorp seq)
        (make-array n :initial-contents result)
        result)))))


(defun make-projection-for-vars (x name &key cols rows
                                   (orthog-too? t))
  (let ((p (q-matrix x cols rows)))
    (if  orthog-too?
      (values  (make-projector p name) (make-orthog-projector p name ))
      (make-projector p name))))



(defun array-to-lists (a)
  (loop for i from 0 below (array-dimension a 0) 
        collect
        (loop for j from 0 below (array-dimension a 1) 
              collect (aref a i j))))
 
