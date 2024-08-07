(in-package :q)

(defun dqrdc-hook (x ldx n p qraux jpvt work job)
  (declare (type fixnum job))
  ;;(declare (type (simple-array double-float (*)) work))
  ;;(declare (type (simple-array fixnum (*)) jpvt))
  ;;(declare (type (simple-array double-float (*)) qraux))
  (declare (type fixnum p))
  (declare (type fixnum n))
  (declare (type fixnum ldx))
  ;;(declare (type (simple-array double-float (* *)) x))
  (prog ((t_ 0.0d0) (jj 0) (pu 0) (pl 0) (maxj 0) (lup 0) (lp1 0) (l 0) (jp 0)
         (j 0) (tt 0.0d0) (maxnrm 0.0d0) (nrmxl 0.0d0) (swapj nil) (negj nil))
         ;(dnrm2 0.0d0) (ddot 0.0d0)) ; 31JUL2023
        (declare (type fixnum jj))
        (declare (type t negj))
        (declare (type t swapj))
        ;(declare (type double-float ddot))
        (declare (type double-float nrmxl))
        (declare (type double-float t_))
        (declare (type double-float maxnrm))
        ;(declare (type double-float dnrm2))
        (declare (type double-float tt))
        (declare (type fixnum j))
        (declare (type fixnum jp))
        (declare (type fixnum l))
        (declare (type fixnum lp1))
        (declare (type fixnum lup))
        (declare (type fixnum maxj))
        (declare (type fixnum pl))
        (declare (type fixnum pu))
        (setf pl 1)
        (setf pu 0)
        (if (= job 0) (go label60))
        (fdo (j 1 (+ j 1))
             ((> j p) nil)
             (tagbody (setf swapj (> (fref jpvt j) 0))
                      (setf negj (< (fref jpvt j) 0))
                      (setf (fref jpvt j) j)
                      (if negj (setf (fref jpvt j) (- j)))
                      (if (not swapj) (go label10))
                      (if (/= j pl)
                          (dswap-hook n (vec-ref x 1 pl) 1 (vec-ref x 1 j) 1))
                      (setf (fref jpvt j) (fref jpvt pl))
                      (setf (fref jpvt pl) j)
                      (setf pl (+ pl 1))
              label10))
        (setf pu p)
        (fdo (jj 1 (+ jj 1))
             ((> jj p) nil)
             (tagbody (setf j (+ (- p jj) 1))
                      (if (>= (fref jpvt j) 0) (go label40))
                      (setf (fref jpvt j) (- (fref jpvt j)))
                      (if (= j pu) (go label30))
                      (dswap-hook n (vec-ref x 1 pu) 1 (vec-ref x 1 j) 1)
                      (setf jp (fref jpvt pu))
                      (setf (fref jpvt pu) (fref jpvt j))
                      (setf (fref jpvt j) jp)
              label30 (setf pu (- pu 1))
              label40))
   label60 (if (< pu pl) (go label80))
        (fdo (j pl (+ j 1))
             ((> j pu) nil)
             (tagbody (setf (fref qraux j) (dnrm2 n (vec-ref x 1 j) 1))
                      (setf (fref work j) (fref qraux j))))
   label80 (setf lup (min0 n p))
        (fdo (l 1 (+ l 1))
             ((> l lup) nil)
             (tagbody (if (or (< l pl) (>= l pu)) (go label120))
                      (setf maxnrm 0.0d0)
                      (setf maxj l)
                      (fdo (j l (+ j 1))
                           ((> j pu) nil)
                           (tagbody (if (<= (fref qraux j) maxnrm)
                                        (go label90))
                                    (setf maxnrm (fref qraux j))
                                    (setf maxj j)
                            label90))
                      (if (= maxj l) (go label110))
                      (dswap-hook n (vec-ref x 1 l) 1 (vec-ref x 1 maxj) 1)
                      (setf (fref qraux maxj) (fref qraux l))
                      (setf (fref work maxj) (fref work l))
                      (setf jp (fref jpvt maxj))
                      (setf (fref jpvt maxj) (fref jpvt l))
                      (setf (fref jpvt l) jp)
              label110 label120 (setf (fref qraux l) 0.0)
                      (if (= l n) (go label190))
                      (setf nrmxl (dnrm2-hook (+ (- n l) 1) (vec-ref x l l) 1))
                      (if (= nrmxl 0.0d0) (go label180))
                      (if (/= (fref x l l) 0.0)
                          (setf nrmxl (dsign nrmxl (fref x l l))))
                      (dscal-hook (+ (- n l) 1) (f2cl/ 1.0 nrmxl) (vec-ref x l l)
                         1)
                      (setf (fref x l l) (+ 1.0 (fref x l l)))
                      (setf lp1 (+ l 1))
                      (if (< p lp1) (go label170))
                      (fdo (j lp1 (+ j 1))
                           ((> j p) nil)
                           (tagbody (setf t_
                                          (f2cl/
                                           (-
                                            (ddot-hook
                                             (+ (- n l) 1)
                                             (vec-ref x l l)
                                             1
                                             (vec-ref x l j)
                                             1))
                                           (fref x l l)))
                                    (daxpy-hook (+ (- n l) 1) t_ (vec-ref x l l)
                                       1 (vec-ref x l j) 1)
                                    (if (or (< j pl) (> j pu)) (go label150))
                                    (if (= (fref qraux j) 0.0) (go label150))
                                    (setf tt
                                          (-
                                           1.0
                                           (expt
                                             (f2cl/
                                              (dabs (fref x l j))
                                              (fref qraux j))
                                             2)))
                                    (setf tt (dmax1 tt 0.0d0))
                                    (setf t_ tt)
                                    (setf tt
                                          (+
                                           1.0d0
                                           (*
                                            0.05d0
                                            tt
                                            (expt
                                             (f2cl/
                                              (fref qraux j)
                                              (fref work j))
                                             2))))
                                    (if (= tt 1.0d0) (go label130))
                                    (setf (fref qraux j) (* (fref qraux j) (dsqrt t_)))
                                    (go label140)
                            label130 (setf (fref qraux j)
                                           (dnrm2 (- n l) (vec-ref x (+ l 1) j) 1))
                                     (setf (fref work j) (fref qraux j))
                            label140 label150))
              label170 (setf (fref qraux l) (fref x l l))
                       (setf (fref x l l) (- nrmxl))
              label180 label190))
        (return (values x ldx n p qraux jpvt work job))))

