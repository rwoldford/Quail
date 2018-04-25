;;; -*- Mode: LISP -*-

(in-package 'Z)

(defvar *Z.boolean.not.available* 'NA)
(defvar *ieee.not.available* -9999.99)
(defvar *Z.string.not.available* "Not Available")

;;;
;;; function definitions
;;;

(defun eb.and (x y)
       (declare (special *Z.boolean.not.available*))
       (if (or (eq x *Z.boolean.not.available*)
               (eq y *Z.boolean.not.available*))
           *Z.boolean.not.available*
           (and x y)))

(defun eb.not (x)
       (declare (special *Z.boolean.not.available*))
       (if (eq x *Z.boolean.not.available*)
           *Z.boolean.not.available*
           (not x)))

(defun eb.or (x y)
       (declare (special *Z.boolean.not.available*))
       (if (or (eq x *Z.boolean.not.available*)
               (eq y *Z.boolean.not.available*))
           *Z.boolean.not.available*
           (or x y)))

(defun eb.xor (x y)
       (declare (special *Z.boolean.not.available*))
       (if (or (eq x *Z.boolean.not.available*)
               (eq y *Z.boolean.not.available*))
           *Z.boolean.not.available*
           (or (and x (not y))
               (and (not x)
                    y))))

(defun er.* (x y)
       (declare (special *ieee.not.available*))
       (if (or (eq x *ieee.not.available*)
               (eq y *ieee.not.available*))
           *ieee.not.available*
           (* x y)))

(defun er.+ (x y)
       (declare (special *ieee.not.available*))
       (if (or (eq x *ieee.not.available*)
               (eq y *ieee.not.available*))
           *ieee.not.available*
           (+ x y)))

(defun er.- (x y)
       (declare (special *ieee.not.available*))
       (if (or (eq x *ieee.not.available*)
               (eq y *ieee.not.available*))
           *ieee.not.available*
           (- x y)))

(defun er./ (x y)
       (declare (special *ieee.not.available*))
       (if (or (eq x *ieee.not.available*)
               (eq y *ieee.not.available*)
               (equal y 0.0))
           *ieee.not.available*
           (/ x y)))

(defun er.< (x y)
       (declare (special *ieee.not.available*
                         *Z.boolean.not.available*))
       (if (or (eq x *ieee.not.available*)
               (eq y *ieee.not.available*))
           *Z.boolean.not.available*
           (< x y)))

(defun er.= (x y)
       (declare (special *ieee.not.available*
                         *Z.boolean.not.available*))
       (if (or (eq x *ieee.not.available*)
               (eq y *ieee.not.available*))
           *Z.boolean.not.available*
           (equal x y)))

(defun er.> (x y)
       (declare (special *ieee.not.available*
                         *Z.boolean.not.available*))
       (if (or (eq x *ieee.not.available*)
               (eq y *ieee.not.available*))
           *Z.boolean.not.available*
           (> x y)))

(defun er.abs (x)
       (declare (special *ieee.not.available*))
       (if (eq x *ieee.not.available*)
           *ieee.not.available*
           (abs x)))

(defun er.acos (x)
       (declare (special *ieee.not.available*))
       (if (or (eq x *ieee.not.available*)
               (> (abs x)
                  1.0))
           *ieee.not.available*
           (acos x)))

(defun er.asin (x)
       (declare (special *ieee.not.available*))
       (if (or (eq x *ieee.not.available*)
               (> (abs x)
                  1.0))
           *ieee.not.available*
           (asin x)))

(defun er.atan (x)
       (declare (special *ieee.not.available*))
       (if (eq x *ieee.not.available*)
           *ieee.not.available*
           (atan x)))

(defun er.ceiling (x)
       (declare (special *ieee.not.available*))
       (if (eq x *ieee.not.available*)
           *ieee.not.available*
           (fceiling x)))

(defun er.cos (x)
       (declare (special *ieee.not.available*))
       (if (eq x *ieee.not.available*)
           *ieee.not.available*
           (cos x)))

(defun er.exp (x)
       (declare (special *ieee.not.available*))
       (if (or (eq x *ieee.not.available*)
               (> (abs x)
                  180.0))
           *ieee.not.available*
           (exp x)))

(defun er.floor (x)
       (declare (special *ieee.not.available*))
       (if (eq x *ieee.not.available*)
           *ieee.not.available*
           (ffloor x)))

(defun er.log (x)
       (declare (special *ieee.not.available*))
       (if (or (eq x *ieee.not.available*)
               (not (> x 0.0)))
           *ieee.not.available*
           (log x)))

(defun er.log10 (x)
       (declare (special *ieee.not.available*))
       (if (or (eq x *ieee.not.available*)
               (not (> x 0.0)))
           *ieee.not.available*
           (log x 10)))

(defun er.mod (x y)
       (declare (special *ieee.not.available*))
       (if (or (eq x *ieee.not.available*)
               (eq y *ieee.not.available*))
           *ieee.not.available*
           (float (mod x y))))

(defun er.qgauss (p &optional (center 0.0)
                    (scale 1.0))
       (declare (special *ieee.not.available*))
       (if (or (eql p *ieee.not.available*)
               (eql center *ieee.not.available*)
               (eql scale *ieee.not.available*)
               (not (> p 0.0))
               (not (> 1.0 p)))
           *ieee.not.available*
           (+ center (* scale (qgauss p)))))

(defun er.random (x y)
       (declare (special *ieee.not.available*))
       (if (or (eq x *ieee.not.available*)
               (eq y *ieee.not.available*))
           *ieee.not.available*
           (+ x (random (- y x)))))

(defun er.rem (x y)
       (declare (special *ieee.not.available*))
       (if (or (eq x *ieee.not.available*)
               (eq y *ieee.not.available*))
           *ieee.not.available*
           (mod x y)))

(defun er.rgauss (&optional (center 0.0)
                        (scale 1.0))
       (declare (special *ieee.not.available*))
       (if (or (eql center *ieee.not.available*)
               (eql scale *ieee.not.available*))
           *ieee.not.available*
           (let (v1 v2 (s 2.0))
                (do nil
                    ((< s 1.0))
                  (setq v1 (- (random 2.0)
                              1.0))
                  (setq v2 (- (random 2.0)
                              1.0))
                  (setq s (+ (* v1 v1)
                             (* v2 v2))))
                (+ center (* scale v1 (sqrt (/ (* -2.0 (log s))
                                               s)))))))

(defun er.sin (x)
       (declare (special *ieee.not.available*))
       (if (eq x *ieee.not.available*)
           *ieee.not.available*
           (sin x)))

(defun er.sqrt (x)
       (declare (special *ieee.not.available*))
       (if (or (eq x *ieee.not.available*)
               (< x 0.0))
           *ieee.not.available*
           (sqrt x)))

(defun er.trunc (x)
       (declare (special *ieee.not.available*))
       (if (eq x *ieee.not.available*)
           *ieee.not.available*
           (float (truncate x))))

(defun er.unary-minus (x)
       (declare (special *ieee.not.available*))
       (if (eq x *ieee.not.available*)
           *ieee.not.available*
           (- x)))

(defun er.^ (x y)
       (declare (special *ieee.not.available*))
       (if (or (eq x *ieee.not.available*)
               (eq y *ieee.not.available*))
           *ieee.not.available*
           (if (< x 0.0)
               (if (not (eql y (float (truncate y))))
                   *ieee.not.available*
                   (expt x (truncate y)))
               (expt x y))))

(defun er.~= (x y)
       (declare (special *ieee.not.available*
                         *Z.boolean.not.available*))
       (if (or (eq x *ieee.not.available*)
               (eq y *ieee.not.available*))
           *Z.boolean.not.available*
           (not (eql x y))))

(defun es.< (x y)
       (declare (special *Z.string.not.available*
                         *Z.boolean.not.available*))
       (if (or (eq x *Z.string.not.available*)
               (eq y *Z.string.not.available*))
           *Z.boolean.not.available*
           (string< x y)))

(defun es.= (x y)
       (declare (special *Z.string.not.available*
                         *Z.boolean.not.available*))
       (if (or (eq x *Z.string.not.available*)
               (eq y *Z.string.not.available*))
           *Z.boolean.not.available*
           (string= x y)))

(defun es.> (x y)
       (declare (special *Z.string.not.available*
                         *Z.boolean.not.available*))
       (if (or (eq x *Z.string.not.available*)
               (eq y *Z.string.not.available*))
           *Z.boolean.not.available*
           (string> x y)))

(defun es.concatenate (x y)
       (declare (special *Z.string.not.available*))
       (if (or (eq x *Z.string.not.available*)
               (eq y *Z.string.not.available*))
           *Z.string.not.available*
           (concatenate 'string x y)))

(defun es.~= (x y)
       (declare (special *Z.string.not.available*
                         *Z.boolean.not.available*))
       (if (or (eq x *Z.string.not.available*)
               (eq y *Z.string.not.available*))
           *Z.boolean.not.available*
           (not (string= x y))))

(defun qgauss (p)
       

;;; 
;;; Rational approximation to the Gaussian inverse distribution function.
;;; Return Xp s.t. P{quail<= Xp} = P Abramowitz and Stegun 26.3.23
;;; 

       (setq p (float p))
       (if (or (> p 1.0)
               (< p 0.0))
           (quail-error "Illegal argument ~S" p)
           (prog (pp tt num denom xp)
                 (setq pp (if (< p 0.5)
                              p
                              (- 1.0 p)))
                 (setq tt (sqrt (* -2.0 (log pp))))
                 (setq num (+ (* (+ (* 0.010328 tt)
                                    0.802853)
                                 tt)
                              2.515517))
                 (setq denom
                       (+ (* (+ (* (+ (* 0.001308 tt)
                                      0.189269)
                                   tt)
                                1.432788)
                             tt)
                          1.0))
                 (setq xp (- tt (/ num denom)))
                 (return (if (< p 0.5)
                             (- xp)
                             xp)))))