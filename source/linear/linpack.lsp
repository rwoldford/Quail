;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                        linpack.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; copyright (c) 1991 statistical computing laboratory, university of waterloo
;;;
;;;
;;;  authors:
;;;     greg anglin, michael lewis 1991.
;;;     Carsten Whimster 1996-97.
;;;
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(dasum daxpy dchdc dcopy ddot  dgeco
          dgedi dgefa dgels dgesl dpoco dpodi
          dpofa dposl dnrm2 dqrdc dqrls dqrsl
          drot  drotg dscal dsico dsifa dsidi
          dsisl dsvdc dswap dtrdi dtrco dtrls
          dtrsl idamax jacobi)))

(defun dasum (n dx incx)
  (let ()
    (dasum-hook n dx incx)))

(defun daxpy (n da dx incx dy incy)
  (let ()
    (daxpy-hook n da dx incx dy incy)))

(defun dchdc (a p work jpvt job info)
  (let ((lda (nth 0 (dimensions-of a))))
    (dchdc-hook a lda p work jpvt job info)))

(defun dcopy (n dx incx dy incy)
  (let () 
    (dcopy-hook n dx incx dy incy)))

(defun ddot (n dx incx dy incy)
  (let () 
    (ddot-hook n dx incx dy incy)))

(defun dgeco (a n ipvt rcond z)
  (let ((lda (nth 0 (dimensions-of a))))
    (dgeco-hook a lda n ipvt rcond z)))

(defun dgefa (a n ipvt info)
  (let ((lda (nth 0 (dimensions-of a))))
    (dgefa-hook a lda n ipvt info)))

(defun dgedi (a n ipvt det work job)
  (let ((lda (nth 0 (dimensions-of a))))
    (dgedi-hook a lda n ipvt det work job)))

(defun dgels (a ipvt b job)
  (let* ((n (first (dimensions-of a)))
         (dim-b (dimensions-of b))
         (m (if (= (length dim-b) 1) 1 (second dim-b))))
    (dgels-hook a n n ipvt b m job)
    b))

(defun dgesl (a n ipvt b job)
  (let ((lda (nth 0 (dimensions-of a))))
    (dgesl-hook a lda n ipvt b job)))

(defun dnrm2 (n dx incx)
  (let () 
    (dnrm2-hook n dx incx)))

(defun dpoco (a n rcond z info)
  (let ((lda (nth 0 (dimensions-of a))))
    (dpoco-hook a lda n rcond z info)))

(defun dpodi (a n det job)
  (let ((lda (nth 0 (dimensions-of a))))
    (dpodi-hook a lda n det job)))

(defun dpofa (a n info)
  (let ((lda (nth 0 (dimensions-of a))))
    (dpofa-hook a lda n info)))

(defun dposl (a n b)
  (let ((lda (nth 0 (dimensions-of a))))
    (dposl-hook a lda n b)))

(defun dqrdc (x n p qraux jpvt work job)
  (let ((ldx (nth 0 (dimensions-of x))))
    (dqrdc-hook x ldx n p qraux jpvt work job)))

(defun dqrls (qr qraux y &key rank qy qty coef resid pred job info)
  (let* ((dim-qr (dimensions-of qr))
         (n (first dim-qr))
         (p (or (second dim-qr) 1))
         (k (or rank (min n p)))
         (dim-y (dimensions-of y))
         (m (or (second dim-y) 1)))
    (dqrls-hook qr n n k qraux y p m qy qty coef resid pred job info)
    (eref info)))

(defun dqrsl (x n k qraux y qy qty b rsd xb job info)
  (let ((ldx (nth 0 (dimensions-of x))))
    (dqrsl-hook x ldx n k qraux y qy qty b rsd xb job info)))

(defun drot (n dx incx dy incy c s)
  (let () 
    (drot-hook n dx incx dy incy c s)))

(defun drotg (da db c s) 
  (let () 
    (drotg-hook da db c s)))

(defun dscal (n da dx incx)
  (let () 
    (dscal-hook n da dx incx)))

(defun dsico (a n kpvt rcond z)
  (let ((lda (nth 0 (dimensions-of a))))
    (dsico-hook a lda n kpvt rcond z)))

(defun dsifa (a n kpvt info)
  (let ((lda (nth 0 (dimensions-of a))))
    (dsifa-hook a lda n kpvt info)))

(defun dsidi (a n kpvt det inert work job)
  (let ((lda (nth 0 (dimensions-of a))))
    (dsidi-hook a lda n kpvt det inert work job)))

(defun dsisl (a n kpvt b)
  (let ((lda (nth 0 (dimensions-of a))))
    (dsisl-hook a lda n kpvt b)))

(defun dsvdc (x n p s e u v work job info)
  (let ((ldx (nth 0 (dimensions-of x)))
        (ldu (nth 0 (dimensions-of u)))
        (ldv (nth 0 (dimensions-of v))))
    (dsvdc-hook x ldx n p s e u ldu v ldv work job info)))

(defun dswap (n dx incx dy incy)
  (let ()
    (dswap-hook n dx incx dy incy)))

(defun dtrco (tt n rcond z job)
  (let ((ldt (nth 0 (dimensions-of tt))))
    (dtrco-hook tt ldt n rcond z job)))

(defun dtrdi (tt n det job info)
  (let ((ldt (nth 0 (dimensions-of tt))))
    (dtrdi-hook tt ldt n det job info)))

(defun dtrls (a b job)
  (let* ((dim-a (dimensions-of a))
         (dim-b (dimensions-of b))
         (lda (first dim-a))
         (n (second dim-a))
         (m (if (= (length dim-b) 1) 1 (second dim-b)))
         (info (array 0 :dimensions 'nil)))
    (dtrls-hook a lda n b m job info)))

(defun dtrsl (tt n b job info)
  (let ((ldt (nth 0 (dimensions-of tt))))
    (dtrsl-hook tt ldt n b job info)))

(defun idamax (n dx incx)
  (let ()
    (idamax-hook n dx incx)))

(defun jacobi (a d v &optional nrot)
  (if nrot
    (jacobi-hook a d v nrot)
    (jacobi-hook a d v)))

;;;
;;; The following four functions manipulate positive definite banded matrices.
;;;

(defun dpbco (abd lda n m rcond z info)
  (let ()
    (dpbco-hook abd lda n m rcond z info)))

(defun dpbfa (abd lda n m info)
  (let ()
    (dpbfa-hook abd lda n m info)))

(defun dpbdi (abd lda n m det)
  (let ()
    (dpbdi-hook abd lda n m det)))

(defun dpbsl (abd lda n m b)
  (let ()
    (dpbsl-hook abd lda n m b)))

;;;
;;; The following four functions manipulate banded matrices.
;;;

(defun dgbco (abd lda n ml mu ipvt rcond z)
  (let ()
    (dgbco-hook abd lda n ml mu ipvt rcond z)))

(defun dgbfa (abd lda n ml mu ipvt info)
  (let ()
    (dgbfa-hook abd lda n ml mu ipvt info)))

(defun dgbdi (abd lda n ml mu ipvt det)
  (let ()
    (dgbdi-hook abd lda n ml mu ipvt det)))

(defun dgbsl (abd lda n ml mu ipvt b job)
  (let ()
    (dgbsl-hook abd lda n ml mu ipvt b job)))
