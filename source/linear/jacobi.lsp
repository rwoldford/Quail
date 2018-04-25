;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          jacobi.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1994-97 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Carsten Whimster 1997.
;;;
;;;
;;;-----------------------------------------------------------------------------
;;;

(in-package :q)

(defun jacobi-hook (a d v &optional (nrot 50))
  "This function takes a real symmetric matrix, and returns eigenvalues
   and eigenvectors. Taken virtually verbatum from 'Numerical Recipes in Fortran,
   2nd Edition'.
   The parameters are as follows:
   A     (input) The real symmetric matrix, n x n.
   D     (input/output) An array of dimension n. Will be overwritten with the
                        eigenvalues of A. These are in no particular order.
   V     (input/output) An array of dimension n x n which contains, on output,
                        the eigenvectors of A in its columns.
   NROT  (input/output) A limit on the number of rotations allowed. Defaults to
                        50 which may possibly never be exceeded in real life."

  (prog* (;; parameters
          ;; integers
          (i 0)
          (n (nrows a))
          ;; reals
          (c 0.0D0) (g 0.0D0) (h 0.0D0) (s 0.0D0) (sm 0.0D0) (t_ 0.0D0)
          (tau 0.0D0) (theta 0.0D0) (tresh 0.0D0)
          (b (array 0.0D0 :dimensions (list n)))
          (z (array 0.0D0 :dimensions (list n)))
          (rot-limit nrot))
    
    ;; initialize matrix to identity
    ;; and initialize b and d to the diagonal of a
    (do ((ip 1 (incf ip)))
        ((> ip n))
      (setf (fref v ip ip) 1.0D0)
      (setf (fref b ip) (fref a ip ip))
      (setf (fref d ip) (fref b ip))
      (setf (fref z ip) 0.0D0))
    
    (setf nrot 0)
    (do ((i 1 (incf i)))
        ((> i rot-limit))
      (setf sm 0.0D0)
      (do ((ip 1 (incf ip)))
          ((= ip n))
        (do ((iq (+ ip 1) (incf iq)))
            ((> iq n))
          (setf sm (+ sm (abs (fref a ip iq))))))
      (when (< sm 1D-5)
        (return))
      (if (< i 4)
        (setf tresh (/ (* 0.2 sm) (* n n)))
        (setf tresh 0.0D0))
      (do ((ip 1 (incf ip)))
          ((= ip n))
        (do ((iq (+ ip 1) (incf iq)))
            ((> iq n))
          (setf g (* 100.0D0 (abs (fref a ip iq))))
          (if (and (> i 4)
                   (= (+ (abs (fref d ip)) g) (abs (fref d ip)))
                   (= (+ (abs (fref d iq)) g) (abs (fref d iq))))
            (setf (fref a ip iq) 0.0D0)
            (when (> (abs (fref a ip iq)) tresh)
              (setf h (- (fref d iq) (fref d ip)))
              (cond ((= (+ (abs h) g) (abs h))
                     (setf t_ (/ (fref a ip iq) h)))
                    (t
                     (setf theta (/ (* 0.5D0 h) (fref a ip iq)))
                     (setf t_ (/ 1.0D0 (+ (abs theta)
                                          (sqrt (+ 1.0D0 (* theta theta))))))
                     (when (< theta 0.0D0)
                       (setf t_ (- t_)))))
              (setf c (/ 1.0D0 (sqrt (+ 1.0D0 (* t_ t_)))))
              (setf s (* t_ c))
              (setf tau (/ s (+ 1.0D0 c)))
              (setf h (* t_ (fref a ip iq)))
              (setf (fref z ip) (- (fref z ip) h))
              (setf (fref z iq) (+ (fref z iq) h))
              (setf (fref d ip) (- (fref d ip) h))
              (setf (fref d iq) (+ (fref d iq) h))
              (setf (fref a ip iq) 0.0D0)
              (do ((j 1 (incf j)))
                  ((= j ip))
                (setf g (fref a j ip))
                (setf h (fref a j iq))
                (setf (fref a j ip) (- g (* s (+ h (* g tau)))))
                (setf (fref a j iq) (+ h (* s (- g (* h tau))))))
              (do ((j (+ ip 1) (incf j)))
                  ((= j iq))
                (setf g (fref a ip j))
                (setf h (fref a j iq))
                (setf (fref a ip j) (- g (* s (+ h (* g tau)))))
                (setf (fref a j iq) (+ h (* s (- g (* h tau))))))
              (do ((j (+ iq 1) (incf j)))
                  ((> j n))
                (setf g (fref a ip j))
                (setf h (fref a iq j))
                (setf (fref a ip j) (- g (* s (+ h (* g tau)))))
                (setf (fref a iq j) (+ h (* s (- g (* h tau))))))
              (do ((j 1 (incf j)))
                  ((> j n))
                (setf g (fref v j ip))
                (setf h (fref v j iq))
                (setf (fref v j ip) (- g (* s (+ h (* g tau)))))
                (setf (fref v j iq) (+ h (* s (- g (* h tau))))))
              (setf nrot (+ nrot 1)))
            )
          ))
      (do ((ip 1 (incf ip)))
          ((> ip n))
        (setf (fref b ip) (+ (fref b ip) (fref z ip)))
        (setf (fref d ip) (fref b ip))
        (setf (fref z ip) 0.0D0))
      )
    
    ;; return the modified values with an error message
    (when (= i rot-limit)
      (format *terminal-io* "~&jacobi: too many iterations~%"))
    (return (values a d v)))
    )
