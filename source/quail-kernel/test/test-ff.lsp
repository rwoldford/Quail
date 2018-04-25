;;; ******************* test-ff.lisp ***********************************

(in-package :quail)

(load "ccl;Z:Z:zffi-load.lisp")

(import 'zffi::peek)

;------------------------------------------------------------------------------

(setf aa (array '(1) :type 'f-real*8
                     :class 'f-array
                     :language :fortran
                     :initial-element 0))

(peek aa)

(setf ab (array '(2 3 4) :type 'f-real*8
                         :class 'f-array
                         :language :fortran
                         :initial-element 1))

(progn
  (loop for i from 0 to 1 do
        (loop for j from 0 to 2 do
              (loop for k from 0 to 3 do
                    (setf (eref ab i j k) (sqrt (* (+ i 2) (+ j 3) (+ k 4)))))))
  ab)

(peek ab)

(setf ac (array '(2 3 4) :type 'f-integer 
                         :class 'f-array
                         :language :fortran
                         :initial-contents '(((1 2 3 4)
                                              (5 6 7 8)
                                              (9 10 11 12))
                                             ((13 14 15 16)
                                              (17 18 19 20)
                                              (21 22 23 24)))))

(ref ac t 0 t)

(setf ad (array '(2 3) :type 'f-real*8
                       :class 'f-array
                       :language :fortran 
                       :initial-contents '((1.7893 2.4678 3.8976)
                                           (4.1256 5.9876 6.3487))))

(setf ae (ref ad '(0 1 0 1 0 1) '(0 1 2 0 1 2 0 1 2)))

(setf af (tp ae))

(setf ag (array '(2 3 4) :type 'f-real*8
                         :class 'f-array
                         :language :c
                         :initial-contents '(((1 2 3 4)
                                              (5 6 7 8)
                                              (9 10 11 12))
                                             ((13 14 15 16)
                                              (17 18 19 20)
                                              (21 22 23 24)))))

(setf a0 '(((210 220 230 240)
            (350 360 370 380)
            (90 100 110 120))
           ((1.1 2.2 3.3 4.4)
            (5.5 6.6 7.7 8.8)
            (9.9 11.0 12.1 13.2))))

(make-array '(2 3 4) :initial-contents a0)

(setf ah (array '(2 3 4) :type 'f-real*8
                         :class 'f-array
                         :language :fortran
                         :initial-contents '(((1.1 2.2 3.3 4.4)
                                              (5.5 6.6 7.7 8.8)
                                              (9.9 11.0 12.1 13.2))
                                             ((10 20 30 40)
                                              (50 60 70 80)
                                              (90 100 110 120)))))

(setf ai (array '(5 3 3) :type 'f-real*8
                         :class 'f-array
                         :language :c
                         :initial-contents '((( 1  2  3)
                                              ( 4  5  6)
                                              ( 7  8  9))
                                             ((10 11 12)
                                              (13 14 15)
                                              (16 17 18))
                                             ((19 20 21)
                                              (22 23 24)
                                              (25 26 27))
                                             ((28 29 30)
                                              (31 32 33)
                                              (34 35 36))
                                             ((37 38 39)
                                              (40 41 42)
                                              (43 44 45)))))





