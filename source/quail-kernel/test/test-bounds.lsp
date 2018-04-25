;;; ******************* test-arrays.lisp ***********************************

(in-package :quail)

(load "ccl;Z:zffi-load.lisp")

;------------------------------------------------------------------------------

(setf a (array '(2 3) :initial-contents '((1.2 3 4.5)
                                          (6.7 8.9 0.1))))

(setf b (array '(2 3) 'f-real*8 :type :f
                                :language :fortran
                                :initial-contents a))

;--------------------------------------------------------------------------------

(setf (bounds-checking b) nil)

(eref b 0 0)
(eref b 2 3)
(eref b -1 2)                 ; oops
(eref b -1 -2)
(eref b 4 0)                  ; oops

(setf (bounds-checking b) t)

(eref b 0 0)
(eref b 2 3)
(eref b -1 2)
(eref b -1 -2)
(eref b 4 0)







