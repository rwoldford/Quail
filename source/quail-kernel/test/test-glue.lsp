;;; ****************************** test-glue.lisp *****************************

(in-package :quail-user)

(setf a (array '(2 3) :initial-contents '((1.2 3 4.5)
                                          (6.7 8.9 0.1))))

;------------------------------------------------------------------------------

(setf cc (vector 100 200))
(cglue a cc)
(cglue 1 2 3)
(rglue 1 2 3)
(rglue '(1 2) cc)
(cglue '(1 2) cc)
(rglue '(1 2) cc :1d :col)
(cglue '(1 2) cc :1d :row)
