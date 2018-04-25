;;; ******************************* test-ops.lisp ******************************

(setf a (array '(2 3) :initial-contents '((1.2 3 4.5)
                                          (6.7 8.9 0.1))))

(setf at (tp a))      ; transpose of a

(setf b (array '(2 3) :initial-contents '((101.2 103.0 104.5) 
                                          (206.7 208.9 200.1))))

(setf c (array '(2 1) :initial-contents '((100.0) (200.0))))

(setf d '(0 10 20 30 40 50 60 70 80))

(setf e (array '(3 2) :initial-contents '((0 0) (1 1) (0 2))))

(setf g (array '(2 3 4) :initial-contents '(((1.1 2.2 3.3 4.4)
                                             (5.5 6.6 7.7 8.8)
                                             (9.9 11.0 12.1 13.2))
                                            ((10 20 30 40)
                                             (50 60 70 80)
                                             (90 100 110 120)))))

;-------------------------------------------------------------------------------

;  new+, z-, new*, Q- do ELEMENT-WISE +, -, *, / in extended real numbers
;  .* does inner product and matrix multiplication
;  these functions are all n-ary

(Q+ a b)                

(Q* a b)

(.* at a)

(Q- d)    

(setf (ref b 0 1) nan)

b

(Q/ b a)

(.* at b)

(Q* d d d)

(.* d d)

(quail-min +infinity 5 0.7 2000)

(quail-min -infinity NaN)

(quail-max +infinity)

;  tp for 0, 1, 2 dimensional things.

(tp d)

;  tp has a more general form for higher dimensional things

(tp a :perm '(1 0))            ; same as (tp a)

(tp g :perm '(2 0 1))




