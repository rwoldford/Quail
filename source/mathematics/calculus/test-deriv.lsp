
(in-package :q)

;;  x^2 - 3 - (x + y + z + x) - x^2
;;  deriv = 2x -2 -2x = -2

(deriv '(- (* X X) 3 (+ x y z x) (* x x)) :wrt 'x)

;;  x^2 - 3 - (x + y + z + x) - x^3
;;  deriv = 2x -2 -3x^2

(deriv '(- (* X X) 3 (+ x y z x) (expt x 3)) :wrt 'x)

(simplify '(- (* X X) 3 (+ x y z x) (* x x)))

(simplify '(+ (- x (* X X) 3 (+ x y z ) (expt x 3))
            (- x (* X X) 3 (+ x y z ) (expt x 3))))

(simplify '(- (- x (* X X) 3 (+ x y z ) (expt x 3))
            3
            (- x (* X X) 3 (+ x y z ) (expt x 3))))

(simplify 
 '(- (- x (* X X) 3 (+ x y z ) (expt x 3))
   3
   (- x (* X X) 3 (+ x y z ) (expt x 3))
   (- x (* X X) 3 (+ x y z ) (expt x 3))
   3
   (- x (* X X) 3 (+ x y z ) (expt x 3)))
 )


(setf x 2 y 3 z 4)

(deriv 
 '(- (- x (* X X) 3 (+ x y z ) (expt x 3))
   3
   (/ x (* X X) 3 (/ x y z ) (expt x 3))
   (- x (* X X) 3 (+ x y z ) (expt x 3))
   3
   (- x (* X X) 3 (+ x y z ) (expt x 3)))
 :wrt 'x
 )

(numerical-deriv #'(lambda (x) (- (- x (* X X) 3 (+ x y z ) (expt x 3))
   3
   (/ x (* X X) 3 (/ x y z ) (expt x 3))
   (- x (* X X) 3 (+ x y z ) (expt x 3))
   3
   (- x (* X X) 3 (+ x y z ) (expt x 3))))
                 :x x)

(deriv '(* x 2 (+ x 5) x y) :wrt 'x)

(float 
 (eval
  (deriv '(/ x 2 (+ x 5) x) :wrt 'x)
  ))
(numerical-deriv #'(lambda (x) (/ x 2 (+ x 5) x)) :x x)

(float 
 (eval
  (deriv 
   '(- (- x (* X X) 3 (+ x y z ) (expt x 3))
     3
     (/ x (* X X) 3 (+ x y z ) (expt x 3))
     (- x (* X X) 3 (+ x y z ) (expt x 3))
     3
     (- x (* X X) 3 (+ x y z ) (expt x 3)))
   :wrt 'x)
  ))

(numerical-deriv
 #'(lambda (x) 
     (- (- x (* X X) 3 (+ x y z ) (expt x 3))
        3
        (/ x (* X X) 3 (+ x y z ) (expt x 3))
        (- x (* X X) 3 (+ x y z ) (expt x 3))
        3
        (- x (* X X) 3 (+ x y z ) (expt x 3))))
 :x x)
                 


(simplify '(/ x y z (+ x x)))

(simplify '(+))

(simplify '(+ 2))
(simplify '(+ 2 3 4 x y 1))
                          X Y (+ 10 (+ X Y)))))
(simplify '(+ 10 (+ (+ 10
                       (+ X Y
                          (+ 10
                            (expt  (+ X Y x) (/ y y x)))))
                    X Y
                    (+ 10
                       (+ X Y)))))
(simplify '(+ 10 (+ 10 x)))

(q::flatten-sum '(+ 10 ))

(q::flatten-product '(* 10 (* 2 X (* 2 X (* 2 X)))
                      (* 2 Y) (* 3 10) (* 2 X) (* 2 Y)))
(q::flatten-product '(* 10 
                      (* 2 Y) (* 3 10) (* 2 X) (* 2 Y)))
(q::flatten-product '(* 10  Y x (* 3 x)))
(simplify 'y)

(defun test-simp (xyz-expression)
  (let ((simp-exp (simplify xyz-expression)))
    (eval
     `(loop for x from -2 to 2
            do (loop for y from -2 to 2
                     do (loop for z from -2 to 2
                              do
                              (cond
                               ((not (zerop (- ,xyz-expression ,simp-exp)))
                                (format *terminal-io* "~&Failed.")
                                (format *terminal-io* "~&~%x = ~s  y = ~s z = ~s"
                                        x y z)
                                (format *terminal-io* "~&orig = ~s  simp = ~s"
                                        ,xyz-expression ,simp-exp))
                               (T (format *terminal-io* "~&Passed.")))
                              )
                     )
            
            finally (return :done))
    )))

(defun test-div ()
  (loop for w in '(-2 -1 1 2)
        do 
        (loop for x in '(-2 -1 1 2)
              do 
              (loop for y in '(-2 -1 1 2)
                    do
                    (loop for z in '(-2 -1 1 2)
                          do
                          (cond
                           ((not (zerop (- (/ w x y z) (/ w (* x y z)))))
                            (format *terminal-io* "~&Failed.")
                            (format *terminal-io* "~&~%w = ~s  x = ~s  y = ~s z = ~s"
                                    w x y z)
                            )
                           (T (format *terminal-io* "~&Passed.")))
                          )
                    )
              )
        
        finally (return :done))
  )
(test-div)
(test-simp '(+ 10 (+ (+ 10 (+ X Y (+ 10 (+ X Y))))
                          X Y (+ 10 (+ X Y)))))

(simplify-difference '(- x (* X X)   ))
(setf expression '(- x (expt X 2)   ))
(setf expression '(+ (expt X 2)))
(simplify-difference '(- x (expt X 2)))

(test-simp '(- (- x (* X X) 3 (+ x y z ) (expt x 3))
             3
             (- x (* X X) 3 (+ x y z ) (expt x 3))
             (- x (* X X) 3 (+ x y z ) (expt x 3))
             3
             (- x (* X X) 3 (+ x y z ) (expt x 3))))
                      