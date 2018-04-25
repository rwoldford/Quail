(in-package :quail)

(setf d1 (data-frame (list '(1 2 3 4)
                           '(11 19 29 40)
                           (array '(4 2) :initial-contents '((67 58) (93 24)
                                                             (20 21) (22 23)))
                           (array '(4)
                                  :class 'factor-array
                                  :initial-contents '("Hi" "Lo" "Lo" "Hi")
                                  :levels '("Lo" "Hi"))
                           (array '(4) 
                                  :class 'factor-array
                                  :initial-contents '("Foo" "Bar" "Baz" "Foo")
                                  :levels '("Foo" "Bar" "Baz")))
                     (list "x" "y" "z" "u" "w")))

(setf m1 (model 'generalized-additive-model "y ~ x + {(* x x)}"))

(setf m2 (model 'generalized-additive-model "y ~ x + {(* z[t 0] z[t 1])}"))

(setf m3 (model 'generalized-additive-model 
                "y ~ xx{(* x x)} + xz{(+ (* x z[t 0]) (* x z[t 1]))}"))

(setf m4 (model 'generalized-additive-model 
                "y ~ x * w * u - x . u"))

(setf m5 (model 'generalized-additive-model
                "y ~ x - 1"))

(setf fit1 (fit-instantiate m1 d1 :maximum-likelihood))

(setf fit2 (fit-instantiate m2 d1 :maximum-likelihood))

(setf fit3 (fit-instantiate m3 d1 :maximum-likelihood))

(setf fit4 (fit-instantiate m4 d1 :maximum-likelihood))

(setf fit5 (fit-instantiate m5 d :maximum-likelihood))

(setf d2 (data-frame (list (array '(6 2) 
                                  :initial-contents '((10 30) (11 30) (11 29)
                                                      (28 12) (32 8) (29 9)))
                           (array '(6) :initial-contents '(10 10 20 20 20 10))
                           (array '(6 2) 
                                  :initial-contents '((10 30) (30 11) (11 29)
                                                      (28 12) (32 8) (9 29)))
                           (array '(6)
                                  :initial-contents '(0 0 0 1 1 1)
                                  :class 'factor-array
                                  :levels '(0 1))
                           (array '(6)
                                  :initial-contents '(1 2 3 4 5 6))
                           (array '(6)
                                  :initial-contents '(11 19 28 39 53 55))
                           (array '(6) :class 'ones-array))
                     (list "y1" "y2" "y3" "x" "y4" "x4" "ones")))

(setf w1 (array '(6) :initial-contents '(3 5 5 3 5 5)))

(inspect 
 (setf g1 
       (glm "y1 ~ ones + x" d2 :family binomial :link logit-link :tolerance .001)))

(setf g2 (glm "y2 ~ x"
              d2
              :family gaussian
              :tolerance .001))

(setf lm1 (lm "y4 ~ x4" d2 :weight w1))

