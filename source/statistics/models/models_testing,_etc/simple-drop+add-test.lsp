(in-package q)

(setq x0 (list 1 2 3 4 5 1 2 3 4 5))
(setq x1 (* x0 x0))
(setq x (cglue x0 x1))

(setq y (+ 3 (* 4 x0) (* 5 x1) (random-gaussian :n (first (dimensions-of x0))
                                                :location 0.0
                                                :scale 2)))

(setq d (data-frame (list y x) (list "y" "x")))

(setq fit1 (lm "y ~ x" d))

(setq fit2 (lm "y ~ x[t 0]" d))

(setq fit3 (lm "y ~ x[t (:c 1)]" d))

;; fails ... too complicated to fix, though ...
(setq fit4 (lm "y ~ x - x[t 0]" d))

;;;;

(setq x2 (array (list 1 2 2 3 3 1 2 2 3 3) :class 'factor-array :levels '(1 2 3)))

(setq d2 (data-frame (list y x0 x1 x2) (list "y" "x0" "x1" "x2")))

(setq fit5 (lm "y ~ x0 + x2 + x0.x2" d2))

(setq fit6 (drop fit5 "x2.x0"))

(setq fit7 (drop fit6 "x2"))

;;

(setq fit8 (add fit7 "x2"))

(setq fit9 (add fit8 "x2.x0"))

;;;  something a little bit different!

(setq fit10 (drop fit5 "x0"))

(setq fit11 (add fit10 "x0"))