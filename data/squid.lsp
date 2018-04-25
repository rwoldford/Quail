
(in-package :q-user)

(defvar squids NIL
  "An experiment was conducted in order to study the size of ~
   squid eaten by sharks. The regresor variables are characteristics of the beak ~
   or mouth of the squid. The regressor variables and response  ~
   considered for the study are ~&~
   x1: rostral length in inches ~&~
   x2: wing length in inches ~&~
   x3: rostral to wing length ~&~
   x4 notch to wing length ~&~
   x5 width in inches ~&~
   y weight in pounds")

(<- squid-vars (list "rostral" "wing" "rostral-wing"
                     "notch-wing" "width" "weight"))
(<- squids
    (array
     '((1.31 1.07 0.44 0.75 0.35 1.95)
      (1.55 1.49 0.53 0.90 0.47 2.90)
      (0.99 0.84 0.34 0.57 0.32 0.72)
      (0.99 0.83 0.34 0.54 0.27 0.81)
      (1.05 0.90 0.36 0.64 0.30 1.09)
      (1.09 0.93 0.43 0.61 0.31 1.22)
      (1.08 0.90 0.40 0.51 0.31 1.02)
      (1.27 1.08 0.44 0.77 0.34 1.93)
      (0.99 0.85 0.36 0.56 0.29 0.64)
      (1.34 1.13 0.45 0.77 0.37 2.08)
      (1.30 1.10 0.45 0.76 0.38 1.98)
      (1.33 1.10 0.48 0.77 0.38 1.90)
      (1.86 1.47 0.60 1.01 0.65 8.56)
      (1.58 1.34 0.52 0.95 0.50 4.49)
      (1.97 1.59 0.67 1.20 0.59 8.49)
      (1.80 1.56 0.66 1.02 0.59 6.17)
      (1.75 1.58 0.63 1.09 0.59 7.54)
      (1.72 1.43 0.64 1.02 0.63 6.36)
      (1.68 1.57 0.72 0.96 0.68 7.63)
      (1.75 1.59 0.68 1.08 0.62 7.78)
      (2.19 1.86 0.75 1.24 0.72 10.15)
      (1.73 1.67 0.64 1.14 0.55 6.88))
     :dimensions '(22 6))
    )

(dataset squids :variates squid-vars :name "Shark eaten squids")
