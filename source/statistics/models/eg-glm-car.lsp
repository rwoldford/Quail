(in-package q-user)

;; ***********  GAMMA FAMILY with RECIPROCAL LINK  ***********

;; See McCullagh and Nelder, Generalized Linear Models, 2nd ed.
;; This dataset and an analysis is given in section 8.4.1.

(set-quail-data-directory (probe-file "q:examples;statistics;models;"))

;; to undo:  (set-quail-data-directory nil :reset t)

(load (quail-file-pathname "full-factorial-design.lisp"))

;; car insurance data
;; column 0 is the average amount of claims
;; column 1 is the corresponding number of claims

(setf cars-data (array (scan "car insurance data")
                       :dimensions '(t 2)))

(setf vehicle-age '("0-3" "4-7" "8-9" "10+"))
(setf policy-holder-age '("17-20" "21-24" "25-29" "30-34"
                          "35-39" "40-49" "50-59" "60+"))
(setf car-group '(a b c d))

(setf car-cov (full-factorial-design vehicle-age
                                     policy-holder-age
                                     car-group))


;; when the number of claims, which are used as weights, is zero, then
;; the term will be automatically dropped from the likelihood

(setf cars-dat (data-frame (list (array (ref car-cov t 0)
                                        :class 'factor-array
                                        :levels vehicle-age)
                                 (array (ref car-cov t 1)
                                        :class 'factor-array
                                        :levels policy-holder-age)
                                 (array (ref car-cov t 2)
                                        :class 'factor-array
                                        :levels car-group)
                                 (ref cars-data t 0)
                                 (ref cars-data t 1))
                           (list "v_age"
                                 "ph_age"
                                 "group"
                                 "amount"
                                 "number")))

(setf c0 (glm "amount ~ 1"
              cars-dat
              :family :gamma
              :link :reciprocal
              :weight (eref cars-dat "number")
              :tolerance 1e-3))
(display c0)

(setf c1 (glm "amount ~ v_age + ph_age + group"
              cars-dat
              :family :gamma
              :link :reciprocal
              :weight (eref cars-dat "number")
              :tolerance 1e-3))
(display c1)

;; next is a lengthy process if you've got less than about 16 Meg
;; of real RAM  ...  enjoy your coffee.
;;
;; [ We haven't exactly optimized any code, yet. :-) ]

(setf c2 (glm "amount ~ v_age * ph_age * group - v_age . ph_age . group"
              cars-dat
              :family :gamma
              :link :reciprocal
              :weight (eref cars-dat "number")
              :tolerance 1e-3))
