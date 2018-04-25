(load "common:start.lisp")
;; A quick and dirty test of most of the distributions.
;; for more detailed tests, see the 'distribution-name.test' files.
;; continuous: Ward
;; Discrete, FM: Joe

;; *********** Continuous Distribution *************************

(setf test-student (make-instance 'student :degrees 10))
(pdf-at test-student .5)
(cdf-at test-student .5)
;;; both pts passed to illinois have fcn vals > or <(quantile-at test-student .7)

(random-value test-student)

(setf test-normal  (make-instance 'normal))
(pdf-at test-normal .5)
(cdf-at test-normal .5)
;; ERROR (quantile-at test-normal .5)
(random-value test-normal)

(setf test-cauchy  (make-instance 'cauchy))
(pdf-at test-cauchy .5)
(cdf-at test-cauchy .5)
(quantile-at test-cauchy .5)
(random-value test-cauchy)

(setf test-gamma   (make-instance 'gamma   :alpha 1))
(pdf-at test-gamma .5)
(cdf-at test-gamma .693148)
(quantile-at test-gamma .5)
(random-value test-gamma)

(setf test-gamma   (make-instance 'gamma   :alpha .5))
(pdf-at test-gamma .5)
(cdf-at test-gamma .22747)
(quantile-at test-gamma .5)
(random-value test-gamma)

(setf test-chi-squared (make-instance 'chi-squared :degrees 10))
(pdf-at test-chi-squared .5)
(cdf-at test-chi-squared .5) 
;; returns two vals??
(quantile-at test-chi-squared .5)
(random-value test-chi-squared)

(setf test-exponential (make-instance 'exponential))
(pdf-at test-exponential .5)
(cdf-at test-exponential .5)
(quantile-at test-exponential .5)
(random-value test-exponential)

(setf test-uniform (make-instance 'uniform :upper-bound 10))
(pdf-at test-uniform .5)
(cdf-at test-uniform .5)
(quantile-at test-uniform .5)
(random-value test-uniform)

(setf test-beta (make-instance 'beta :shape1 1
                                     :shape2 1))
                                     
(pdf-at test-beta .5)
(cdf-at test-beta .5)
(quantile-at test-beta .5)
(random-value test-beta)

(defclass gen (continuous)
()
)

(defmethod  pdf-at ((distribution gen) x)
  (let ((N (make-instance 'normal)))
        (pdf-at N x)))

(<- test-gen (make-instance 'gen ))

(pdf-at test-gen .25)
(cdf-at test-gen 0)
(quantile-at test-gen .25)
(random-value test-gen)


(defclass gen2 (continuous)
()
)
(defmethod  cdf-at ((distribution gen2) x)
 (let ((N (make-instance 'normal)))
        (cdf-at N x)))


(<- test-gen2 (make-instance 'gen2 :lower-bound 0 :upper-bound .5))
(pdf-at test-gen2 4)
(cdf-at test-gen2 4)
(quantile-at test-gen2 .25)
(random-value test-gen2)

(diff-cdf test-beta .2 .8)



;; *********** Discrete Distributions*************************
;;
;;************************************************************

;;;------------Discrete General Method-----------------------------------------


(defclass gen1 (discrete)
  ()
)

(defmethod pdf-at ((distribution gen1) x)
  (/ (expt 0.5 x) (* (exp 0.5) (factorial x)))
)

(<- test-gen1 (make-instance 'gen1 :lower-bound 0 :upper-bound 5))
(pdf-at test-gen1 3)
(cdf-at test-gen1 1)
(quantile-at test-gen1 0.3)
(random-value test-gen1)



(defclass gen2 (discrete)
  ()
)

(defmethod cdf-at ((distribution gen2) x)
  (cond ((< x 0) 0)
        ((= x 0) 0.1)
        ((= x 1) 0.4)
        ((= x 2) 0.5)
        ((= x 3) 0.75)
        ((= x 4) 0.8)
        ((>= x 5) 1))
)

(<- test-gen2 (make-instance 'gen2 :lower-bound 0 :upper-bound 5))
(pdf-at test-gen2 3)
(cdf-at test-gen2 3)
(quantile-at test-gen2 0.3)
(random-value test-gen2)

;;;------------Binomial Distribution-------------------------------------------
(setf test-binomial (make-instance 'binomial :upper-bound 10 :p .4))
 ;Testing
 (pdf-at test-binomial 10)
 (cdf-at test-binomial 7)
 (quantile-at test-binomial .5)
 (random-value test-binomial)


;;;-------------------Bernoulli Distribution------------------
(setf distribution (make-instance 'bernoulli :p 0.5))  
 ;Testing
 (pdf-at distribution 0)
 (pdf-at distribution 1)
 (cdf-at distribution 0)
 (quantile-at distribution 0.7)
 (quantile-at distribution 0.3)
 (quantile-at distribution 0.5)
 (random-value distribution)


;;;------------Poisson Distribution---------------------------
(setf test-poisson (make-instance 'poisson :mean 5))
 ;Testing
 (pdf-at test-poisson 7)
 (cdf-at test-poisson 7)
 (quantile-at test-poisson .5)
 (random-value test-poisson)

;;;-------------------Hypergeometric Distribution-------------
(setf test-hyper (make-instance 'hypergeometric
                                :N1 10
                                :N2 10
                                :n 10
                                :upper-bound 10))
 ;Testing
 (pdf-at test-hyper 5)
 (cdf-at test-hyper 10)
 (quantile-at test-hyper 0.4)
 (random-value test-hyper)


;;;-------------------Finite-Discrete-Uniform-----------------
(setf test-UD (make-instance 'finite-discrete-uniform
                         :lower-bound 0
                         :upper-bound 10))
 ;Testing
 (pdf-at test-UD 5)
 (cdf-at test-UD 4)
 (quantile-at test-UD 0.5)
 (random-value test-UD)

;;;------------------Negative-Binomial Distribution-----------
(setf random-variable (make-instance 'negative-binomial
                                      :x 5
                                      :p 0.5))
 ;Testing
 (pdf-at random-variable 6)
 (cdf-at random-variable 6)
 (quantile-at random-variable 0.4)
 (random-value random-variable)
;;;-------------------Geometric Distribution------------------
(setf distribution (make-instance 'geometric :p 0.5))
 ;Testing
 (pdf-at distribution 5)
 (cdf-at distribution 5)
 (quantile-at distribution 0.9)
 (random-value distribution)




;; *********** Finite Discrete Mixture************************
;;
;;************************************************************

 ; An instance for discrete variates
 (setf F1 (make-instance 'finite-discrete-uniform
                          :lower-bound 0
                          :upper-bound 10))
 (setf F2 (make-instance 'binomial
                          :p 0.5))
 ; An instance for continuous variates
 (setf F1 (make-instance 'chi-squared
                          :degrees 5))
 (setf F2 (make-instance 'exponential
                          :mean 5))

(setf F3 (make-instance 'discrete-mixture
                         :mixing-probs (list 0.3 0.7)
                         :distributions (list F1 F2)
                         :num 2))
 ;Testing
 (pdf-at F3 1)
 (cdf-at F3 9)
 (Dquantile-at F3 0.5)
 (Cquantile-at F3 0.5)
 (random-value F3)
 
