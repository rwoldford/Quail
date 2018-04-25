(setf foo1 (make-instance 'interval :infimum 7 :supremum 23))
(setf foo2 (make-instance 'interval :infimum 34 :supremum 47 :closure-property :LEFT))
(setf foo3 (make-instance 'interval :infimum 2 :supremum 20))
(setf foo4 (make-instance 'interval :infimum 22 :supremum 32))


;;;interval EFUs
(setf foo (set-union foo1 foo3))
(setf foo (set-union foo1 foo2))
(setf bar (set-intersection foo1 foo2))
(setf bar (set-union foo3 foo4))
(setf dill (set-union bete 5))
(setf mill (set-union bete 2))
(setf bete
      (make-instance 'predicate-set
            :infimum 5
            :supremum 17
            :closure-property :open
            :defining-predicate
            #'(lambda (test-member)
                (and (< 5 test-member)
                     (< test-member 17)))))
(setf tete
      (make-instance 'predicate-set
            :infimum 8
            :supremum 23
            :closure-property :open
            :defining-predicate
            #'(lambda (test-member)
                (and (< 8 test-member)
                     (< test-member 23)))))

(describe foo)
(describe bar)

(setf fox (set-union foo foo))
(setf fox (set-union foo bar))
(setf whix (set-intersection foo bar))
(setf foo (set-union foo2 foo3))
(setf whix (set-intersection foo bar))
(setf whix (op-set-intersection dill mill))
(setf scull (set-complement foo bar))
(setf foo (set-union foo2 foo3))
(setf scull (set-complement foo bar))
(setf scull (op-set-complement dill mill))  ;why does address change predicate set

(describe fox)
(describe whix)
(describe scull)

(setf foo5 (make-instance 'interval :infimum 41 :supremum 72))
(setf bar (set-union foo3 foo4 foo5))

;;; non-interval EFU
;;(setf foo (make-instance 'borel-set :infimum 7 :supremum 23))
(setf bar (set-union foo 25))

(setf fox (set-union foo bar))
(setf whix (set-intersection foo bar))
(setf scull (set-complement foo bar))

;;; predicate-sets
(setf sete
      (make-instance 'predicate-set
            :infimum -infinity
            :supremum +infinity
            :closure-property :closed
            :defining-predicate
            #'(lambda (test-member)
                (extended-integerp test-member))))

(setf bete
      (make-instance 'predicate-set
            :infimum 5
            :supremum 17
            :closure-property :open
            :defining-predicate
            #'(lambda (test-member)
                (and (< 5 test-member)
                     (< test-member 17)))))

(setf fox (set-union sete bete))
(setf whix (set-intersection sete bete))
(setf scull (set-complement bete sete))
(memberp 17 fox)
(memberp 6 fox)
(memberp infinity fox)
(memberp 17 whix) 
(memberp 6 whix)
(memberp infinity whix)
(memberp 17 scull)
(memberp 6 scull)
(memberp infinity scull)

;;; complement-sets
(setf alpha (make-instance 'complement-set :set foo3 :wrt foo1))
(setf gamma (set-complement 5 foo3))

(setf fox (set-union alpha gamma))
(setf whix (set-intersection alpha gamma))
(setf scull (set-complement alpha gamma))
(memberp 17 fox)
(memberp 6 fox)
(memberp infinity fox)
(memberp 17 whix)
(memberp 6 whix)
(memberp infinity whix)
(memberp 17 scull)
(memberp 6 scull)
(memberp infinity scull)

(setf mix1 (set-union foo1 bete))
(setf mix2 (set-union foo1 gamma))
(setf mix3 (set-union bete gamma))
(memberp 17 mix1)
(memberp 6 mix1)
(memberp infinity mix1)
(memberp 17 mix2)
(memberp 6 mix2)
(memberp infinity mix2)
(memberp 17 mix3)
(memberp 6 mix3)
(memberp infinity mix3)

(setf mix1 (set-intersection foo1 bete))
(setf mix2 (set-intersection foo1 gamma))
(setf mix3 (set-intersection bete gamma))
(memberp 17 mix1)
(memberp 6 mix1)
(memberp infinity mix1)
(memberp 17 mix2)
(memberp 6 mix2)
(memberp infinity mix2)
(memberp 17 mix3)
(memberp 6 mix3)
(memberp infinity mix3)

(setf mix1 (set-complement foo1 bete))
(setf mix2 (set-complement foo1 gamma))
(setf mix3 (set-complement bete gamma))
(memberp 17 mix1)
(memberp 6 mix1)
(memberp infinity mix1)
(memberp 17 mix2)
(memberp 6 mix2)
(memberp infinity mix2)
(memberp 17 mix3)
(memberp 6 mix3)
(memberp infinity mix3)


(setf doo (set-intersection mix1 gamma))
(describe doo)

(setf moo (set-complement 6 doo))
(describe moo)

