;;;objects
(setf bor (make-instance 'borel-set))
(setf empty (make-instance 'empty-set))
(setf foo2 (make-instance 'interval :infimum 34 :supremum 47 
                          :closure-property :LEFT))
(setf comp (make-instance 'complement-set :set 8 :wrt foo2))
(setf comp3 (make-instance 'complement-set :set 9 :wrt bor))
(setf sete
      (make-instance 'predicate-set
            :infimum -infinity
            :supremum +infinity
            :closure-property :closed
            :defining-predicate
            #'(lambda (test-member)
                (extended-integerp test-member))))
(setf foo1 (make-instance 'interval :infimum 7 :supremum 23))
(setf foo (set-union foo1 foo2))
(setf bor2 (make-instance 'borel-set :closure-property :left-open))
(setf foo3 (make-instance 'interval :infimum 7 :supremum 20))
(setf foo4 (make-instance 'interval :infimum 22 :supremum 32))
(setf foo5 (make-instance 'interval :infimum 32 :supremum 35 :closure-property :closed))
(setf foo6 (make-instance 'interval :infimum 5 :supremum 32 :closure-property :right-closed))
(setf foo7 (make-instance 'interval :infimum 32 :supremum 35 :closure-property :left-open))
(setf foo12 (make-instance 'interval :infimum 32 :supremum 35))
(setf comp4 (make-instance 'complement-set :set foo2 :wrt 40))
(setf foo8 (make-instance 'interval :infimum 33 :supremum 34 :closure-property :closed))
(setf comp5 (make-instance 'complement-set :set foo8 :wrt foo7))
(setf foo9 (make-instance 'interval :infimum 1 :supremum 50))
(setf foo10 (make-instance 'interval :infimum 100 :supremum 102))
(setf foo (set-union foo3 foo4))
(setf foo11 (make-instance 'interval :infimum 7 :supremum 35))
(setf foo (set-union foo3 foo7))
(setf foo3 (copy-set foo3 :closure-property :left-closed))

(setf comp2 (set-complement 10 foo3))
(setf foo6 (make-instance 'interval :infimum 5 :supremum 15))
(setf comp3 (set-complement 10 foo6))
(setf (set-of comp2) 7)
(setf bete
      (make-instance 'predicate-set
            :infimum 5
            :supremum 17
            :closure-property :open
            :defining-predicate
            #'(lambda (test-member)
                (and (< 5 test-member)
                     (< test-member 17)))))

(setf efs (make-instance 'explicit-finite-set :contents '(4 8 64 77 90)))
(setf efs2 (make-instance 'explicit-finite-set :contents '(55 33 24 28)))
(setf efs3 (make-instance 'explicit-finite-set :contents '(9 19 17 13)))
(setf efs4 (make-instance 'explicit-finite-set :contents '(4 19 64 77 91)))
(setf efs5 (make-instance 'explicit-finite-set :contents '(6 8 11 12 15)))
(setf efs6 (make-instance 'explicit-finite-set :contents '(8 10)))


;;; extended reals

(describe (set-union 3 4))
(describe (set-union 3 3))
(describe (set-intersection 4 4))
(describe (set-intersection 3 4))
(describe (set-complement 3 4))
(describe (set-complement 3 3))

(setf bor (make-instance 'borel-set))
(describe (set-union 3 bor))   ;;; faults in memberp
(describe (set-intersection bor 3))
(describe (set-complement 3 bor))
(describe (set-complement bor 3))

(setf empty (make-instance 'empty-set))
(describe (set-union 3 empty))
(describe (set-intersection empty 3))
(describe (set-complement 3 empty))
(describe (set-complement empty 3))

(setf foo2 (make-instance 'interval :infimum 34 :supremum 47 
                          :closure-property :LEFT))
(describe (set-union 3 foo2))
(describe (set-union foo2 40))
(describe (set-intersection 3 foo2))
(describe (set-intersection foo2 40))
(describe (set-complement 3 foo2))
(describe (set-complement foo2 40))
(describe (set-complement 40 foo2))
(describe (set-complement foo2 3))

(setf comp (make-instance 'complement-set :set 8 :wrt foo2))
(setf comp3 (make-instance 'complement-set :set 9 :wrt bor))
(describe (set-union 3 comp))
(describe (set-union comp 8))
(describe (set-union comp 3))
(describe (set-union 7 comp2))
(describe (set-intersection 3 comp))
(describe (set-intersection comp 8))
(describe (set-intersection comp 40))
(describe (set-complement 3 comp))
(describe (set-complement 8 comp))
(describe (set-complement 40 comp))
(describe (set-complement comp 3))
(describe (set-complement comp 40))
(describe (set-complement comp 8))

(setf sete
      (make-instance 'predicate-set
            :infimum -infinity
            :supremum +infinity
            :closure-property :closed
            :defining-predicate
            #'(lambda (test-member)
                (extended-integerp test-member))))
(describe (set-union 3 sete))
(describe (set-union sete 3.3))
(describe (set-intersection 3 sete))
(describe (set-intersection sete 3.3))
(describe (set-complement 3 sete))
(describe (set-complement 3.3 sete))
(describe (set-complement sete 3))
(describe (set-complement sete 3.3))

(setf foo1 (make-instance 'interval :infimum 7 :supremum 23))
(setf foo (set-union foo1 foo2))
(describe (set-union 3 foo))
(describe (set-intersection 3 foo))
(describe (set-intersection foo 8))
(describe (set-complement 3 foo))
(describe (set-complement 8 foo))
(describe (set-complement foo 3))
(describe (set-complement foo 8))


;;; borel-sets
(setf bor2 (make-instance 'borel-set :closure-property :left-open))
(describe (set-union bor bor2))
(describe (set-intersection bor bor2))
(describe (set-complement bor bor2))

(describe (set-union bor empty))
(describe (set-intersection bor empty))
(describe (set-complement bor empty))
(describe (set-complement empty bor))

(describe (set-union bor foo2))
(describe (set-intersection bor foo2))
(describe (set-complement bor foo2))
(describe (set-complement foo2 bor))

(describe (set-union bor comp))
(describe (set-intersection bor comp))
(describe (set-complement bor comp))
(describe (set-complement comp bor))  ;;; error - memberp and reals & borel

(describe (set-union bor sete))
(describe (set-intersection bor sete))
(describe (set-complement bor sete))
(describe (set-complement sete bor))

(describe (set-union bor foo))
(describe (set-intersection bor foo))
(describe (set-complement bor foo))
(describe (set-complement foo bor))

;;; empty-sets
(describe (set-union empty empty))
(describe (set-intersection empty empty))
(describe (set-complement empty empty))

(describe (set-union empty foo2))
(describe (set-intersection empty foo2))
(describe (set-complement empty foo2))
(describe (set-complement foo2 empty))

(describe (set-union empty comp))
(describe (set-intersection empty comp))
(describe (set-complement empty comp))
(describe (set-complement comp empty))

(describe (set-union empty sete))
(describe (set-intersection empty sete))
(describe (set-complement empty sete))
(describe (set-complement sete empty))

(describe (set-union empty foo))
(describe (set-intersection empty foo))
(describe (set-complement empty foo))
(describe (set-complement foo empty))

;;; intervals
(setf foo3 (make-instance 'interval :infimum 7 :supremum 20))
(setf foo4 (make-instance 'interval :infimum 22 :supremum 32))
(setf foo5 (make-instance 'interval :infimum 32 :supremum 35 :closure-property :closed))
(setf foo6 (make-instance 'interval :infimum 5 :supremum 32 :closure-property :right-closed))
(setf foo7 (make-instance 'interval :infimum 32 :supremum 35 :closure-property :left-open))
(setf foo12 (make-instance 'interval :infimum 32 :supremum 35))
(describe (set-union foo4 foo2))
(describe (set-union foo4 foo4))
(describe (set-union foo1 foo3))
(describe (set-union foo4 foo5))  ;just touching
(describe (set-intersection foo4 foo2))
(describe (set-intersection foo1 foo3))
(describe (set-intersection foo4 foo5))
(describe (set-intersection foo5 foo6))
(describe (set-complement foo4 foo2))
(describe (set-complement foo2 foo4))
(describe (set-complement foo5 foo7))
(describe (set-complement foo7 foo5))
(describe (set-complement foo3 foo6))
(describe (set-complement foo12 foo5))

(describe (set-union foo1 foo3))
(describe (set-intersection foo1 foo3))
(describe (set-complement foo1 foo3))
(describe (set-complement foo3 foo1))

(describe (set-union foo3 comp))
(describe (set-union comp foo2))
(setf comp4 (make-instance 'complement-set :set foo2 :wrt 40))
(describe (set-union comp4 foo2))
(describe (set-intersection foo3 comp))
(describe (set-intersection comp foo2))
(setf foo8 (make-instance 'interval :infimum 33 :supremum 34 :closure-property :closed))
(describe (set-intersection comp foo8))
(setf comp5 (make-instance 'complement-set :set foo8 :wrt foo7))
(describe (set-intersection comp4 foo7))
(describe (set-complement foo3 comp))
(describe (set-complement foo2 comp))
(describe (set-complement foo7 comp5))
(describe (set-complement foo8 comp5))
(describe (set-complement comp foo3))
(setf foo9 (make-instance 'interval :infimum 1 :supremum 50))
(describe (set-complement comp5 foo9))
(describe (set-complement comp5 foo5))

(describe (set-union foo3 sete))
(describe (set-intersection foo3 sete))
(describe (set-complement foo3 sete))
(describe (set-complement sete foo3))

(describe (set-union foo3 foo))
(describe (set-intersection foo3 foo))
(describe (set-intersection foo foo8))
(setf foo10 (make-instance 'interval :infimum 100 :supremum 102))
(describe (set-intersection foo foo10))
(describe (set-intersection foo9 foo))
(describe (set-complement foo3 foo))
(describe (set-complement foo1 foo))
(describe (set-complement foo9 foo))
(describe (set-complement foo10 foo))
(setf foo (set-union foo3 foo4))
(describe (set-complement foo6 foo))
(setf foo11 (make-instance 'interval :infimum 7 :supremum 35))
(setf foo (set-union foo3 foo7))
(describe (set-complement foo11 foo))
(setf foo3 (copy-set foo3 :closure-property :left-closed))

(describe (set-complement foo foo3))
(describe (set-complement foo foo10))
(describe (set-complement foo foo9))


;;;complement-sets

(setf comp2 (set-complement 10 foo3))
(describe (set-union comp2 comp))
(describe (set-intersection comp2 comp))
(describe (set-complement comp2 comp))
(describe (set-complement comp comp2))

(setf foo6 (make-instance 'interval :infimum 5 :supremum 15))
(setf comp3 (set-complement 10 foo6))
(describe (set-intersection comp2 comp3))
(describe (set-complement comp2 comp3))
(describe (set-complement comp3 comp2))
(setf (set-of comp2) 7)
(describe (set-intersection comp2 comp3))
(describe (set-complement comp2 comp3))
(describe (set-complement comp3 comp2))

(describe (set-union comp2 sete))
(describe (set-intersection comp2 sete))
(describe (set-intersection sete comp2))
(describe (set-complement comp2 sete))
(describe (set-complement sete comp2))

(describe (set-union comp2 foo))
(describe (set-intersection comp2 foo))
(describe (set-intersection foo comp2))
(describe (set-complement comp2 foo))
(describe (set-complement foo comp2))

;;; predicate-sets
(setf bete
      (make-instance 'predicate-set
            :infimum 5
            :supremum 17
            :closure-property :open
            :defining-predicate
            #'(lambda (test-member)
                (and (< 5 test-member)
                     (< test-member 17)))))

(describe (set-union bete sete))
(describe (set-intersection bete sete))
(describe (set-complement bete sete))
(describe (set-complement sete bete))

(describe (set-union bete foo))
(describe (set-intersection bete foo))
(describe (set-complement bete foo))
(describe (set-complement foo bete))


;;; explicit-finite-sets
(setf efs (make-instance 'explicit-finite-set :contents '(4 8 64 77 90)))
(setf efs2 (make-instance 'explicit-finite-set :contents '(55 33 24 28)))
(setf efs3 (make-instance 'explicit-finite-set :contents '(9 19 17 13)))
(setf efs4 (make-instance 'explicit-finite-set :contents '(4 19 64 77 91)))
(setf efs5 (make-instance 'explicit-finite-set :contents '(6 8 11 12 15)))
(setf efs6 (make-instance 'explicit-finite-set :contents '(8 10)))
(setf efs7 (make-instance 'explicit-finite-set :contents '(11 8 10)))


(insidep efs foo3)
(insidep efs3 foo3)
(insidep efs3 foo)
(insidep foo3 efs)
(insidep efs3 comp2)

(describe (set-union efs 13))
(describe (set-union efs efs2))
(describe (set-union foo3 efs3))
(describe (set-union efs foo3))
(describe (set-union efs bor))
(describe (set-union efs empty))
(describe (op-set-union empty efs))
(describe (set-union efs comp))
(describe (set-union comp efs))
(describe (set-union efs sete))
(describe (set-union bete efs5))
(describe (set-union efs5 bete))
(describe (set-union sete efs))
(describe (set-union efs foo))


(describe (set-intersection 13 efs3))
(describe (set-intersection 14 efs))
(describe (set-intersection efs foo1))
(describe (set-intersection foo1 efs2))
(describe (set-intersection foo1 efs5))
(describe (set-intersection efs bor))
(describe (set-intersection efs empty))
(describe (set-intersection efs efs4))
(describe (set-intersection efs3 efs4))
(describe (set-intersection efs efs3))
(describe (set-intersection efs comp))
(describe (set-intersection efs comp2))
(describe (set-intersection comp2 efs6))
(describe (op-set-intersection bete efs5)) ;prefers pred to efs method
(describe (op-set-intersection efs5 bete))
(describe (set-intersection efs foo))
(describe (set-intersection efs6 foo))  ;;again prefers other method
(describe (set-intersection efs2 foo))



;START HERE ON THE NEXT WORK DAY

(describe (set-complement 64 efs4))
(describe (set-complement 78 efs4))
(describe (set-complement efs4 64))
(describe (set-complement efs4 77))
(describe (setf efs6 (set-complement 8 efs6)))
(describe (set-complement bor efs))
(describe (set-complement efs bor))
(describe (op-set-complement efs empty))
(describe (set-complement empty efs))
(describe (set-complement efs foo1))
(describe (set-complement efs3 foo1))
(describe (set-complement efs2 foo1))
(describe (set-complement foo1 efs))
(describe (set-complement foo1 efs3))
(describe (set-complement foo2 efs2))
(describe (set-complement efs efs2))
(describe (set-complement efs3 efs4))
(describe (set-complement efs efs))
(describe (op-set-complement efs6 efs7))
(describe (set-complement comp efs))
(describe (set-complement efs comp))

(describe (set-complement efs sete)) ;;; results in complement-set
                                     ;;; would it be better as a
                                     ;;; predicate-set?
(describe (set-complement sete efs))
(describe (set-complement efs bor))
(describe (set-complement bor efs))

(describe (op-set-complement efs foo))
(describe (set-complement foo efs))


(describe (copy-set foo1))
(describe (copy-set bor :infimum 5))
(describe (copy-set foo :contents (list (elt (contents-of foo) 0) empty)))
(describe (copy-set bete :infimum 2 :supremum 18))




