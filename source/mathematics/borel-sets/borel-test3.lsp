;;;countable-unions

(setf cu1 (make-instance 'countable-union
            :from 1
            :to +infinity
            :element-function
            #'(lambda (collection index)
                (declare (ignore collection))
                (make-instance 'interval
                  :closure-property :closed
                  :infimum index
                  :supremum (+ index 5)))
            ))

(setf bug (make-instance 'interval :infimum 7 :supremum 12 :closure-property 
                         :open))
(setf bug (make-instance 'interval :infimum 7 :supremum 12 :closure-property 
                         :closed))

(contains-set-p cu1 bug)
(disjoint-sets-p bug cu1)
(memberp 100 cu1)
(memberp -3 cu1)
(insidep bug cu1)
(insidep cu1 bug)
(describe (set-union 10 cu1))
(describe (set-union cu1 -10))
(describe (set-union bor cu1))
(describe (set-union cu1 bor))
(describe (op-set-union empty cu1))
(describe (set-union cu1 empty))
(describe (op-set-union cu1 foo1))
(describe (op-set-union foo1 cu1))
(describe (op-set-union comp cu1))
(describe (op-set-union bete cu1))
(describe (op-set-union cu1 bete))
(describe (op-set-union cu1 cu1))

(setf cu2 (set-union cu1 bug))  ;;should it do this? (i)

(same-set-p cu1 cu2)
(describe (set-union cu1 cu2))
(describe (set-union cu1 foo))
(describe (set-union foo cu1))
(setf foo13 (make-instance 'interval :infimum 5 :supremum 10 :closure-property
:closed))
(setf foo14 (make-instance 'interval :infimum 20 :supremum 25 :closure-property
:closed))
(setf bar (set-union foo13 foo14))
(describe (setf bear (op-set-union bar cu1)))
(describe (setf racoon (set-union cu1 bar)))
(describe (set-union efs cu1))
(describe (set-union cu1 efs))

(describe (op-set-intersection 10 cu1))
(describe (op-set-intersection cu1 -10))
(describe (op-set-intersection bor cu1))
(describe (op-set-intersection cu1 bor))
(describe (op-set-intersection empty cu1))
(describe (op-set-intersection cu1 empty))
(describe (op-set-intersection cu1 bug))
(describe (op-set-intersection bug cu1))
(describe (op-set-intersection comp cu1))
(describe (op-set-intersection cu1 comp))
(describe (op-set-intersection bete cu1))
(describe (op-set-intersection cu1 bete))
(describe (op-set-intersection cu1 cu1))

(describe (op-set-intersection cu1 foo))
(describe (op-set-intersection foo cu1))
(describe (op-set-intersection efs cu1))
(describe (op-set-intersection cu1 efs))



(describe (op-set-complement 10 cu1))
(describe (op-set-complement -10 cu1))
(describe (op-set-complement cu1 -10))
(describe (op-set-complement bor cu1))
(describe (op-set-complement cu1 bor))
(describe (op-set-complement empty cu1))
(describe (op-set-complement cu1 empty))
(describe (op-set-complement cu1 bug))
(describe (op-set-complement bug cu1))
(describe (op-set-complement comp cu1))
(describe (op-set-complement cu1 comp))
(describe (op-set-complement bete cu1))
(describe (op-set-complement cu1 bete))
(describe (op-set-complement cu1 cu1))

(describe (op-set-complement cu1 foo))
(describe (op-set-complement foo cu1))
(describe (op-set-complement efs cu1))
(describe (op-set-complement cu1 efs))


;;;countable-intersections

(setf ci1 (make-instance 'countable-intersection
            :from 1
            :to +infinity
            :element-function
            #'(lambda (collection index)
                (declare (ignore collection))
                (make-instance 'interval
                  :closure-property :closed
                  :infimum index
                  :supremum (+ index 5)))
            ))

(contains-set-p ci1 bug)
(disjoint-sets-p bug ci1)
(memberp 100 ci1)
(memberp -3 ci1)
(insidep bug ci1)
(insidep ci1 bug)
(setf ci2 (set-union ci1 bug))
(same-set-p ci1 ci2) ;is that an appropriate error?
(setf comp (set-complement bug ci1))   ;predicate-sets?
(setf comp2 (set-complement ci1 bug))
(describe (set-intersection bug ci1))

(describe (set-union 10 ci1))
(describe (set-union ci1 -10))
(describe (set-union bor ci1))
(describe (set-union ci1 bor))
(describe (op-set-union empty ci1))
(describe (set-union ci1 empty))
(describe (op-set-union ci1 foo1))
(describe (op-set-union foo1 ci1))
(describe (op-set-union comp ci1))
(describe (op-set-union ci1 comp))
(describe (op-set-union bete ci1))
(describe (op-set-union ci1 cu1))
(describe (op-set-union cu1 ci1))
(describe (op-set-union ci1 bete))

(describe (op-set-union ci1 ci1))
(describe (set-union ci1 cu2))
(describe (set-union ci1 foo))
(describe (set-union foo ci1))
(describe (set-union efs ci1))
(describe (set-union ci1 efs))

(describe (op-set-intersection 10 ci1))
(describe (op-set-intersection ci1 10000))
(describe (op-set-intersection bor ci1))
(describe (op-set-intersection ci1 bor))
(describe (op-set-intersection empty ci1))
(describe (op-set-intersection ci1 empty))
(describe (op-set-intersection ci1 bug))
(describe (op-set-intersection bug ci1))
(describe (op-set-intersection comp ci1))
(describe (op-set-intersection ci1 comp))
(describe (op-set-intersection bete ci1))
(describe (op-set-intersection ci1 bete))
(describe (op-set-intersection ci1 ci1))
(describe (op-set-intersection cu1 ci1))
(describe (op-set-intersection ci1 cu1))

(describe (op-set-intersection ci1 foo))
(describe (op-set-intersection foo ci1))
(describe (op-set-intersection efs ci1))
(describe (op-set-intersection ci1 efs))



(describe (op-set-complement 10 ci1))
(describe (op-set-complement -10 ci1))
(describe (op-set-complement ci1 -10))
(describe (op-set-complement ci1 10))
(describe (op-set-complement bor ci1))
(describe (op-set-complement ci1 bor))
(describe (op-set-complement empty ci1))
(describe (op-set-complement ci1 empty))
(describe (op-set-complement ci1 bug))
(describe (op-set-complement bug ci1))
(describe (op-set-complement comp ci1))
(describe (op-set-complement ci1 comp))
(describe (op-set-complement bete ci1))
(describe (op-set-complement ci1 bete))
(describe (op-set-complement ci1 ci1))

(describe (op-set-complement ci1 foo))
(describe (op-set-complement foo ci1))
(describe (op-set-complement efs ci1))
(describe (op-set-complement ci1 efs))
(describe (op-set-complement foo ci1))
(describe (op-set-complement ci1 foo))

;;;countable-sets
(setf natural-numbers
      (make-instance 'countable-set
            :infimum 0
            :supremum +infinity
            :begin-index 0
            :end-index +infinity
            :element-function
            #'(lambda (set index)
                (declare (ignore set))
                index)
            :defining-predicate
            #'(lambda (set test-member)
                (declare (ignore set))
                (and (extended-integerp test-member)
                     (> test-member 0)))))

(contains-set-p natural-numbers -41)
(contains-set-p natural-numbers 4)
(contains-set-p natural-numbers 304)
(disjoint-sets-p 6 natural-numbers)
(disjoint-sets-p -41 natural-numbers)
(op-memberp 4 natural-numbers)
(memberp -3 natural-numbers)
(insidep bug natural-numbers)
(insidep 5 natural-numbers)
(insidep -34 natural-numbers)
(insidep natural-numbers bug)
(setf cs2 (set-union natural-numbers 2))
(same-set-p natural-numbers cs2)
(setf comp (set-complement bug natural-numbers))   ;predicate-sets?
(setf comp2 (set-complement natural-numbers bug))
(setf efi (set-intersection bug natural-numbers))  ;7 8 9 10 11 12


(describe (set-union 10 natural-numbers))
(describe (set-union natural-numbers -10))
(describe (set-union bor natural-numbers))
(describe (set-union natural-numbers bor))
(describe (op-set-union empty natural-numbers))
(describe (op-set-union natural-numbers empty))
(describe (op-set-union natural-numbers foo1))
(describe (op-set-union foo1 natural-numbers))
(describe (op-set-union comp natural-numbers))
(describe (op-set-union natural-numbers comp))
(describe (op-set-union bete natural-numbers))
(describe (op-set-union natural-numbers bete))
(describe (op-set-union natural-numbers cu1))
(describe (op-set-union cu1 natural-numbers))

(describe (op-set-union natural-numbers natural-numbers))
(describe (set-union natural-numbers cu2))
(describe (op-set-union natural-numbers foo))
(describe (set-union foo natural-numbers))
(describe (set-union efs natural-numbers))
(describe (set-union natural-numbers efs))
(describe (set-union ci1 natural-numbers))
(describe (set-union natural-numbers ci1))

(describe (op-set-intersection 10 natural-numbers))
(describe (op-set-intersection natural-numbers 10000))
(describe (op-set-intersection bor natural-numbers))
(describe (op-set-intersection natural-numbers bor))
(describe (op-set-intersection empty natural-numbers))
(describe (op-set-intersection natural-numbers empty))
(describe (op-set-intersection natural-numbers bug))
(describe (op-set-intersection bug natural-numbers))
(setf bug2 (make-instance 'interval :infimum -10 :supremum -5))
(describe (op-set-intersection natural-numbers bug2))
(describe (op-set-intersection bug2 natural-numbers))
(describe (op-set-intersection comp natural-numbers))
(describe (op-set-intersection natural-numbers comp))
(describe (op-set-intersection bete natural-numbers))
(describe (op-set-intersection natural-numbers bete))
(describe (op-set-intersection natural-numbers natural-numbers))
(describe (op-set-intersection cu1 natural-numbers))
(describe (op-set-intersection natural-numbers cu1))
(describe (op-set-intersection ci1 natural-numbers))
(describe (op-set-intersection natural-numbers ci1))

(describe (op-set-intersection natural-numbers foo))
(describe (op-set-intersection foo natural-numbers))
(describe (op-set-intersection efs natural-numbers))
(describe (op-set-intersection natural-numbers efs))



(describe (op-set-complement 10 natural-numbers))
(describe (op-set-complement -10 natural-numbers))
(describe (op-set-complement natural-numbers -10))
(describe (op-set-complement natural-numbers 10))
(describe (op-set-complement bor natural-numbers))
(describe (op-set-complement natural-numbers bor))
(describe (op-set-complement empty natural-numbers))
(describe (op-set-complement natural-numbers empty))
(describe (op-set-complement natural-numbers bug))
(describe (op-set-complement bug natural-numbers))
(describe (op-set-complement comp natural-numbers))
(describe (op-set-complement natural-numbers comp))
(describe (op-set-complement bete natural-numbers))
(describe (op-set-complement natural-numbers bete))
(describe (op-set-complement natural-numbers natural-numbers))


;;;START HERE LATER
(describe (op-set-complement cu1 natural-numbers))
(describe (op-set-complement natural-numbers cu1))
(describe (op-set-complement ci1 natural-numbers))
(describe (op-set-complement natural-numbers ci1))

(describe (op-set-complement natural-numbers foo))
(describe (op-set-complement foo natural-numbers))
(describe (op-set-complement efs natural-numbers))
(describe (op-set-complement natural-numbers efs))


;;; efis
(setf efi1 (make-instance 'explicit-finite-intersection
             :contents (list ci1 bug)))
(disjoint-sets-p bug efi1)
(memberp 100 efi1)
(memberp -3 efi1)
(insidep bug efi1)
(insidep efi1 bug)
(setf efi2 (op-set-intersection bug efi1))
(same-set-p efi2 efi1)
(setf comp (set-complement efi1 bug))
(setf comp2 (set-complement bug efi1))
(describe (set-union bug efi1))

(disjoint-sets-p efi efi1)
(memberp 9 efi)
(memberp 100 efi)
(memberp -3 efi)
(insidep bug efi)
(insidep efi bug)
(setf efi2 (op-set-intersection bug efi))
(same-set-p efi2 efi)
(setf comp (set-complement efi bug))
(setf comp2 (set-complement bug efi))
(describe (set-union bug efi)) ;predicate-set?

(describe (set-union 10 efi1))
(describe (set-union efi1 -10))
(describe (set-union bor efi1))
(describe (set-union efi1 bor))
(describe (op-set-union empty efi1))
(describe (set-union efi1 empty))
(describe (op-set-union efi1 foo1))
(describe (op-set-union foo1 efi1))
(describe (op-set-union comp efi1))
(describe (op-set-union efi1 comp))
(describe (op-set-union bete efi1))
(describe (op-set-union efi1 cu1))
(describe (op-set-union cu1 efi1))
(describe (op-set-union efi1 bete))
(describe (op-set-union efi1 ci1))
(describe (op-set-union ci1 efi1))
(describe (op-set-union efi1 natural-numbers))
(describe (op-set-union natural-numbers efi1))

(describe (op-set-union efi1 efi1))
(describe (set-union efi1 foo))
(describe (set-union foo efi1))
(describe (set-union efs efi1))
(describe (set-union efi1 efs))

(describe (op-set-intersection 10 efi1))
(describe (op-set-intersection efi1 10000))
(describe (op-set-intersection bor efi1))
(describe (op-set-intersection efi1 bor))
(describe (op-set-intersection empty efi1))
(describe (op-set-intersection efi1 empty))
(describe (op-set-intersection efi1 bug))
(describe (op-set-intersection bug efi1))
(describe (op-set-intersection comp efi1))
(describe (op-set-intersection efi1 comp))
(describe (op-set-intersection bete efi1))
(describe (op-set-intersection efi1 bete))
(describe (op-set-intersection efi1 efi1))
(describe (op-set-intersection cu1 efi1))
(describe (op-set-intersection efi1 cu1))

(describe (op-set-intersection efi1 foo))
(describe (op-set-intersection foo efi1))
(describe (op-set-intersection efs efi1))
(describe (op-set-intersection efi1 efs))
(describe (op-set-intersection ci1 efi1))
(describe (op-set-intersection efi1 ci1))
(describe (op-set-intersection natural-numbers efi1))
(describe (op-set-intersection efi1 natural-numbers))



(describe (op-set-complement 10 efi1))
(describe (op-set-complement -10 efi1))
(describe (op-set-complement efi1 -10))
(describe (op-set-complement efi1 10))
(describe (op-set-complement bor efi1))
(describe (op-set-complement efi1 bor))
(describe (op-set-complement empty efi1))
(describe (op-set-complement efi1 empty))
(describe (op-set-complement efi1 bug))
(describe (op-set-complement bug efi1))
(describe (op-set-complement comp efi1))
(describe (op-set-complement efi1 comp))
(describe (op-set-complement bete efi1))
(describe (op-set-complement efi1 bete))
(describe (op-set-complement efi1 efi1))

(describe (op-set-complement efi1 foo))
(describe (op-set-complement foo efi1))
(describe (op-set-complement efs efi1))
(describe (op-set-complement efi1 efs))
(describe (op-set-complement cu1 efi1))
(describe (op-set-complement efi1 cu1))
(describe (op-set-complement ci1 efi1))
(describe (op-set-complement efi1 ci1))
(describe (op-set-complement natural-numbers efi1))
(describe (op-set-complement efi1 natural-numbers))
(describe (op-set-complement efi1 efi1))
(describe (op-set-complement efi1 cu1))