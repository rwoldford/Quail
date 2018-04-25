(setf foo
      (make-instance 'explicit-finite-collection))
(describe foo)
(funcall (position-from-index-function-of foo) 1)
(funcall (index-from-position-function-of foo) 1)
(add-sets foo 400 500 600 300 200 100 0 700)
(contains-set-p foo 0)
(contains-set-p foo 4)
(contains-set-p foo 123)
(funcall (index-function-of foo) foo 200)
(funcall (index-function-of foo) foo 12)
(remove-sets foo 16 600 14)
(remove-sets foo 0 2 4)
(remove-sets-if foo #'(lambda (a) (> a 499)))
(describe foo)
(delete-sets foo 16 21 14)
(delete-sets foo 6 2 4)
(delete-sets-if foo #'(lambda (a) (> a 3)))
(sorted-collection-p foo)
(collection-sort foo #'<)

(describe (copy-set foo))

(setf bar (make-instance 'explicit-finite-collection
                         :contents '( 2 541 3 4 1)))
(describe bar)
(funcall (element-function-of bar) bar 1)
(collection-sort bar #'>)
(setf foobar (combine-collections foo bar))
(describe foobar)


(setf barfoo (combine-collections bar foo))
(describe barfoo)

;;;;;;;;;;;;;;;
;;;  Testing countable-collections

(setf cc-1 (make-instance 'countable-collection))
(describe cc-1)

(setf (element-function-of cc-1)
      #'(lambda (collection index)
          (declare (ignore collection))
          index))

(setf cc-2 (make-instance 'countable-collection))
(describe cc-2)

(setf (element-function-of cc-2)
      #'(lambda (collection index)
          (declare (ignore collection))
          (- index)))

(contains-set-p cc-1 3)
(funcall (index-function-of cc-1) cc-1 3)
(count-start-of cc-1)
(contains-set-p cc-1 +infinity)
(loop for i from 0 to 20
  do (print (funcall (element-function-of cc-1) cc-1 i)))
(loop for i from 0 to 20
  do (print (funcall (element-function-of cc-2) cc-2 i)))

(setf cc-1-2 (combine-collections cc-1 cc-2))
(describe cc-1-2)
(setf ith-set-of (element-function-of cc-1-2))
(funcall ith-set-of cc-1-2 0)
(funcall ith-set-of cc-1-2 1)
(funcall ith-set-of cc-1-2 20)
(loop for i from 0 to 20  collect (funcall ith-set-of cc-1-2 i))

(setf foo-cc-1 (combine-collections foo cc-1))
(setf cc-1-foo (combine-collections cc-1 foo))
(describe foo-cc-1)
(setf ith-set-of (element-function-of foo-cc-1))
(funcall (element-function-of foo) foo (/ 10 2))
(funcall ith-set-of foo-cc-1 0)
(funcall ith-set-of foo-cc-1 1)
(funcall ith-set-of foo-cc-1 2)
(funcall ith-set-of foo-cc-1 3)
(funcall ith-set-of foo-cc-1 16)
(funcall ith-set-of foo-cc-1 17)
(funcall ith-set-of foo-cc-1 18)
(loop for i from 1 to 31 by 2
  do (print (funcall ith-set-of foo-cc-1 i)))
(setf ith-set-of (element-function-of cc-1-foo))
(funcall ith-set-of cc-1-foo 0)
(funcall ith-set-of cc-1-foo 1)
(funcall ith-set-of cc-1-foo 2)
(funcall ith-set-of cc-1-foo 3)
(funcall ith-set-of cc-1-foo 16)
(funcall ith-set-of cc-1-foo 17)
(funcall ith-set-of cc-1-foo 18)
(funcall ith-set-of cc-1-foo +infinity)


