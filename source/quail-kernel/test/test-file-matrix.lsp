;;; ************************** test-file-matrix.lisp *********************************

(setf f (array '(5 3) :class 'file-matrix :file "test1" :initial-element 5))

(setf g (ref f t '(1 2)))

(describe g)

(setf h (sel f t '(1 2)))

(setf ff (array '(5 5) :class 'file-matrix :file "test2" :initial-element 3))

;-------------------------------------------------------------------------------------

#|
(scan-reset "test1.fmat")

(setf new-f (array '(10 3) :class 'file-matrix :file "test3" :initial-contents :scan))
|#
