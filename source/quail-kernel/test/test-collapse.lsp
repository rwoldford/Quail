;;; **************************** test-collapse.lisp ***************************

(in-package :quail-user)

(setf a (array '(2 3) :initial-contents '((1.2 3 4.5)
                                          (6.7 8.9 0.1))))

;------------------------------------------------------------------------------

(collapse 5 nil #'(lambda (x) (* x x)))

#|

(collapse "abcde" nil #'reverse)

(collapse "abcde" t #'(lambda (x) (make-string 3 :initial-element x)))

|#

(collapse a t #'tp)

(collapse a '(nil t) #'tp)

(collapse a '(t) #'tp)

(collapse a nil #'tp)

(collapse a t #'ref-list)

(collapse a '(nil t) #'ref-list)

(collapse a '(t) #'ref-list)

(collapse a nil #'ref-list)






