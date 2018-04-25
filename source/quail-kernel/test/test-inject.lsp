;;; ****************************** test-inject.lisp ***************************

(setf a (array '(2 3) :initial-contents '((1.2 3 4.5)
                                          (6.7 8.9 0.1))))

(setf c (array '(2 1) :initial-contents '((100.0) (200.0))))

(setf d '(0 10 20 30 40 50 60 70 80))

(setf e (array '(3 2) :initial-contents '((0 0) (1 1) (0 2))))

(setf g (array '(2 3 4) :initial-contents '(((1.1 2.2 3.3 4.4)
                                             (5.5 6.6 7.7 8.8)
                                             (9.9 11.0 12.1 13.2))
                                            ((10 20 30 40)
                                             (50 60 70 80)
                                             (90 100 110 120)))))

;------------------------------------------------------------------------------

(inject '(1 2 3) t #'Q+ :init 0)

(inject (vector 1 2 3) t #'new+)

(inject 10 t #'new+)

(inject 10 nil #'new+)

(defun sum (r &optional (margin nil))
  (let ((not-margin (if (atom margin) 
                      (not margin) 
                      (mapcar #'not margin))))
      (inject r not-margin #'new+)))

(ref g)

(sum g)

(sum g '(t nil nil))

(sum g '(t t nil))

;; This one blows up ... FIND OUT WHY !!!!
;(sum g '(t nil t))

(sum g '(nil t nil))






