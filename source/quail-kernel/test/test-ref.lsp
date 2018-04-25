;;; ********************************** test-ref.lisp ******************************

(in-package :quail-user)

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

;----------------------------------------------------------------------------------

(setf h (ref g (array '(2 3 3) :initial-contents '(((0 1 2) (1 0 2) (1 2 0))
                                                   ((1 0 1) (1 1 2) (1 2 3))))))

(setf hh (ref h e))

(setf hhh (ref hh '(0 2)))

(ref a)
(ref a t 1)
(ref a t 1 :shape t)

(ref c)
(ref c :shape t)
(ref c :shape '(t nil))
(ref c :shape '(nil t))

(setf (ref a t 1) c)
(ref a)

(setf (ref a 0 0) 555)
(sel a 0)

(setf (sel a t 1) '(111.11 222.22))
a

(setf (ref a 1) '(11 22 33))
(ref a)

(setf d '(0 10 20 30 40 50 60 70 80))
(ref d '(2 3))
(ref d '(:c 4 6))
(ref (ref d '(:c 4 6)) '(3 4))               
(setf (ref d '((2) (3))) c)
d

;
;   This type of ref ("by indices") basically works, but the interaction of this
;   with the previous ref ("by margin") is not yet complete.  For now, it's 
;   probably best to ignore "by indices" ref.
;

a
(ref a '((0 0) (1 1) (0 2)))
(setf e (make-array '(3 2) :initial-contents '((0 0) (1 1) (0 2))))
(ref a e)
(setf (ref a e) '(100.0 200.0 300.0))
a

;------------------------------------------------------------------------------

;
;  Here is the difference between sel and ref.  Note that even if two things
;  _print_ identically, they do not necessarily behave the same way.
;
;  Since ref identifies a place, it works with setf.  By contrast, sel copies
;  to another object, so setf does not work on sel.
;

a
(setf b (sel a t 1))
(setf (ref b 1) 17)
a

a
(setf b (ref a t 1))
(setf (ref b 1) 17)
a
