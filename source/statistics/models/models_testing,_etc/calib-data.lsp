(in-package q)

(setf calib (data-frame (list (array '(10) :initial-contents
                                     '(4.0 8.0 12.5 16.0 20.0 25.0
                                       31.0 36.0 40.0 40.0))
                              (array '(10) :initial-contents
                                     '(3.7 7.8 12.1 15.6 19.8 24.5 
                                       31.1 35.5 39.4 39.5))
                              (array '(10 1) :initial-contents
                                     '((3.7) (7.8) (12.1) (15.6) (19.8) (24.5) 
                                       (31.1) (35.5) (39.4) (39.5))))
                        (list "x" "y" "ymat")))

(setf f1 (lm "y ~ x" calib))
(setf f2 (lm "y ~ -1 + x" calib))
(setf f3 (lm "ymat ~ -1 + x" calib))