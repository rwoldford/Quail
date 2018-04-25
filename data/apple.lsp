(in-package :quail-user)

(<- apple-vars
      (list "Block" "TN" "PN" "P" "K" "Ca" "Mg" "WGT" "BPI" "Treatment"))
(<- apple
      (array
      '((1 3580 1790  932  8220 244 410  85.3  0.0 1)
        (1 2880 1670  836  9840 142 367 113.8  3.2 1)
        (1 3260 1530  740  8180 269 387  92.9  0.0 1)
        
        (2 2870 1700  926  7550 272 332  48.9  0.0 1)
        (2 3430 1800  899  9520 202 370  99.4  3.6 1)
        (2 2930 1490  847  8310 272 413  79.1  0.0 1)

        (3 3110 1700  770  8180 297 389  70.0  2.7 1)
        (3 3300 1840  891  8970 225 362  86.9  1.8 1)

        (4 3370 1780  899  9420 212 403  87.7  6.5 1)
        (4 3290 1730  879  7240 206 330  67.3  4.3 1)

        (1 3040 1810  798 10760 138 414 117.5 47.0 2)
        (1 4470 2020  886  9990 151 401  98.9 39.6 2)
        (2 5810 2400 1037 11340 165 497 108.5 44.2 2)
        (2 4610 2070  840  9070 151 351 104.4 19.0 2)
        (2 4690 2070  914  9730 199 429  96.8 10.0 2)

        (3 3010 1780  813  9830 159 403  94.5 18.5 2)
        (3 6740 2310 1111 11150 158 458  90.6  7.3 2)
        (3 4510 2320  912 10360 163 401 100.8 23.6 2)

        (4 4890 2040  925  9550 239 397  96.0  6.5 2)
        (4 4340 1990  915 10440 180 428  99.9 20.4 2)
        (4 4130 1870  710  9040 199 363  84.6  0.0 2)

        (1 4250 2040  932 11830 169 408 127.1  9.5 3)
        (1 3710 1810  792 10530 210 392 108.5  3.9 3)
        (1 4640 2340  883 11210 172 393  99.9  1.6 3)
        (2 6950 2300 1202 12910 148 510 124.8 27.2 3)
        (2 4880 1800  829 11210 219 411  94.5  2.0 3)

        (3 4680 1940  850 11010 224 411  99.4  2.7 3)
        (3 5170 2130  862 11750 152 419 117.5 13.9 3)
        (3 5730 2560 1161 12440 160 454 135.0 50.0 3)

        (4 5360 2000  898 10960 211 428  85.6  3.6 3)
        (4 6310 2420  984 12210 178 428 102.5 14.3 3)
        (4 4370 2080  874 12650 183 404 110.8 10.0 3)

        (1 4700 1990  938  8830 148 349  77.4 50.0 4)
        (1 5930 2720 1211 11430 128 449  91.3 54.0 4)
        (1 4840 2360 1038 10370 132 406  91.3 89.5 4)
        (2 7230 3280 1233 10840 120 437  81.7 70.5 4)
        (2 7650 2670 1289 10800 124 455  89.2 37.5 4)
        (3 5760 1610 1137  9200 147 378  69.6 64.0 4)
        (3 7140 2240 1074  9300 255 420  69.0 16.0 4)
        (3 7950 2730 1200 10630 172 425  73.7 39.5 4)
        (4 5040 2270  869  9120 140 334  75.1 36.1 4)
        (4 3850 1880  823  8520 181 334  87.0 58.6 4))))

(dataset apple :identifiers NIL :variates apple-vars :name "Apple")
#|


(setq ss (scat-mat :data apple
                  :vars '("Ca" "Mg" "WGT" "BPI")

                  :size 1 :symbol :box :fill? t :color wb:*black-color*
                  :title "APPLE DATA"))
(defun log1 (x) (log (+ 1 x)))

(change-variable ss :from-var "BPI" 
                 :var '(log1 "BPI") )

(defun tb-sub (d)
  (loop with g = (make-list 16) with num
        for c in (cdr d) do
        (setq num (+ (* 4 (position (value-of c "Treatment" apple-vars) 
                                    (list 1 2 3 4)
                                    :test #'equal))
                     (- (value-of c "Block" apple-vars) 1)))
        (push c (elt g num))
        finally (return g)))
                    



(setq old-var-tr *variable-transforms*)
(setq *variable-transforms* '(mean median sd))




(setq c (window-of (car (viewports-of s3))))
;;------------------------------
(defun tf (c) 
  (position (value-of (car c) "Treatment"  :vars apple-vars) 
                 (list 1 2 3 4)
                 :test #'equal))

(defun bf (c) 
     (value-of (car c) "Block"  :vars apple-vars))




(defun tb-sub (d)
  (loop with g = (make-list 16) with num
        for c in (cdr d) do
        (setq num (+ (* 4 (position (value-of c "Treatment" :vars apple-vars) 
                                    (list 1 2 3 4)
                                    :test #'equal))
                     (- (value-of c "Block" :vars apple-vars) 1)))
        (push c (elt g num))
        finally (return g)))

(setq s3 (scatterplot :data apple
                      :subjects #'tb-sub
                      :x #'bf  
                      :y "WGT" :y-function #'vw::mean
                      ))
(link-views s3 ss)

                    
(setq s (scatterplot :data apple
                     :subjects #'tb-sub
                   :x #'tf :y #'bf
                   :bottom-label "Treatment"
                   :left-label "Block"
                  ;; :cloud-subviews-from (car (point-clouds-of ss))
                   ))

(setq h (histogram :data apple
                      :subjects #'tb-sub
                      :var 'tf :bottom-label "Treatment"
                      ))



;;(setq pc-apple (pc (cdr apple) :cols '( 2 3 4 5 6 7 8 9)))                                   
;;(setq apple-pc (array-to-lists (first pc-apple)))

(setq apple-pc (loop for a in (cdr apple)
                     for p in apple-pc collect
                     (push (car a) p)))
(setq spc (scatterplot :data apple-pc
                       :title "PC apple"
                      :x 0
                      :y 1
                      ))

(setq smat (scat-mat :data apple-pc
                       :title "PC apple"
                       :vars '(0 1 2 3)
                      ))

(unlink-views spc s)
(link-views (car (point-clouds-of smat)) s)


;;------------------------------
(setq apple-ds (loop for a in (cdr apple)
                     for p in apple-ds collect
                     (push (car a) p)))


(setq smat (scat-mat :data apple-ds
                       :title "Discrim apple"
                       :vars '(0 1 2 )
                      ))

(unlink-views smat s)
(unlink-views (car (point-clouds-of smat)) s)

|#