 (defvar michelson-1879
  NIL
  "Determinations of the speed of light in air by A.A. Michelson in 1879. ~
   Variables are jittered speed (less 299800), measured speed less 299800 in km per sec,~
   the beat count between tuning forks, ~
   the temperature correction to tuning forks, the day where 1 = June 5 1879, ~
   difference between greatest and least values in cps for the 10 observations on which, ~
   each case is based, the distinctness of the division where 3 is good and 1 poor, ~
   displacement of the image in micrometer divisions, position of the reflected image, ~
   radius or distance from the rotating mirror to the slit source, ~
   number of revolutions per second of the rotating mirror as measured by the electric ~
   tuning fork, screw or millimetres per division of the micrometer, ~
   position of the slit source on the micrometer scale, ~
   time of day 0 is night, 1 is one hour before sunset, 2 is one hour after sunrise, ~
   and temperature in degrees Fahrenheit at the time of the observation.  ~
   The plane of rotation was slightly inclined by tilting the rotating mirror slightly.  ~
   The tangent of the angle of inclination is given as the last variable.  ~
   Note that the first measurement was taken at night using electric light, all others ~
   used a heliostat to focus sunlight.  ~
   The rotating mirror was inverted for measurements on June 30 and July 1.  ~
   It was inverted again back to the original orientation for the measurements on July 2.  ~
   Most measurements were taken by Michelson.  The exceptions are the sixth, seven and ~
   eighth measurements taken June 14 by Lieut. Nazro, and those taken the evening of ~
   June 17 by Mr. Clason. ~
   Michelson also notes that each measurement taken on June 7 and 8 had the frame inclined ~
   at various angles.  Beside the last two measurements on June 13 it was noted that ~
   Michelson `Set micrometer and counted oscillations.' and `Oscillations of ~
   revolving mirror.' respectively.  ~
   (:reference A.A. Michelson, 1880, Experimental Determination of the Velocity of Light, ~
   Astronomical Papers, 1, US Nautical Almanac, pp. 110-145.~
   R.W. Oldford 1994 The Speed of Light: A Case Study ~
   in Empirical Problem Solving U. of Waterloo Stats. and Act. Sci. Tech Report, ~
   )")

(<- michelson-1879
    (array
     '(  50 1.423 -0.132  1 0.17 3 114.55 114.85 28.672 257.36 0.99614 0.300 0 76 .02
         -60 1.533 -0.084  3 0.10 2 114.56 114.64 28.655 257.52 0.99614 0.074 1 72 .02
         100 1.533 -0.084  3 0.08 2 114.50 114.58 28.647 257.52 0.99614 0.074 1 72 .02
         270 1.533 -0.084  3 0.12 2  85.84  85.91 28.647 193.14 0.99598 0.074 1 72 .02
         130 1.533 -0.084  3 0.07 2  85.89  85.97 28.650 193.14 0.99598 0.074 1 72 .02
         
         50 1.533 -0.084  3 0.07 2 114.53 114.61 28.650 257.42 0.99614 0.074 1 72 .02
         150 1.533 -0.216  5 0.07 3 114.47 114.54 28.658 257.39 0.99614 0.074 1 83 .02
         180 1.533 -0.216  5 0.10 3 114.46 114.54 28.658 257.39 0.99614 0.074 1 83 .02
         180 1.533 -0.216  5 0.08 3 114.47 114.57 28.662 257.39 0.99614 0.074 1 83 .02
         80 1.533 -0.216  5 0.06 3 114.50 114.57 28.660 257.39 0.99614 0.074 1 83 .02
         
         200 1.533 -0.216  5 0.13 2 114.53 114.61 28.678 257.39 0.99614 0.074 1 83 .02
         180 1.517 -0.300  6 0.11 2 114.52 114.60 28.685 257.29 0.99614 0.074 1 90 .02
         130 1.517 -0.300  6 0.08 2 114.54 114.62 28.685 257.29 0.99614 0.074 1 90 .02
         -150 1.450 -0.072  8 0.09 2 114.74 114.81 28.690 257.45 0.99614 0.074 2 71 .02
        -40 1.450 -0.072  8 0.05 2 114.70 114.78 28.690 257.45 0.99614 0.074 2 71 .02
         
         10 1.450 -0.072  8 0.09 1 114.68 114.76 28.690 257.45 0.99614 0.074 2 71 .02
         200 1.500 -0.084  9 0.09 3 112.56 112.64 28.172 257.49 0.99614 0.074 2 72 .02
         200 1.500 -0.084  9 0.10 3 112.56 112.63 28.172 257.49 0.99614 0.074 2 72 .02
         160 1.500 -0.084  9 0.08 2 112.57 112.65 28.172 257.49 0.99614 0.074 2 72 .02
         160 1.517 -0.168  9 0.06 3 112.56 112.82 28.178 257.42 0.99614 0.260 1 79 .02
         
         160 1.517 -0.168  9 0.13 3 112.56 112.82 28.178 257.42 0.99614 0.260 1 79 .02
         140 1.517 -0.168  9 0.07 3 112.57 112.83 28.178 257.42 0.99614 0.260 1 79 .02
         160 1.517 -0.168  9 0.06 3 112.56 112.82 28.178 257.42 0.99614 0.260 1 79 .02
         140 1.517 -0.168  9 0.11 3 112.57 112.83 28.178 257.42 0.99614 0.260 1 79 .02
         80 1.517 -0.168  9 11   3 113.15 113.41 28.152 258.70 0.99614 0.260 1 79 .02
         
         0 1.517 -0.168  9 6    3 111.88 112.14 28.152 255.69 0.99614 0.260 1 79 .02
         50 1.500  0.012 10 0.12 1 112.57 112.83 28.152 257.58 0.99614 0.260 2 64 .02
         80 1.517  0.012 10 0.05 1 112.57 112.83 28.152 257.60 0.99614 0.260 2 64 .02
         100 1.517  0.000 10 0.11 1 112.55 112.81 28.152 257.59 0.99614 0.260 2 65 .02
         40 1.517 -0.012 10 0.09 1 112.57 112.83 28.152 257.57 0.99614 0.260 2 66 .02
         
         30 1.517 -0.024 10 0.12 1 112.57 112.83 28.152 257.56 0.99614 0.260 2 67 .02
         -10 1.517 -0.228 10 0.06 1 112.52 112.78 28.159 257.36 0.99614 0.260 1 84 .02
         10 1.500 -0.240 10 0.08 1 112.50 112.76 28.159 257.33 0.99614 0.260 1 85 .02
         80 1.483 -0.228 10 0.08 1 112.46 112.72 28.159 257.32 0.99614 0.260 1 84 .02
         80 1.483 -0.228 10 0.09 1 112.47 112.73 28.159 257.32 0.99614 0.260 1 84 .02
         
         30 1.483 -0.228 10 0.09 1 112.49 112.75 28.159 257.32 0.99614 0.260 1 84 .02
         0 1.517  0.036 13 0.09 2 112.59 112.85 28.149 257.62 0.99614 0.260 2 62 .02
         -10 1.500  0.024 13 0.06 2 112.58 112.84 28.149 257.59 0.99614 0.260 2 63 .02
         -40 1.500  0.012 13 0.07 1 112.59 112.85 28.149 257.58 0.99614 0.260 2 64 .02
         0 1.500 -0.144 13 0.07 3 112.54 112.80 28.157 257.43 0.99614 0.260 1 77 .02
         
         80 1.500 -0.144 13 0.08 3 112.51 112.77 28.157 257.43 0.99614 0.260 1 77 .02
         80 1.500 -0.144 13 0.11 3 112.51 112.77 28.157 257.43 0.99614 0.260 1 77 .02
         80 1.500 -0.144 13 0.09 3 112.51 112.77 28.157 257.43 0.99614 0.260 1 77 .02
         60 1.500 -0.144 13 0.08 3 112.52 112.78 28.157 257.43 0.99614 0.260 1 77 .02
         -80 1.500  0.084 14 0.07 1 112.64 112.90 28.150 257.65 0.99614 0.265 2 58 .02
        
         -80 1.500  0.084 14 0.10 1 112.64 112.90 28.150 257.65 0.99614 0.265 2 58 .02
         -180 1.483  0.072 14 0.07 1 112.66 112.92 28.150 257.62 0.99614 0.265 2 59 .02
         60 1.483 -0.120 14 0.09 2 112.52 112.79 28.158 257.43 0.99614 0.265 1 75 .02
         170 1.483 -0.120 14 0.10 2 112.48 112.75 28.158 257.43 0.99614 0.265 1 75 .02
         150 1.483 -0.120 14 0.08 2 112.49 112.76 28.158 257.43 0.99614 0.265 1 75 .02
         
         80 1.517  0.063 16 0.07 3 112.67 112.94 28.172 257.65 0.99614 0.265 2 60 .02
         110 1.517  0.048 16 0.09 3 112.65 112.92 28.172 257.63 0.99614 0.265 2 61 .02
         50 1.517  0.036 16 0.07 2 112.67 112.94 28.172 257.62 0.99614 0.265 2 62 .02
         70 1.517  0.024 16 0.03 2 112.66 112.93 28.172 257.61 0.99614 0.265 2 63 .02
         40 1.450 -0.156 16 0.13 2 133.21 133.48 33.345 257.36 0.99627 0.265 1 78 .02
         
         40 1.500 -0.168 16 0.09 2 133.23 133.49 33.345 257.40 0.99627 0.265 1 79 .02
         50 1.500 -0.180 16 0.07 2 133.22 133.49 33.345 257.39 0.99627 0.265 1 80 .02
         40 1.483 -0.168 16 0.13 2 133.24 133.50 33.345 257.39 0.99627 0.265 1 79 .02
         40 1.483 -0.168 16 0.06 2 133.22 133.49 33.345 257.38 0.99627 0.265 1 79 .02
         40 1.483 -0.168 16 0.10 2 133.22 133.49 33.345 257.38 0.99627 0.265 1 79 .02
         
         90 1.533  0.048 17 0.12 2 133.29 133.56 33.332 257.65 0.99627 0.265 2 61 .02
         10 1.533  0.036 17 0.08 2 133.31 133.58 33.332 257.64 0.99627 0.265 2 62 .02
         10 1.533  0.024 17 0.09 2 133.31 133.57 33.332 257.63 0.99627 0.265 2 63 .02
         20 1.533  0.012 17 0.11 2 133.30 133.57 33.332 257.61 0.99627 0.265 2 64 .02
         0 1.533  0.000 17 0.13 2 133.30 133.56 33.332 257.60 0.99627 0.265 2 65 .02
         
         -30 1.533 -0.180 17 0.06 3 133.21 133.48 33.330 257.42 0.99627 0.265 1 80 .02
         -40 1.500 -0.192 17 0.10 3 133.19 133.46 33.330 257.38 0.99627 0.265 1 81 .02
         -60 1.500 -0.204 17 0.05 3 133.20 133.46 33.330 257.37 0.99627 0.265 1 82 .02
         -50 1.517 -0.204 17 0.08 3 133.20 133.46 33.330 257.38 0.99627 0.265 1 82 .02
        -40 1.500 -0.192 17 0.08 3 133.19 133.46 33.330 257.38 0.99627 0.265 1 81 .02
         
         110 1.542 -0.288 19 0.08 3 133.16 133.43 33.345 257.32 0.99627 0.265 1 89 .02
         120 1.550 -0.288 19 0.06 3 133.15 133.42 33.345 257.33 0.99627 0.265 1 89 .02
         90 1.550 -0.300 19 0.09 3 133.17 133.43 33.345 257.32 0.99627 0.265 1 90 .02
         60 1.533 -0.300 19 0.07 3 133.16 133.43 33.345 257.30 0.99627 0.265 1 90 .02
         80 1.517 -0.300 19 0.07 3 133.16 133.42 33.345 257.29 0.99627 0.265 1 90 .02
         
         -80 1.517 -0.084 20 0.15 3 133.20 133.47 33.319 257.50 0.99627 0.265 2 72 .02
         40 1.517 -0.096 20 0.04 3 133.17 133.44 33.319 257.49 0.99627 0.265 2 73 .02
         50 1.517 -0.108 20 0.11 3 133.16 133.42 33.319 257.48 0.99627 0.265 2 74 .02
         50 1.517 -0.120 20 0.06 3 133.16 133.42 33.319 257.47 0.99627 0.265 2 75 .02
        -20 1.517 -0.132 20 0.10 3 133.18 133.44 33.319 257.45 0.99627 0.265 2 76 .02
         
         90 1.508 -0.252 22 0.05 2 133.15 133.42 33.339 257.33 0.99627 0.265 1 86 .02
         40 1.508 -0.252 22 0.08 2 133.17 133.44 33.339 257.33 0.99627 0.265 1 86 .02
         -20 1.483 -0.096 23 0.11 3 133.22 133.49 33.328 257.46 0.99627 0.265 2 73 .02
         10 1.483 -0.108 23 0.06 3 133.20 133.47 33.328 257.44 0.99627 0.265 2 74 .02
         -40 1.483 -0.120 23 0.09 3 133.21 133.47 33.328 257.43 0.99627 0.265 2 75 .02
         
         10 1.467 -0.120 23 0.09 3 133.19 133.45 33.328 257.42 0.99627 0.265 2 75 .02
         -10 1.483 -0.132 23 0.08 3 133.20 133.47 33.328 257.42 0.99627 0.265 2 76 .02
         10 1.483 -0.132 23 0.10 3 133.19 133.45 33.328 257.42 0.99627 0.265 2 76 .02
         10 1.500 -0.240 26 0.05 2  99.68  35.32 33.274 193.00 0.99645 135.000 1 85 .015
         50 1.508 -0.252 26 0.06 2  99.67  35.34 33.274 193.00 0.99645 135.000 1 86 .015
         
         70 1.508 -0.252 26 0.10 2  99.66  35.34 33.274 193.00 0.99645 135.000 1 86 .015
         70 1.517 -0.252 26 0.09 2  99.66  35.34 33.274 193.00 0.99645 135.000 1 86 .015
         10 1.500 -0.216 27 0.07 2 132.98   2.17 33.282 257.35 0.99627 135.145 1 83 .015
         -60 1.500 -0.228 27 0.09 2 133.00   2.15 33.282 257.34 0.99627 135.145 1 84 .015
         10 1.467 -0.252 27 0.06 2 133.01   2.14 33.311 257.28 0.99627 135.145 1 86 .015
         
         140 1.467 -0.252 27 0.08 2 133.00   2.14 33.311 257.28 0.99627 135.145 1 86 .015
         150 1.450 -0.252 28 0.05 3  99.45  99.85 33.205 192.95 0.99606 0.400 1 86 .015
         0 1.450 -0.252 28 0.03 3  66.34  66.74 33.205 128.63 0.99586 0.400 1 86 .015
         10 1.467 -0.252 28 0.07 3  47.96  50.16 33.205  96.48 0.99580 0.400 1 86 .015
         70 1.450 -0.240 28 0.06 3  33.17  33.57 33.205  64.32 0.99574 0.400 1 85 .015
         )
     :dimensions '(100 15)))

;;; Because so many of the values of the speed are the same and because this will
;;; cause overstriking in the plots, we will add a ``jittered'' version of the speed to
;;; the dataset.  Jittering has the effect of moving the identical points apart.

(<- michelson-1879 (cglue (+ (ref michelson-1879 T 0)
                             (random-uniform :n 100 :from -4 :to 4))
                          michelson-1879))

(<- michelson-vars
    '("jittered speed" "speed" "beat" "correction" 
      "day" "difference" "distinct" "division" "image"
      "radius" "revolutions" "screw" "slit" "am=2,pm=1,night=0"
      "temperature" "tan angle of inclination of plane of rotation"))

(<- michelson-ids
    '("June  5  1"
      
      "June  7  1" "June  7  2" "June  7  3" "June  7  4" "June  7  5"
      
      "June  9  1" "June  9  2" "June  9  3" "June  9  4" "June  9  5"
      
      "June 10  1" "June 10  2"
      
      "June 12  1" "June 12  2" "June 12  3"
      
      "June 13  1" "June 13  2" "June 13  3" "June 13  4" "June 13  5"
      "June 13  6" "June 13  7" "June 13  8" "June 13  9" "June 13 10"
      
      "June 14  1" "June 14  2" "June 14  3" "June 14  4" "June 14  5"
      "June 14  6" "June 14  7" "June 14  8" "June 14  9" "June 14 10"
      
      "June 17  1" "June 17  2" "June 17  3" "June 17  4" "June 17  5"
      "June 17  6" "June 17  7" "June 17  8"
      
      "June 18  1" "June 18  2" "June 18  3" "June 18  4" "June 18  5"
      "June 18  6"
      
      "June 20  1" "June 20  2" "June 20  3" "June 20  4" "June 20  5"
      "June 20  6" "June 20  7" "June 20  8" "June 20  9" "June 20 10"
      
      "June 21  1" "June 21  2" "June 21  3" "June 21  4" "June 21  5"
      "June 21  6" "June 21  7" "June 21  8" "June 21  9" "June 21 10"
      
      "June 23  1" "June 23  2" "June 23  3" "June 23  4" "June 23  5"
      
      "June 24  1" "June 24  2" "June 24  3" "June 24  4" "June 24  5"
      
      "June 26  1" "June 26  2" 
      
      "June 27  1" "June 27  2" "June 27  3" "June 27  4" "June 27  5"
      "June 27  6" 
      
      "June 30  1" "June 30  2" "June 30  3" "June 30  4" 
      
      "July  1  1" "July  1  2" "July  1  3" "July  1  4" 
      
      "July  2  1" "July  2  2" "July  2  3" "July  2  4" )
    )

(<- michelson-1879
    (dataset michelson-1879 
             :variates michelson-vars
             :identifiers michelson-ids
             :name "Michelson's 1879 determinations of the speed of light.")
    )

#| 
;;; Attempts at getting the speed from Michelson's data.
;;; Note that there is disagreement between our calculations and Michelson's
;;; (in some cases by as much as 300 km/s !).
;;;
;;; First the speed is calculated from the data in three different ways.
;;; The third is essentially the method used by Michelson (using log10 tables)
;;;

(defun speed  (mirror-freq disp screw-in-mm radius-in-ft tan-alpha)
  (flet
    ((feet-to-km (x)
       (/ (* x 1.609344) 5280)))
    (let*
      ((radius-in-km (feet-to-km radius-in-ft))
       (log-sec-alpha (- (log  (cos (atan tan-alpha)) 10)))
       (disp-in-km (expt
                    10
                    (+
                     ;; Disp in screw turns as measured by the micrometer
                     (log disp 10)
                     ;; use screw calibration to change units to millimetres
                     (log screw-in-mm 10)
                     ;; correction for inclination of plane
                     ;; of rotation. Rotating mirror was
                     ;; inclined slightly, alpha, where
                     ;; tan alpha = 0.02 in all but
                     ;; the last 12 observations where it is 0.015
                     ;; multiply by the secant of alpha
                     log-sec-alpha
                     -6
                     ))
                   )
       (phi (/ (atan disp-in-km radius-in-km) 2.0))
       (frac-of-rev (/ phi 2.0 pi))
       (time (/ frac-of-rev mirror-freq))
       (distance-in-km (* 2 (feet-to-km 1986.23)))
       )
      (/ distance-in-km time))))

(defun speed2 (mirror-freq disp screw-in-mm radius-in-ft tan-alpha)
  (flet
    ((feet-to-km (x)
       (/ (* x 1.609344) 5280))
     (log10 (x) (log x 10))
     )
    (let*
      ((log-sec-alpha (- (log10  (cos (atan tan-alpha)))))
       (log-disp-in-km 
        (+
         ;; Disp in screw turns as measured by the micrometer
         (log10 disp )
         ;; use screw calibration to change units to millimetres
         (log10 screw-in-mm )
         ;; correction for inclination of plane
         ;; of rotation. Rotating mirror was
         ;; inclined slightly, alpha, where
         ;; tan alpha = 0.02 in all but
         ;; the last 12 observations where it is 0.015
         ;; multiply by the secant of alpha
         log-sec-alpha
         -6
         )
        )
       (log-tan-phi (- log-disp-in-km
                       (+ (- (log10 radius-in-ft)
                             (log10 5280))
                          (log10 1.609344))
                       ))
       ;; phi in radians
       (phi (atan (expt 10.0 log-tan-phi)))
       (frac-of-rev (/ phi 2.0 pi))
       
       (log-distance-in-km (+ (log10 2.0) (log10 (feet-to-km 1986.23))))
       )
      (expt 10
            (- (+ (log10 2.0) log-distance-in-km (log10 mirror-freq))
               (log10 frac-of-rev))
            )
  )))

(defun speed3 (mirror-freq disp screw-in-mm radius-in-ft tan-alpha)
  (flet
    ((feet-to-km (x)
       (/ (* x 1.609344) 5280))
     (log10 (x) (log x 10))
     )
    (let*
      ((log-sec-alpha (- (log10  (cos (atan tan-alpha)))))
       (logT (log10 screw-in-mm))
       (logd (log10 disp))
       (logr (log10 radius-in-ft))
       (logn (log10 mirror-freq))
       (logc1 (+ -6 log-sec-alpha (- (log10 1.609344)) (log10 5280)))
       (logc (log10 (* 2 60 60 360 (feet-to-km 3972.46))))
       (logtanphi (- (+ logc1 logt logd) logr))
       (phi (/ (* 360 60 60 (atan (expt 10.0 logtanphi)))
               (* 2.0 pi)))
       (logphi (log10 phi))
       (logv (- (+ logc logn) logphi))
       )
      (expt 10.0 logv)))
  )



(<- michelson-speed (+ 299800
                       (ref michelson-1879 t 1)))

(<- calculated-speed (speed (ref michelson-1879 t  10)
                            (abs (- (ref michelson-1879 t 8)
                                    (ref michelson-1879 t 12)))
                            ;;(ref michelson-1879 t  7)
                            (ref michelson-1879 t 11)
                            (ref michelson-1879 t  9)
                            (ref michelson-1879 t 15)))



(<- calculated-speed2 (speed2 (ref michelson-1879 t  10)
                              (abs (- (ref michelson-1879 t 8)
                                      (ref michelson-1879 t 12)))
                              ;;(ref michelson-1879 t  7)
                              (ref michelson-1879 t 11)
                              (ref michelson-1879 t  9)
                              (ref michelson-1879 t 15)))



(<- calculated-speed3 (speed3 (ref michelson-1879 t  10)
                              (abs (- (ref michelson-1879 t 8)
                                      (ref michelson-1879 t 12)))
                              ;;(ref michelson-1879 t  7)
                              (ref michelson-1879 t 11)
                              (ref michelson-1879 t  9)
                              (ref michelson-1879 t 15)))


    
(<- calculated-speeds
    (dataset
     (cglue (ref michelson-1879 t 4)
            calculated-speed
            michelson-speed
            (- calculated-speed michelson-speed)
            (ref michelson-1879 t 14)
            (ref michelson-1879 t 13))
     :variates '("Day"
                 "Calculated speed"
                 "Michelson's speed"
                 "Residuals"
                 "Temperature"
                 "am=2,pm=1,night=0")
     :identifiers michelson-ids
     :name "Checking Michelson's calculations. 2")
    )


(scatterplot :data calculated-speeds 
             :x "Michelson's speed" :y "Calculated speed")




;;;
;;;  The following set of numbers are those whose calculation Michelson
;;;  gives in some detail in his paper. Again our numbers do not agree with his,
;;;  although these are a lot closer and the differences may be attributable to
;;;  rounding.  (Note that for observation index 43, he calculates the value to
;;;  be 299850 in the paper but records 299860 in his table!
;;;
;;;  Notation follows Michelson pp 133-134

(defun log10 (x) (log x 10))
(defun feet-to-km (x) (/ (* x 1.609344) 5280))

;;; get michelson's speeds (except for jittered ones)
(<- test (sel michelson-1879 (iseq 39 43)  '(:c 0)))
(setf (ref test t 0) (+ 299800 (ref test t 0)))

(<- b (ref test T 1))
(<- rev (ref test T 9))

(<- div (ref test T 6))
(<- image (ref test T 7))
(<- slit (ref test T 11))
(<- d (abs (- image slit)))
;; or (<- d div)

(<- n rev)
(<- r (ref test t 8))

(<- tan-alpha (ref test t 14))
(<- log-sec-alpha (- (log10 (cos (atan tan-alpha)))))

(<- screw (ref test t 10))
(<- logscrew (log10 screw))
(<- logscrew+1 (+ 1 logscrew))  ;; what Michelson records

(<- logT (log10 screw))
(<- logd (log10 d))
(<- logr (log10 r))
(<- logn (log10 n))

(<- logc1 (+ -6 log-sec-alpha (- (log10 1.609344)) (log10 5280)))
(<- logc1+3 (+ 3 logc1))  ;; what Michelson records

(<- logc (log10 (* 2 60 60 360 (feet-to-km 3972.46))))
(<- logc-6 (- logc 6))  ;; what Michelson records

(<- logtanphi (- (+ logc1 logt logd) logr))
(<- phi (/ (* 360 60 60 (atan (expt 10.0 logtanphi)))
           (* 2.0 pi)))
(<- logphi (log10 phi))

(<- logv (- (+ logc logn) logphi))

;;; Velocity is
(<- v (expt 10.0 logv))



;;;
;;;
;;; Now for Michelson's published versions
;;;

(<- mlog-sec-alpha 0.00008)

(<- mlogc1+3 0.51607)
(<- mlogc1 (- mlogc1+3 3.0))

(<- mlogc-6 0.49670)
(<- mlogc (+ 6.0 mlogc-6))

(<- mlogtanphi (- (array '(.11612 .11600  .11600 .11600 0.11604)) 2))
(<- mphi (/ (* 360 60 60 (atan (expt 10.0 mlogtanphi)))
           (* 2.0 pi)))
(<- mlogphi (log10 mphi))

(<- mlogv (- (+ mlogc logn) mlogphi))

;;; Velocity is
(<- mv (expt 10.0 mlogv))

;;;
;;; Even mv does not agree exactly with Michelson's numbers!
;;;


|#
