

(setq w (make-view-window))

(setq s (point-symbol :symbol :poly-star :fill? t :color wb:*yellow-color*
                      :size 50))


(setq cx 142 cy 142 r 110 )




(loop for angle from 0 below 360 by 30
      for angr = (* pi (/ angle 180))
      for x = (+ cx (truncate (* r (cos angr))))
      for y = (+ cy (truncate (* r (sin angr))))
      do
