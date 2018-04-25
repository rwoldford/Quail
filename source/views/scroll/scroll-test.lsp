



  
(in-package :Vw)             


(setq d (scrolling-display :draw? t
                      :display (case-display-list :data cigs)))

(setq r (right-scroller-of d))
(inspect r)
(set-drawing-style d :font (wb:
(reposition-view d)
(scroller-level-of d)
(setq s (first (ev d)))
(setq v (second (ev d)))
(max-display-start v :viewport (vp v))
(display-start-of (car (scroller-displays-of d)))
(slider-level-of (car (ev (scroller-of d))))

(draw-view d)
