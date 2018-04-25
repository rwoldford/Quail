 (format t "~% Values to be returned: ") ;06FEB2015
           (format t "~%left is ~d " (min (first anchor-point) (first original-mouse-position) (first final-mouse-position))) ;07FEB2015
           ;(format t "~%left is ~d " left) ;06FEB2015
           (format t "~%top is ~d " (min (second anchor-point) (second original-mouse-position) (second final-mouse-position))) ;07FEB2015
           ;(format t "~% (- (canvas-to-host-y canvas top) height) is ~d " (- (canvas-to-host-y canvas top) height)) ;06FEB2015
           (format t "~%width is ~d " width) ;06FEB2015
           (format t "~%height is ~d " height) ;06FEB2015
           (format t "~%host-mouse-position is now ~s " (host-mouse-position)) ;06FEB2015
           (values (min (first anchor-point) (first original-mouse-position) (first final-mouse-position)) ; 07FEB22015left
                   (cond ((and ( > (first final-mouse-position) (first original-mouse-position))
                               (> (second final-mouse-position) (second original-mouse-position))
                               )
                          (canvas-to-host-y canvas (second final-mouse-position)))
                         ((and (< (first final-mouse-position) (first original-mouse-position))
                               (> (second final-mouse-position) (second original-mouse-position))
                               )
                          (canvas-to-host-y canvas (second final-mouse-position)))
                         ((and (< (first final-mouse-position) (first original-mouse-position))
                               (< (second final-mouse-position) (second original-mouse-position))
                               )
                          (canvas-to-host-y canvas (second original-mouse-position)))
                         (T ;(and (> (first final-mouse-position) (first original-mouse-position))
                               ;(< (second final-mouse-position) (second original-mouse-position))
                               ;)
                          (canvas-to-host-y canvas (second original-mouse-position)))
                          )
                   ;(min (second anchor-point) (second original-mouse-position) (second final-mouse-position)) ;07FEB2015
              ; 07FEB2015(- (canvas-to-host-y canvas top) height)
                   ;;(+ top height)
                   (cond ((and ( > (first final-mouse-position) (first original-mouse-position))
                               (> (second final-mouse-position) (second original-mouse-position))
                               )
                          (- (first final-mouse-position) (min (first anchor-point) (first original-mouse-position)))
                          )
                         ((and (< (first final-mouse-position) (first original-mouse-position))
                               (> (second final-mouse-position) (second original-mouse-position))
                               )
                          (- (max (first anchor-point) (first original-mouse-position)) (first final-mouse-position)))
                         ((and (< (first final-mouse-position) (first original-mouse-position))
                               (< (second final-mouse-position) (second original-mouse-position))
                               )
                          (- (max (first anchor-point) (first original-mouse-position)) (first final-mouse-position)))
                         (T ;(and (> (first final-mouse-position) (first original-mouse-position))
                               ;(< (second final-mouse-position) (second original-mouse-position))
                               ;)
                          (- (first final-mouse-position) (min (first anchor-point) (first original-mouse-position)) )
                          )
                         );; end width
                   (cond ((and ( > (first final-mouse-position) (first original-mouse-position))
                               (> (second final-mouse-position) (second original-mouse-position))
                               )
                          (- (second final-mouse-position) (min (second anchor-point) (second original-mouse-position)))
                          )
                         ((and (< (first final-mouse-position) (first original-mouse-position))
                               (> (second final-mouse-position) (second original-mouse-position))
                               )
                          (- (second final-mouse-position) (min (second anchor-point) (second original-mouse-position)))
                          )
                         ((and (< (first final-mouse-position) (first original-mouse-position))
                               (< (second final-mouse-position) (second original-mouse-position))
                               )
                          (- (max (second anchor-point) (second original-mouse-position)) (second final-mouse-position)))
                         (T ;(and (> (first final-mouse-position) (first original-mouse-position))
                               ;(< (second final-mouse-position) (second original-mouse-position))
                               ;)
                          (- (max (second anchor-point) (second original-mouse-position)) (second final-mouse-position))
                          )
                         );; end height
                   ;width height ;07FEB2015