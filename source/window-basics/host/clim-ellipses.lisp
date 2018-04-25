;; This clim-ellipses.lisp
;; holding some test forms based on P34-35 and P53 of the clim-ug 2.2.2
;; started 03May2017
(draw-ellipse* *test-pane* 100 100  30 0 0 20 :start-angle 0)
;; There it is (green as it happens as a carry over from invert-rectangle)
(draw-ellipse* *test-pane* 200 200 30 0 0 50 :filled NIL)
;; another one .. so start-angle defaults to 0, it seems - that is reasonable
(draw-ellipse* *test-pane* 300 300 30 10 10 50)
;; Fails because we can only draw ellipses parallel to the standard coordinate axes
;; But there is the keyword argument transformation and P71 shows how to combine
;; transforms, including location and scale and rotation. Critically, there is
;; the identity: +identity-transformation+ so that simple location, scale, rotation
;; changes are composites with +i-t+ as described on P71 etc..
;;
;; Quail's draw-ellipse uses canvas left &optional top right bot suggesting
;; axes-aligned figures - check with rwo.
;;
;; arcs seem to come from :start-angle and :end-angle arguments.
(draw-ellipse* *test-pane* 300 300 30 0 0 50 :start-angle 1.0 :end-angle 2.0)
;; Yup
;; SInce the syntax is ltrb, try This:
;; A rectangle centred at 200 200, 200 wide 100 high:
(draw-rectangle* *test-pane* 100 150 300 250 :filled NIL)
(draw-ellipse* *test-pane* 200 200 100 0 0 50 :filled NIL)
;; the ellipse matches the box since we must draw parallel to the axes
;; at some point I must try the :transformation argument.
;;; On to polygons..
;;; the manual mentions a list of either points or x-y pairs .. is this a real list?
;; Try this
(draw-polygon* *test-pane* 100 100 180 190 250 330 100 100 :filled NIL)
;; Nope - incorrect argumen error.  Try this instead
(draw-polygon* *test-pane* (list 100 100 180 190 250 330 100 100) :filled NIL)
;; That's OK There is also d-p which takes a list of points as vertices.
(setf vertices (list (make-point 100 100) (make-point 180 190) (make-point 250 200) (make-point 100 100)))
(draw-polygon *test-pane* vertices :filled NIL) -> OK
;; This is the way the -pc function is called
;;
;; Now strings/text
(draw-text* *test-pane* "Some Text" 25 25)
;; Yipe!
Stream #<BASIC-FILE-BINARY-INPUT-STREAM ("/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"/6 ISO-8859-1) #x302003C2055D> is private to #<PROCESS test(13) [Semaphore timed wait] #x302003BCF8AD>
   [Condition of type SIMPLE-ERROR]
;; But
 (clim:with-text-style (*test-pane* (clim:make-text-style :fix :bold :large))
	     (write-string "Here is a text-style example." *test-pane*))
 ;; works
 ;; Try this
 (draw-text* *test-pane* "Some text" 25 25 :text-style (clim:make-text-style :fix :bold :large) :towards-x 25 :towards-y 100 :transform-glyphs T)
;; Which draws the text horizonatally 
(draw-text *test-pane* "Some more text" (make-point 25 25) :text-style (clim:make-text-style :fix :bold :large)  :toward-point (make-point 25 200))