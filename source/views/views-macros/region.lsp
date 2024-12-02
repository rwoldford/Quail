;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               region.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is part of the Views system, a toolkit for interactive statistical graphics.
;;;  
;;;
;;;  Authors:
;;;     C.B. Hurley 1988-1991 George Washington University
;;;     R.W. Oldford 1988-1991
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute)
(export  '( make-interval interval-p min-of max-of length-of
            make-2d-position 2d-position-x
            ;;(setf 2d-position-x)
            2d-position-y
            ;;(setf 2d-position-y)
            2d-position-p
            left-of right-of bottom-of top-of 
            ;;( setf left-of) ( setf right-off) ( setf bottom-of) ( setf top-of) 
            height-of width-of  radius-of region-p make-region
            window-of ;;(setf window-of )
            viewport-p make-viewport window-of bounds-of
            ;;( setf bounds-of )
            x-extent-of y-extent-of
            maximize-regions
            bottom-left-of bottom-right-of top-left-of top-right-of centre-of contains-p
            coerce-bounds-to-integer
            distance-from intersects-p 
            make-transform-for-regions map-region
            scale-sides-by shift-by
            sub-region tile-region tile-region-list
            wb-region  wb-position view-region view-position set-viewport-width-height
            set-square-region-center copy-region set-square-viewport-size
            active-viewport-p activate-viewport deactivate-viewport
            valid-region-check)))

;;;----------------------------------------------------------------------------------
(defmacro make-interval (&optional (x 0) (y 0)) `(vector ,x ,y))
(defmacro interval-p (x) `(and (vectorp ,x) (= 2 (length ,x))))
(defmacro min-of (interval) `(svref ,interval 0))
(defmacro max-of (interval) `(svref ,interval 1))

(defsetf min-of (interval) (new-min)
  `(setf (svref ,interval 0) ,new-min))

(defsetf max-of (interval) (new-max)
  `(setf (svref ,interval 1) ,new-max))

(defmacro length-of (interval)
  `(- (max-of ,interval) (min-of ,interval)))
;;=================================================================    
(defmacro make-2d-position (&optional (x 0) (y 0)) `(vector ,x ,y))
(defmacro 2d-position-x (position) `(svref ,position 0))
(defmacro 2d-position-y (position) `(svref ,position 1))

(defsetf 2d-position-x (position) (new-x)
  `(setf (svref ,position 0) ,new-x))

(defsetf 2d-position-y (position) (new-y)
  `(setf (svref ,position 0) ,new-y))

(defmacro 2d-position-p (x) `(and (vectorp ,x) (= 2 (length ,x))))
;;================================================================= 
;;; Regions are stored as left right bottom top.


(defmacro left-of (region) `(svref ,region 1))
 
(defmacro right-of (region) `(svref ,region 2))
 
(defmacro bottom-of (region) `(svref ,region 3))

(defmacro top-of (region) `(svref ,region 4))

(defsetf left-of (region) (new)
  `(setf (svref ,region 1) ,new))
 
(defsetf right-of (region) (new)
  `(setf (svref ,region 2) ,new))

(defsetf bottom-of (region) (new)
  `(setf (svref ,region 3) ,new))

(defsetf top-of (region) (new)
  `(setf (svref ,region 4) ,new))
   

(defmacro height-of (region)
  `(- (top-of ,region) (bottom-of ,region)))

(defmacro width-of (region)
  `(- (right-of ,region) (left-of ,region)))

(defsetf width-of (region) (new-width)
 (let ((c (gensym)) (cx (gensym))   )
  `(let* ((,c (centre-of ,region))
         (,cx (2d-position-x ,c))
         )
    (setf (left-of ,region) (- ,cx (/ ,new-width 2)))
    (setf (right-of ,region) (+ ,cx (/ ,new-width 2))))))

(defsetf height-of (region) (new-height)
 (let ((c (gensym)) (cy (gensym)) )
  `(let* ((,c (centre-of ,region))
         (,cy (2d-position-y ,c))
         )
    (setf (bottom-of ,region) (- ,cy (/ ,new-height 2)))
    (setf (top-of ,region) (+ ,cy (/ ,new-height 2))))))

      
(defmacro region-p (r) 
  `(and (vectorp ,r) 
        (or (eql (svref ,r 0) 'region)
            (eql (svref ,r 0) 'viewport))))

#-:sbcl-linux(eval-when (:compile-toplevel :load-toplevel :execute) (proclaim '(inline make-region)))
#+:sbcl-linux(proclaim '(sb-ext:maybe-inline make-region)) ;25NOV2024
(defun make-region 
       (&optional (b1 0.0)  (b2 1.0)
                  (b3 0.0) (b4 1.0))
  
  (if (region-p b1) 
    (multiple-value-setq (b1 b2 b3 b4) (bounds-of b1)))
  (vector 'region b1 b2 b3 b4))


;;;=================================================================
;;; viewports are regions on windows

(defmacro window-of (viewport)
  `(svref ,viewport 5))

(defsetf window-of (viewport) (window)
  `(setf (svref ,viewport 5), window))

(defmacro active-viewport-p (viewport)
  `(and (wb::canvas-visible-p (svref ,viewport 5) )
        (svref ,viewport 6)))

(defmacro deactivate-viewport (viewport)
  `(setf (svref ,viewport 6) nil))

(defmacro activate-viewport (viewport)
  `(setf (svref ,viewport 6) t))



(defmacro viewport-p (r) `(and (vectorp ,r) (eql (svref ,r 0) 'viewport)))
;;=================================================================    

#-:sbcl-linux(eval-when (:compile-toplevel :load-toplevel :execute) (proclaim '(inline bounds-of)))
#+:sbcl-linux(proclaim '(sb-ext:maybe-inline bounds-of)) ;25NOV2024
(defun bounds-of (region)
  (values (left-of region) (right-of region) (bottom-of region)
          (top-of region)))

#-:sbcl-linux(eval-when (:compile-toplevel :load-toplevel :execute) (proclaim '(inline valid-region-check)))
#+:sbcl-linux(proclaim '(sb-ext:maybe-inline valid-region-check)) ;25NOV2024
(defun valid-region-check(region &optional (signal-error? t ))
       (multiple-value-bind (l r b tp) (bounds-of region)
         (if (or (>= l r) (>= b tp))
           (if signal-error? 
             (quail-error 
            "Region with bounds ~F,~F,~F,~F is invalid" l r b tp)
             nil)
           t)))


(defsetf bounds-of (region) (bounds)
  ;;; sets bounds of REGION to BOUNDS
  ;;; BOUNDS is a REGION or a list in order left, right bottom top
  (let ((l (gensym)) (r (gensym))
        (b (gensym)) (tp (gensym)))
    `(multiple-value-bind (,l ,r ,b ,tp) 
                          (if (region-p ,bounds) 
                            (bounds-of ,bounds)
                            (values-list ,bounds))
       (setf (left-of ,region) ,l)
       (setf (right-of ,region) ,r)
       (setf (top-of ,region) ,tp)
       (setf (bottom-of ,region) ,b)
       (if (viewport-p ,region) (coerce-bounds-to-integer ,region)))))

(defsetf x-extent-of (region) (bounds)
  ;;; sets x extent of REGION to BOUNDS
  ;;; BOUNDS is a REGION or a list in order left, right 
  (let ((l (gensym)) (r (gensym))
         )
    `(multiple-value-bind (,l ,r ) 
                          (if (region-p ,bounds) 
                            (bounds-of ,bounds)
                            (values-list ,bounds))
       (setf (left-of ,region) ,l)
       (setf (right-of ,region) ,r)
       (if (viewport-p ,region) (coerce-bounds-to-integer ,region)))))

(defsetf y-extent-of (region) (bounds)
  ;;; sets y extent of REGION to BOUNDS
  ;;; BOUNDS is a REGION or a list in order  bottom top
  (let ( (b (gensym)) (tp (gensym))
         (ignore-left (gensym))
         (ignore-right (gensym)))
    
    `(multiple-value-bind (,ignore-left ,ignore-right ,b ,tp) 
                          (if (region-p ,bounds) 
                            (bounds-of ,bounds)
                            (values-list (append '(nil nil) ,bounds)))
          (declare (ignore ,ignore-left ,ignore-right))
      
       (setf (top-of ,region) ,tp)
       (setf (bottom-of ,region) ,b)
       (if (viewport-p ,region) (coerce-bounds-to-integer ,region)))))

(defun maximize-regions (region &rest regions)
  ;; returns the smallest region enclosing region
  (loop for r in (push region regions)
          maximize (top-of r) into top
          minimize (bottom-of r) into bottom
          maximize (right-of r) into right
          minimize (left-of r) into left
          finally (return (make-region left right  bottom top) )))

#-:sbcl-linux(eval-when (:compile-toplevel :load-toplevel :execute) (proclaim '(inline bottom-left-of bottom-right-of top-left-of
            top-right-of  centre-of coerce-bounds-to-integer distance-from)))
#+:sbcl-linux(proclaim '(sb-ext:maybe-inline bottom-left-of)) ;25NOV2024
(defun bottom-left-of (region)
  ;;; return the POSITON coresponding to bottom-left of region
 (make-2d-position (left-of region) (bottom-of region)))

#+:sbcl-linux(proclaim '(sb-ext:maybe-inline bottom-right-of)) ;25NOV2024
(defun bottom-right-of (region)
  ;; return the POSITON coresponding to bottom-right of REGION
  (make-2d-position (right-of region) (bottom-of region)))

#+:sbcl-linux(proclaim '(sb-ext:maybe-inline top-left-of)) ;25NOV2024  
(defun top-left-of (region)
  ;; return the POSITON coresponding to top-left of REGION
  (make-2d-position (left-of region) (top-of region)))
  
  
#+:sbcl-linux(proclaim '(sb-ext:maybe-inline top-right-of)) ;25NOV2024  
(defun top-right-of (region)
  ;; return the POSITON coresponding to top-right of REGION
  (make-2d-position (right-of region) (top-of region)))
  
(defun centre-of ( region)
  ;; return the POSITON coresponding to center  of region
  (multiple-value-bind (l r b tp) (bounds-of region)
    (if (viewport-p region)
      (make-2d-position (truncate  (+ l r)  2) (truncate  (+ b tp) 2))
      (make-2d-position (/ (+ l r) 2) (/ (+ b tp) 2)))))


(defmethod contains-p ((region t) z)
  ;; returns REGION  when it contains Z (a 2d-position or a region) 
  (if (2d-position-p z)
    (multiple-value-bind (l r b tp) (bounds-of region)
      (when (and (>= (2d-position-x z) l)
                 (<= (2d-position-x z) r)
                 (>= (2d-position-y z) b)
                 (<= (2d-position-y z) tp))
        region))
    (if (region-p z)
      (multiple-value-bind (l r b tp) (bounds-of region)
        (multiple-value-bind (l1 r1 b1 tp1) (bounds-of z)
          (when (and (>= l1 l) (<= r1 r)
                     (>= b1 b) (<= tp1 tp))
            region))))))

(defun coerce-bounds-to-integer (region)
  (multiple-value-bind (l r b tp)  (bounds-of region)
    (setf (left-of region) (truncate l))
    (setf (right-of region) (truncate r))
    (setf (bottom-of region) (truncate b))
    (setf (top-of region) (truncate tp))))


(defun distance-from (region point)
   ;; returns distance-from regions centre to POINT
  (let ((c (centre-of region)))
    (max (abs (- (2d-position-x c) (2d-position-x point)))
         (abs (- (2d-position-y c) (2d-position-y point))))))

(defmethod intersects-p ((region1 t) (region2 t))
  ;;; returns non NIL IF REGION1 intersects REGION2
  (multiple-value-bind (la ra ba ta) (bounds-of region1)
    (multiple-value-bind (lb rb bb tb) (bounds-of region2)
  (and (<= (max la lb) (min ra rb))
                 (<= (max ba bb) (min ta tb))))))

#|
(defun make-transform-for-regions (from-region to-region)
  ;;; returns a 2D-AFFINE mapping FROM-REGION to TO-REGION
  
  (multiple-value-bind (l r b tp)  (bounds-of from-region)
    (multiple-value-bind (l1 r1 b1 tp1)  (bounds-of to-region)
      (let* ((tr (make-affine-transform 2))
             (w1 (- r l))
             (w2 (- r1 l1))
             (h1 (- tp b))
             (h2 (- tp1 b1))
             (x-slope (if (zerop w1) 1 (/ w2 w1)))
             (y-slope (if (zerop h1) 1 (/ h2 h1)))
             (x-intercept (if (zerop w1) (- (/ (+ l1 r1) 2) l)
                              (/ (- (* r l1) (* r1 l)) w1)))
             (y-intercept (if (zerop h1) (- (/ (+ b1 tp1) 2) b)
                              (/ (- (* tp b1) (* tp1 b)) h1))))
        (scale-transform! tr x-slope y-slope)
        (translate-transform! tr x-intercept y-intercept)
        tr))))
|#

(defun make-transform-for-regions (from-region to-region)
  ;;; returns a 2D-AFFINE mapping FROM-REGION to TO-REGION
  
  (multiple-value-bind (l r b tp)  (bounds-of from-region)
    (multiple-value-bind (l1 r1 b1 tp1)  (bounds-of to-region)
      (let* ((tr (make-affine-transform 2))
             (w1 (- r l))
             (w2 (- r1 l1))
             (h1 (- tp b))
             (h2 (- tp1 b1))
             (x-slope (if (or (zerop w1) (zerop w2)) 0 (/ w2 w1)))
             (y-slope (if (or (zerop h1) (zerop h2)) 0 (/ h2 h1)))
             (x-intercept (if (or (zerop w1) (zerop w2)) (+ l1 (/ w2 2))
                              (/ (- (* r l1) (* r1 l)) w1)))
             (y-intercept (if (or (zerop h1) (zerop h2)) (+ b1 (/ h2 2))
                              (/ (- (* tp b1) (* tp1 b)) h1))))
        (scale-transform! tr x-slope y-slope)
        (translate-transform! tr x-intercept y-intercept)
        tr))))


(defun map-region (self from-region to-region)
  ;; compute the affine transform mapping from-region into to-region
  ;; and apply it to self, returning the result
  (apply-transform (make-transform-for-regions from-region to-region)
                   self))

#-:sbcl-linux(eval-when (:compile-toplevel :load-toplevel :execute) (proclaim '(inline radius-of)))
#+:sbcl-linux(proclaim '(sb-ext:maybe-inline radius-of)) ;25NOV2024
(defun radius-of (region)
  ;;;  returns min of width and height times .5
  
  (/ (min (height-of region) (width-of region)) 2))

(defsetf radius-of (region) (new-radius)
 (let ((c (gensym)) (cx (gensym)) (cy (gensym)))
  `(let* ((,c (centre-of ,region))
         (,cx (2d-position-x ,c))
         (,cy (2d-position-y ,c)))
    (setf (bounds-of ,region)
          (list (- ,cx ,new-radius) (+ ,cx ,new-radius)
                (- ,cy ,new-radius) (+ ,cy ,new-radius)))
    ,new-radius)))

(defun scale-sides-by (region 
                           &optional (x-factor 1) (y-factor 1))
  ;;; multiplies width and height of REGION by X-FACTOR and Y-FACTOR 
  ;;; respectively.

  (let* ((w/2 (/ (* (width-of region)  x-factor) 2))
         (h/2 (/ (* (height-of region) y-factor) 2))
         (c (centre-of region))
         (cx (2d-position-x c))
         (cy (2d-position-y c)))
    (setf (bounds-of region)
          (list (- cx w/2) (+ cx w/2) (- cy h/2) (+ cy h/2)))
    region))

(defun shift-by (region
                     &optional (x-shift 0) (y-shift 0))
  ;;;  shifts REGION right by x and up by y
  (incf (left-of region) x-shift)
  (incf (right-of region) x-shift)
  (incf (bottom-of region) y-shift)
  (incf (top-of region) y-shift)
  region)



(defun sub-region (big-region position &key 
                        (width (width-of big-region)) 
                        (height (height-of big-region))
                        (mid-pt (if (or (eql position :n) (eql position :s)) 
                                  (2d-position-x (centre-of big-region))
                                  (2d-position-y (centre-of big-region))) )
                        remains)
  ;;; returns a REGION in BIG-REGION at position (:n :s :e :w),  
  ;; with dimenions WIDTH  * HEIGHT, centered at mid-pt

  (let (new-region (wid/2 (* 0.5 width)) (hgt/2 (* 0.5 height)))
    (multiple-value-bind (l r b tp) (bounds-of big-region)
      (if remains (setf (bounds-of remains) (list l r b tp)))
      (ecase position
             (:n 
              (setf new-region 
                    (make-region (- mid-pt wid/2) (+ mid-pt wid/2)  (- tp height) tp ))
              (if remains (setf (top-of remains) (bottom-of new-region))))
             (:s
              (setf new-region 
                    (make-region (- mid-pt wid/2) (+ mid-pt wid/2) b (+ b height)))
              (if remains (setf (bottom-of remains) (top-of new-region))))
             (:e
              (setf new-region 
                    (make-region (- r width) r (- mid-pt hgt/2) (+ mid-pt hgt/2)))
              (if remains (setf (right-of remains) (left-of new-region))))
             (:w
              (setf new-region
                    (make-region l (+ l  width)  (- mid-pt hgt/2) (+ mid-pt hgt/2)))
              (if remains (setf (left-of remains) (right-of new-region))))))
    new-region)
  )

(defun tile-region (region nrows ncols &optional square? (gap-x 0) (gap-y 0))
  "Returns a list where each list element is a list~
   of regions in a row beginning at the top-left,~
   separated in the horizontal direction by gap-x and by gap-y in~
   the vertical direction.~
   If square? is non-nil the regions created are square."
  
  
  (let* ((w (/ (width-of region) (+ (* (- ncols 1 ) gap-x) ncols)))
        (h (/ (height-of region) (+ (* (- nrows 1 ) gap-y) nrows))))
    (setq gap-x (* gap-x w))
    (setq gap-y (* gap-y h))
    
    (if square?
      (setq w (min w h) h w))
    (multiple-value-bind (l r b tp) (bounds-of region)
      (loop for i from 0 below nrows
            collect
            (loop for j from 0 below ncols collect
                  (make-region (+ l (* j w) (* j gap-x)) 
                               (min r (+ l (* (1+ j) w) (* j gap-x)))
                               (max b (- tp (* (1+ i) h) (* i gap-y)))
                               (- tp (* i h) (* i gap-y)))
                  )))))
    
(defun tile-region-list (region nrows ncols &optional square? (gap-x 0) (gap-y 0))
  "Returns a list of sub regions of region with nrows rows and ncols ~
   columns, separated in the horizontal direction by gap-x and by gap-y in~
   the vertical direction.~
  If square? is non-nil the regions created are square."
  (reduce #'append (tile-region region nrows ncols square? gap-x gap-y)))
   
;;;=================================================================

(defun make-viewport (&optional
                      (w (make-view-window))
                      (xmin 0.0) (xmax 1.0)
                      (ymin 0.0) (ymax 1.0))

  "Makes a VIEWPORT in the window w having bounds given by xmin xmax ymin ymax.  ~
   These may be in window coordinates or in 0-1 coords."
  (setq w (if (typep w 'view-window) 
            w
            (make-view-window :title w)))
  (multiple-value-setq
   (xmin xmax ymin ymax)
   (if (region-p xmin) 
     (bounds-of xmin)
     ;;else
     (case xmin
       (left (values 0.0 0.49 0.0 1.0))
       (right (values 0.51 1.0 0.0 1.0))
       (bottom (values 0.0 1.0 0.0 0.49))
       (top (values 0.0 1.0 0.51 1.0))
       (top-left (values 0.0 0.49 0.51 1.0))
       (top-right (values 0.51 1.0 0.51 1.0))
       (bottom-left (values 0.0 0.49 0.0 0.49))
       (bottom-right (values 0.51 1.0 0.0 0.49))
       (t (values xmin xmax ymin ymax)))))
       (let* ((vp (vector 'viewport nil nil nil nil nil t))
              (reg (region-of w))
             (w-lt (left-of reg))
             (w-bm (bottom-of reg))
             (w-ht  (height-of reg))
             (w-wid  (width-of reg)))
            (setf (window-of vp) w)
            (setf (bounds-of vp) (list xmin xmax ymin ymax))
            (when (contains-p (make-region)  vp)
                ;; transform bounds to window coords 
                (setf (bounds-of vp)
                      (list (+ w-lt (* xmin w-wid))
                            (+ w-lt (* xmax w-wid))
                            (+ w-bm (* ymin w-ht))
                            (+ w-bm (* ymax w-ht)))))
            vp))




;;;=================================================================
;;; conversion of types
;;;=================================================================


#-:sbcl-linux(eval-when (:compile-toplevel :load-toplevel :execute) 
  (proclaim '(inline wb-region wb-position view-region view-position)))

#+:sbcl-linux(proclaim '(sb-ext:maybe-inline wb-region)) ;25NOV2024
(defun wb-region ( region)
  ;; returns coords of region as a wb:region

  (wb::make-region (left-of region ) (bottom-of region ) 
                   (1+ (width-of region )) (1+  (height-of region ))))

#+:sbcl-linux(proclaim '(sb-ext:maybe-inline wb-position)) ;25NOV2024
(defun wb-position (p)
  ;; returns coords of p as a wb:position
  (wb::make-position 
   (2d-position-x p) 
   (2d-position-y p)))

#+:sbcl-linux(proclaim '(sb-ext:maybe-inline wb-region)) ;25NOV2024
(defun view-region (wb-region)
  (make-region (wb::region-left wb-region)
               (wb::region-right wb-region)
               (wb::region-bottom wb-region)
               (wb::region-top wb-region)))

#+:sbcl-linux(proclaim '(sb-ext:maybe-inline view-position)) ;25NOV2024
(defun view-position (wb-position)
  (make-2d-position (wb::position-x wb-position)
                 (wb::position-y wb-position)))

(defmacro set-viewport-size (viewport size)
  "Sets smaller of viewport width and height to size"
  (let ((c (gensym)) (cz (gensym))
        (s1 (gensym)) (s2 (gensym)))
    `(let* ((,c (centre-of ,viewport))
            (,s1 (truncate ,size 2))
            (,s2 (- ,size ,s1))
            ,cz)
       (cond ((< (width-of ,viewport) (height-of ,viewport) )
              (setq ,cz (2d-position-x ,c))
              (setf (left-of ,viewport) (- ,cz ,s1))
              (setf (right-of ,viewport) (+ ,cz ,s2)))
             (t (setq ,cz (2d-position-y ,c))
                (setf (bottom-of ,viewport) (- ,cz ,s1))
                (setf (top-of ,viewport) (+ ,cz ,s2)))))))
    
(defmacro set-square-viewport-size (viewport size)
 (let ((c (gensym)) (cx (gensym)) (cy (gensym))
       (s1 (gensym)) (s2 (gensym)))
  `(let* ((,c (centre-of ,viewport))
         (,cx (2d-position-x ,c))
         (,cy (2d-position-y ,c))
         (,s1 (truncate ,size 2))
         (,s2 (- ,size ,s1)))
    (setf (bounds-of ,viewport)
          (list (- ,cx ,s1) (+ ,cx ,s2)
                (- ,cy ,s1) (+ ,cy ,s2))))))

(defmacro set-square-viewport-center (viewport x y)
 (let ((size (gensym))
       (s1 (gensym)) (s2 (gensym)))
  `(let* ((,size (width-of ,viewport))
         (,s1 (truncate ,size 2))
         (,s2 (- ,size ,s1)))
    (setf (bounds-of ,viewport)
          (list (- ,x ,s1) (+ ,x ,s2)
                (- ,y ,s1) (+ ,y ,s2))))))



#|
(defmacro set-viewport-width-height (viewport w h)
 (let ((c (gensym)) (cx (gensym)) (cy (gensym))
       (w1 (gensym)) (w2 (gensym)) (h1 (gensym)) (h2 (gensym)))
  `(let* ((,c (centre-of ,viewport))
         (,cx (2d-position-x ,c))
         (,cy (2d-position-y ,c))
         (,w1 (truncate ,w 2))
         (,w2 (- ,w ,w1))
         (,h1 (truncate ,h 2))
         (,h2 (- ,h ,h1)))
    (setf (bounds-of ,viewport)
          (list (- ,cx ,w1) (+ ,cx ,w2)
                (- ,cy ,h1) (+ ,cy ,h2))))))
|#
           

(defmacro set-viewport-width-height (viewport w h)
  (let ((c (gensym)) (cx (gensym)) (cy (gensym))
        (w1 (gensym)) (w2 (gensym)) (h1 (gensym)) (h2 (gensym)))
    `(let* ((,c (centre-of ,viewport))
            ,cx ,cy ,w1  ,w2 ,h1 ,h2)
       (declare (ignorable ,c ,cx ,cy ,w1 ,w2 ,h1 ,h2))
       
       (when ,w 
         (setq ,cx (2d-position-x ,c))
         (setq ,w1 (truncate ,w 2))
         (setq ,w2 (- ,w ,w1))
         (setf (left-of ,viewport) (- ,cx ,w1))
         (setf (right-of ,viewport) (+ ,cx ,w2)))
       (when ,h
         (setq ,cy (2d-position-y ,c))
         (setq ,h1 (if ,h (truncate ,h 2)))
         (setq ,h2 (if ,h (- ,h ,h1)))
         (setf (bottom-of ,viewport)  (- ,cy ,h1))
         (setf (top-of ,viewport) (+ ,cy ,h2))))))

(defmacro set-square-region-center (region x y)
 (let ((r (gensym)) )
  `(let* ((,r (radius-of ,region)))
    (setf (bounds-of ,region)
          (list (- ,x ,r) (+ ,x ,r)
                (- ,y ,r) (+ ,y ,r))))))



(defun copy-region (r)
  (copy-seq r))
    

#-:sbcl-linux(eval-when (:compile-toplevel :load-toplevel :execute) 
  (proclaim '(inline line-segment-intersectp)))
#+:sbcl-linux(proclaim '(sb-ext:maybe-inline line-segment-intersectp)) ;25NOV2024
(defun line-segment-intersectp(x1 y1 x2 y2 u1  v1 u2 v2)
  (let* ((dx (- x2 x1))
        (dy (- y2 y1))
        (du (- u2 u1))
        (dv (- v2 v1))
        (xu (- x1 u1))
        (yv (- y1 v1))
        (d (- (* du dy) (* dx dv))))
    (and (not (zerop d))
         (<= 0 (/ (- (* dy xu) (* dx yv)) d) 1)
         (<= 0 (/ (- (* dv xu) (* du yv)) d) 1))))
