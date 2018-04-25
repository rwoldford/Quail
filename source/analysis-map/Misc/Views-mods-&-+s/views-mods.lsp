;;; replace draw axis methods in axis.lisp to accomodate changes in
;;; canvas-font-ascent, -descent, and -height.


(defmethod draw-axis ((self axis) (position (eql :top)) vp)
 (multiple-value-bind (xmin xmax ymin ymax) (bounds-of vp)
    
    (if (tics? self)
      (let* ((bw (basic-window-of vp))
             (font (draw-style self :font))
             (tic-list (tic-list-for-viewport self vp)) 
             (font-height (wb:canvas-font-height font))
             (font-ascent (wb:canvas-font-ascent font))
             (font-descent (wb:canvas-font-descent font))
             (tic-len (ceiling (* 0.7 font-ascent)))
             tic-end label-posn)
        (if (> (- ymax ymin) (+ tic-len font-height))
          (setf tic-end  (- ymax tic-len )
                label-posn (- tic-end font-height))
          (setf label-posn (+ ymin font-descent)
                tic-end (+ label-posn font-ascent )))
        (draw-horizontal-axis tic-list ymax tic-end label-posn font bw)))))
        
  
(defmethod draw-axis ((self axis) (position (eql :bottom)) vp)
 (multiple-value-bind (xmin xmax ymin ymax) (bounds-of vp)
    
    (if (tics? self)
      (let* ((bw (basic-window-of vp))
             (font (draw-style self :font))
             (tic-list (tic-list-for-viewport self vp)) 
             (font-height (wb:canvas-font-height font))
             (font-ascent (wb:canvas-font-ascent font))
             (font-descent (wb:canvas-font-descent font))
             (tic-len (ceiling (* 0.7 font-ascent)))
             tic-end label-posn)
        (if (> (- ymax ymin) (+ tic-len font-height))
          (setf tic-end  (+ ymin tic-len )
                label-posn (+ tic-end font-descent))
          (setf label-posn (- ymax font-ascent)
                tic-end (- label-posn font-descent )))
        (draw-horizontal-axis tic-list ymin tic-end label-posn font bw)))))


(defmethod draw-axis ((self axis) (position (eql :right)) vp)
 (multiple-value-bind (xmin xmax ymin ymax) (bounds-of vp)
    
    (if (tics? self)
      (let* ((bw (basic-window-of vp))
             (font (draw-style self :font))
             (tic-list (tic-list-for-viewport self vp)) 
             (font-ascent (wb:canvas-font-ascent font))
             (font-descent (wb:canvas-font-descent font))
             (font-height (wb:canvas-font-height font))
             (tic-len (ceiling (* 0.7 font-ascent)))
             tic-end label-posn tic-label-wid)
        (setq tic-label-wid 
              (loop for t-l in tic-list 
                    maximize 
                    (wb:canvas-string-width (cdr t-l) bw :font font)))
        (if (> (- xmax xmin) (+ tic-len font-descent tic-label-wid ))
          (setf tic-end  (- xmax tic-len )
                label-posn (- tic-end font-descent tic-label-wid))
          (setf label-posn xmin
                tic-end (+ label-posn tic-label-wid )))
        (draw-vertical-axis tic-list xmax tic-end label-posn font bw)))))


(defmethod draw-axis ((self axis) (position (eql :left)) vp)
 (multiple-value-bind (xmin xmax ymin ymax) (bounds-of vp)
    
    (if (tics? self)
      (let* ((bw (basic-window-of vp))
             (font (draw-style self :font))
             (tic-list (tic-list-for-viewport self vp)) 
             (font-ascent (wb:canvas-font-ascent font))
             (font-descent (wb:canvas-font-descent font))
             (font-height (wb:canvas-font-height font))
             (tic-len (ceiling (* 0.7 font-ascent)))
             tic-end label-posn tic-label-wid)
        (setq tic-label-wid 
              (loop for t-l in tic-list 
                    maximize 
                    (wb:canvas-string-width (cdr t-l) bw :font font)))
        (if (> (- xmax xmin) (+ tic-len font-descent tic-label-wid ))
          (setf tic-end  (+ xmin tic-len)
                label-posn (+ tic-end  font-descent ))
          (setf label-posn (- xmax tic-label-wid)
                tic-end (- label-posn font-descent)))
        (draw-vertical-axis tic-list xmin tic-end label-posn font bw)))))


(defun draw-horizontal-axis (tic-list tic-posn-y1 tic-posn-y2 label-posn font window)
  (wb:canvas-draw-line (caar tic-list) tic-posn-y1 
                        (caar (last tic-list)) tic-posn-y1 window)
  (loop  for t-l in tic-list
        for tic-posn-x = (car t-l)
        for tic-label = (cdr t-l) do
        (wb:canvas-draw-line tic-posn-x  tic-posn-y1 
                              tic-posn-x tic-posn-y2 window)
        (wb:canvas-move-to 
         (- tic-posn-x 
            (truncate (wb:canvas-string-width tic-label window :font font) 2)
            )
         label-posn window)
        (wb:canvas-draw-string tic-label window :font font)))

(defun draw-vertical-axis (tic-list tic-posn-x1 tic-posn-x2 label-posn font window)
  (wb:canvas-draw-line tic-posn-x1 (caar tic-list ) 
              tic-posn-x1 (caar (last tic-list))  window)
  (loop for t-l in tic-list
        for tic-posn-y = (car t-l)
        for tic-label = (cdr t-l) do
        (wb:canvas-draw-line tic-posn-x1  tic-posn-y 
                              tic-posn-x2 tic-posn-y window)
        (wb:canvas-move-to label-posn (- tic-posn-y 
                                          (truncate (wb:canvas-font-ascent font) 2))
                                          window)
        (wb:canvas-draw-string tic-label window :font font)))


;;; Just change fun-name at-top? to at-top-p in
;;;        macros.lisp 
;;;        region.lisp  and
;;;        view-window.lisp