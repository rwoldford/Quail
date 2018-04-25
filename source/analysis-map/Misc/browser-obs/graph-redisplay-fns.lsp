;;;
;;;  Graph redisplay functions
;;;

(defun make-translation-vector (window)
  (make-position (- (get-window-x-offset window))
                 (- (+ (get-window-y-offset window)
                       (get-window-height window)))))

(defun mac-redisplay-graph (window &aux (w-ptr (ask window wptr)))
   (_BeginUpdate :ptr w-ptr)
   (graph::redisplay-graph window (make-translation-vector window))
   (_EndUpdate :ptr w-ptr))

(defun redisplay-graph-in-rect (window rect)
   (with-port (ask window wptr)
       (_InvalRect :ptr rect))
   (mac-redisplay-graph window))