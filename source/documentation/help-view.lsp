;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                              help-view.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;
;;;  Authors:
;;;     C.B. Hurley 1992 George Washington University
;;;     R.W. Oldford 1992,1994.
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :quail)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(help-view)))

(defclass help-view (compound-view)
  ((help-type :initarg :help-type :initform NIL
              :documentation
              "The type of help that was requested in the construction ~
               of this help-view."))
  )

(defmethod construct-sub-views  ((self help-view) &rest args)
  (declare (ignore args))
  (setf (sub-views-of self)
        (help-sub-views (viewed-object-of self)
                        :type (slot-value self 'help-type))))

(defmethod compute-sub-viewports ((self help-view)
                                  &optional viewport junk)
  (declare (ignore junk)) 
  (loop
    for vp in (if viewport (list viewport) (viewports-of self))
    do
    (loop
      for sv in (sub-views-of self)
      for sv-vp = (or (vw::select-viewport sv vp) (make-viewport (window-of vp)))
      for new-sv-vp in
      (construct-viewports (sub-views-of self) vp )
      do
      (setf (bounds-of sv-vp) new-sv-vp)
      (add-viewport sv sv-vp vp))
    ))


(defmethod reshape-sub-viewports ((self help-view) viewport  
                                  &key new-location transform )
  (declare (ignore new-location transform))
  (map-subviews-to-viewport self viewport))

(defun construct-viewports (help-view-list viewport)
  "Constructs the necessary viewports for each view in ~
   help-view-list to be placed in viewport."
  (let ((header (first help-view-list))
        (paras (rest help-view-list))
        (w (window-of viewport))
        key-width body-width max-leading
        viewports current-y)
    (multiple-value-bind (l r b tp) (bounds-of viewport)
      (declare (ignore b))
      
      (setf max-leading
            (or 
             (loop for v in paras
                   when (has-draw-style-p v :font) ;;(typep v 'scrolling-display)
                   maximize
                   (wb:canvas-font-leading (draw-style v :font)))
             0))
      (setf current-y (- tp (help-height header) max-leading))
      (setf viewports
            (list (make-viewport w l r current-y tp)))
      
      (setf key-width (max-help-key-width help-view-list))
      (setf body-width (max 100 (max 0 (- r l key-width))))
      
      (append
       viewports
       (loop
         for key in paras by #'cddr
         for body in (cdr paras) by #'cddr
         append
         (list
          (make-viewport w l (+ l key-width)
                         (- current-y (help-height key))
                         current-y)
          (make-viewport w
                         (+ l key-width)
                         (+ l key-width body-width)
                         (- (decf current-y (help-height key))
                            (help-height body body-width))
                         current-y))
         do
         (decf current-y
               (+ (* 2 max-leading)
                  (help-height body body-width)))))
      )))







