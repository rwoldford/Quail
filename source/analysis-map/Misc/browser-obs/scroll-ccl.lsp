;;
;; scroll-bars
;;

(in-package 'specific)

  ;some constants for tracking the clicks in the scroll-bar

  (defconstant $InUpButton 20)
  (defconstant $InDownButton 21)
  (defconstant $InPageUp 22)
  (defconstant $InPageDown 23)
  (defconstant $InThumb 129)

(proclaim '(object-variable wptr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;*scroll-bar-dialog-item*
;;
;;this class inherits from *dialog-item*
;;

(defobject *scroll-bar-dialog-item* *dialog-item*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;exist
;;
;;init-list keywords:
;;   scroll-bar-length
;;   direction
;;
;;in addition, the standard dialog-item init-list keywords can be used
;;

(defobfun (exist *scroll-bar-dialog-item*) (init-list)
  (declare (object-variable min-setting max-setting direction scroll-bar-length))
  (have 'scroll-bar-length (getf init-list :scroll-bar-length))
  (have 'direction (getf init-list :direction))
  (usual-exist (init-list-default init-list
                                  :dialog-item-size
                                  (case direction
                                    (:vertical (make-point 16
                                                           scroll-bar-length))
                                    (:horizontal (make-point scroll-bar-length
                                                             16))
                                    (t (quail-error "illegal :direction ~a (must be :vertical or :horizontal)."
                                              direction))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;add-self-to-dialog
;;
;;  this is when we actually create the control (when the item
;;  is added to a window)

(defobfun (add-self-to-dialog *scroll-bar-dialog-item*) (dialog)
  (usual-add-self-to-dialog dialog)
  (let* ((my-size (dialog-item-size))
         (my-position (dialog-item-position)))
    (rlet ((scroll-rect :rect))
      (rset scroll-rect rect.topleft my-position)
      (rset scroll-rect rect.bottomright (add-points my-position my-size))
      (have 'dialog-item-handle
            (#_NewControl :ptr (ask dialog wptr)         ;window
                         :ptr scroll-rect               ;item rectangle
                         :ptr 0                         ;title
                         :word -1                       ;visible
                         :word 0                        ;initial value
                         :word 0                        ;min value
                         :word 0                        ;max value
                         :word 16                       ;type of control
                         :long 0                        ;refcon
                         :ptr)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;dialog-item-draw
;;
;;this function is called whenever the item needs to be drawn
;;
;;to draw the dialog-item, we just call _Draw1Control
;;

(defobfun (dialog-item-draw *scroll-bar-dialog-item*) ()
  (declare (object-variable dialog-item-handle))
  (#_Draw1Control :ptr dialog-item-handle))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;dialog-item-click-event-handler
;;
;;this is the function which is called when the user clicks in the scroll-bar
;;
;;It checks the scroll-bar part, and calls _TrackControl
;;  If appropriate, it passes a hook function to _TrackControl
;;
;;During tracking, dialog-item-action is repeatedly called.
;;

(defobfun (dialog-item-click-event-handler *scroll-bar-dialog-item*) (where)
  (declare (object-variable dialog-item-handle scroll-bar-proc))
  (declare (special page-size))
  (let* ((sb-handle dialog-item-handle)
         (part (#_TestControl :ptr sb-handle :long where :word)))
    (setq page-size (scroll-bar-length))
    (if (eq part #.$InThumb)
        (progn
          (#_TrackControl :ptr sb-handle
                         :long where
                         :ptr -1
                         :word)
          (dialog-item-action))
        (#_TrackControl :ptr sb-handle
                       :long where
                       :ptr scroll-bar-proc
                       :word))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;scroll-bar-proc
;;
;;this is the hook function which is passed to _TrackControl.  The toolbox
;;  will call this function periodically as the control is clicked.
;;

(defpascal scroll-bar-proc ((sb-handle :ptr) (part :word))
  (declare (special page-size))
  "This procedure adjusts the control value, and calls dialog-item-action."
  (#_SetCtlValue :ptr sb-handle :word
    (+ (#_GetCtlValue :ptr sb-handle :word)
       (case part
         (#.$InUpButton -5)
         (#.$InDownButton 5)
         (#.$InPageUp (- page-size))
         (#.$InPageDown page-size)
         (t 0))))
    (dialog-item-action))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;scroll-bar-value
;;
;;a nice safe Lisp-level function for getting the value of the scroll-bar
;;

(defobfun (scroll-bar-value *scroll-bar-dialog-item*) ()
  (declare (object-variable dialog-item-handle))
  (#_GetCtlValue :ptr dialog-item-handle :word))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;scroll-bar-length
;;
;;this is a variation of dialog-item-size
;;
;;It only used one dimension, since scroll-bars almost always have a width
;;  of 16 pixels.
;;

(defobfun (scroll-bar-length *scroll-bar-dialog-item*) ()
  (max (point-h (dialog-item-size)) 
       (point-v (dialog-item-size))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;set-scroll-bar-length
;;
;;sets the length of the scroll-bar
;;

(defobfun (set-scroll-bar-length *scroll-bar-dialog-item*) (new-length)

  (declare (object-variable direction dialog-item-handle))
  (set-dialog-item-size (if (eq direction :horizontal)
                            (make-point new-length 16)
                            (make-point 16 new-length)))
  (rlet ((rect :rect))
     (rset rect rect.topleft (dialog-item-position))
     (rset rect rect.bottomright (add-points (dialog-item-position)
                                             (dialog-item-size)))
     (rset dialog-item-handle control.rect rect)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; more scroll bar item functions

(defobfun (dialog-item-enable *scroll-bar-dialog-item*) ()
  (declare (object-variable dialog-item-handle))
  (#_ShowControl :ptr dialog-item-handle)
  (usual-dialog-item-enable))

(defobfun (dialog-item-disable *scroll-bar-dialog-item*) ()
  (declare (object-variable dialog-item-handle))
  (#_HideControl :ptr dialog-item-handle)
  (usual-dialog-item-disable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; window event functions

(defobfun (window-activate-event-handler graph-window) (&aux graph)

  (declare (object-variable browser))
  (dolist (control (dialog-items))
      (ask control (dialog-item-enable)))
  (when (setq graph (ask (self) browser))
        (install-browser-menubar graph))
  (usual-window-activate-event-handler)
  ;; 
  ;; after activating the window, we generate a mouse-up and down again
  ;; so that we can select a node in the window the same time the window
  ;; is activated 
  
  (unless (eq 0 (logand 512 (rref *current-event* event.modifiers)))
     (#_PostEvent :errchk :a0 2 :d0 0 :d0)
     (#_PostEvent :errchk :a0 1 :d0 0 :d0)))


(defobfun (window-deactivate-event-handler graph-window) ()

  (dolist (control (dialog-items))
      (ask control (dialog-item-disable)))
  (set-menubar *default-menubar*)
  (usual-window-deactivate-event-handler))
  
 
(defobfun (window-update-event-handler graph-window) ()

   (mac-redisplay-graph (self))
   (dolist (control (dialog-items))
       (ask control (dialog-item-draw)))
   (window-draw-grow-icon))

(defobfun (window-click-event-handler graph-window) (where &aux (window (self)))
 
   ;; ********* rwo (declare (special *last-mouse-state*))                                 
   (or (usual-window-click-event-handler where)
       (progn
         #|   removed so that mouse info is only thru wb package ********* rwo
         (or (boundp '*last-mouse-state*)
             (setq *last-mouse-state* (make-mouse-state)))
         (setf (mouse-state-position *last-mouse-state*)
               (make-position (point-h (rref *current-event* event.where))
                              (point-v (rref *current-event* event.where))))
         (setf (mouse-state-button *last-mouse-state*)
               (if (eq (rref *current-event* event.what) 1)    ; mouse button down
                   (if (logbitp 11 (rref *current-event* event.modifiers)) ; option key down
                       'left
                       'middle)
                   'up))
         |#
         (graph::apply-to-selected-node window))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; function to access data related to scroll
;;;

(defun get-window-x-offset (window)

  (declare (object-variable extent-region))
  (if (ask window (boundp 'extent-region))
      (first (ask window extent-region))
      0))


(defun get-window-y-offset (window)

  (declare (object-variable extent-region))
  (if (ask window (boundp 'extent-region))
      (second (ask window extent-region))
      0))

(defun set-extent-window-region (window extent)

  (declare (object-variable extent-region))
  (ask window (setq extent-region extent))
  (make-scroll-controls window))


(defun set-window-x-offset (window value)

  (declare (object-variable extent-region))
  (ask window (setf (first extent-region) value))
  value)


(defun set-window-y-offset (window value)

  (declare (object-variable extent-region))
  (ask window (setf (second extent-region) value))
  value)


(defun graph-rect (window &aux)

;;; returns the rectangle in which the graph is drawn inside the window in
;;; local coordinates

(when (ask window (boundp 'wptr))
  (let ((rgn (%get-ptr (rref (ask window wptr) window.contRgn))))
     (make-rect 0
                0
                (- (%get-signed-word rgn 6) (%get-signed-word rgn 2) 15)
                (- (%get-signed-word rgn 8) (%get-signed-word rgn 4) 15)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; other functions

(defun make-translation-vector (window)
 
   (make-position (- (get-window-x-offset window))
                  (- (+ (get-window-y-offset window)
                        (get-window-height window)))))

(defun mac-redisplay-graph (window &aux (w-ptr (ask window wptr)))
   (#_BeginUpdate :ptr w-ptr)
   (graph::redisplay-graph window (make-translation-vector window))
   (#_EndUpdate :ptr w-ptr))

(defun redisplay-graph-in-rect (window rect)
   (with-port (ask window wptr)
       (#_InvalRect :ptr rect))
   (mac-redisplay-graph window))

(defun redisplay-after-horizontal-scroll (&aux (window (front-window))
                                               move update-rect)
   (setq update-rect (graph-rect window))
   (unless (eq (setq move (- (get-window-x-offset window) (scroll-bar-value))) 0)
      (set-window-x-offset window (scroll-bar-value))
      (#_ScrollRect :ptr update-rect :word move :word 0 
                   :ptr (rref (ask window wptr) window.updateRgn))
      (if (< 0 move)
          (rset update-rect rect.right (+ move (rref update-rect rect.left)))
          (rset update-rect rect.left (+ move (rref update-rect rect.right))))
      (redisplay-graph-in-rect window update-rect)))

(defun redisplay-after-vertical-scroll (&aux (window (front-window))
                                             move update-rect)
   (setq update-rect (graph-rect window))
   (unless (eq 0 (setq move (+ (get-window-y-offset window) 
                               (scroll-bar-value))))
      (set-window-y-offset window (- (scroll-bar-value)))
      (#_ScrollRect :ptr update-rect :word 0 :word (- move) 
                   :ptr (rref (ask window wptr) window.updateRgn))
      (if (< 0 move)
          (rset update-rect rect.top (- (rref update-rect rect.bottom) move))
          (rset update-rect rect.bottom (- (rref update-rect rect.top) move)))
      (redisplay-graph-in-rect window update-rect)))
  

(defun compute-scroll-dimensions (window scroll-direction &aux length position)

    (if (eq scroll-direction :horizontal)
        (progn (setq position (make-point -1 (+ 1 (get-window-height window))))
               (setq length (+ 3 (get-window-width window))))
        (progn (setq position (make-point (+ 1 (get-window-width window)) -1))
               (setq length (+ 3 (get-window-height window)))))
    (values position length))


(defun set-horizontal-scroll-bounds (scroll window)
   (declare (object-variable extent-region dialog-item-handle))
   (let ((extent (ask window extent-region))
         maxi value)
      (setq maxi (max 0 (- (third extent)
                           (get-window-width window))))
      (if (> maxi 0) (setq maxi (+ 8 maxi)))
      (setq value (max 0 (min maxi (first extent))))
      (#_SetMaxCtl :ptr (ask scroll dialog-item-handle) :word maxi)
      (#_setCtlValue :ptr (ask scroll dialog-item-handle)
                    :word (set-window-x-offset window value))))

(defun set-vertical-scroll-bounds (scroll window)
   (declare (object-variable extent-region dialog-item-handle))
   (let ((extent (ask window extent-region))
         mini value)
      (setq mini (- (max 0 (- (fourth extent)
                              (get-window-height window)))))
      (setq value (max 0 (min (- mini) (second extent))))
      (#_SetMinCtl :ptr (ask scroll dialog-item-handle) :word mini)
      (#_setCtlValue :ptr (ask scroll dialog-item-handle)
                    :word (- (set-window-y-offset window value)))))


(defun resize-scroll-controls (window)

   (declare (object-variable direction))
   (dolist (control (ask window (dialog-items)))
       (multiple-value-bind (position length)
                            (compute-scroll-dimensions 
                                  window (ask control direction))
           (ask control (set-dialog-item-position position))
           (ask control (set-scroll-bar-length length))
           (if (eq (ask control direction) :horizontal)
               (set-horizontal-scroll-bounds control window)
               (set-vertical-scroll-bounds control window)))))
            

(defun make-horizontal-scroll (window &aux scroll)

;;; window must be opened in order for make-horizontal-scroll to work

  (multiple-value-bind (position length) 
                       (compute-scroll-dimensions window :horizontal)      
    (ask window (add-dialog-items
                  (setq scroll
                     (oneof *scroll-bar-dialog-item*
                            :dialog-item-position position
                            :direction :horizontal
                            :scroll-bar-length length
                            :dialog-item-action 
                              #'redisplay-after-horizontal-scroll)))))
  (set-horizontal-scroll-bounds scroll window))

(defun make-vertical-scroll (window &aux scroll)

;;; window must be opened in order for make-vertical-scroll to work

  (multiple-value-bind (position length) 
                       (compute-scroll-dimensions window :vertical)      
    (ask window (add-dialog-items
                  (setq scroll
                     (oneof *scroll-bar-dialog-item*
                            :dialog-item-position position
                            :direction :vertical
                            :scroll-bar-length length
                            :dialog-item-action 
                              #'redisplay-after-vertical-scroll)))))
  (set-vertical-scroll-bounds scroll window))

(defun make-scroll-controls (window)
    
   (unless (ask window (dialog-items))
           (make-horizontal-scroll window)
           (make-vertical-scroll window)))

