;;
;; scroll-bars
;;

(in-package 'specific)


(proclaim '(object-variable wptr))


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
     (_PostEvent :errchk :a0 2 :d0 0 :d0)
     (_PostEvent :errchk :a0 1 :d0 0 :d0)))


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


(defun mac-redisplay-graph (window &aux (w-ptr (ask window wptr)))
   (_BeginUpdate :ptr w-ptr)
   (graph::redisplay-graph window (make-translation-vector window))
   (_EndUpdate :ptr w-ptr))

(defun redisplay-graph-in-rect (window rect)
   (with-port (ask window wptr)
       (_InvalRect :ptr rect))
   (mac-redisplay-graph window))
