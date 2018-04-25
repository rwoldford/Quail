;;;
;;; specific mac functions needed for Z
;;;

(in-package 'Z)                       ; create Z package
(use-package '(window-basics browser pcl specific))
(import 'pcl::object)
(shadow 'object)
(use-package 'ccl)

(in-package 'specific)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(access-expression
         set-window-icon
         Z-print-help
         Z-prompt-read
         edit-text
         get-last-typed-command
         get-user-name)))

;;;
;;; functions definitions
;;;

(defun access-expression (variable)

;;;
;;; returns an expression which returns variable when it is evaluated
;;;
   
   (when (listp variable)
         (setq variable (first variable)))
   (let ((name (pcl::get-name variable)))

     (or name
        (list 'ccl:%int-to-ptr 
              (%ptr-to-int variable)))))


;;; function access-object is useless for mac


;;; function key-for-access is useless for mac


(defun Z-print-help (message &key font-family font-size font-case)

;;;
;;; print a Help message in the help window using given font if any
;;;

  (declare (special *Z-help-window*))
  (declare (ignore font-family font-size))
  (declare (object-variable wptr))
  (unless (and (boundp '*Z-help-window*)
               (ask *Z-help-window* (boundp 'wptr)))
          (setq *Z-help-window* (oneof *listener*
                                           :window-type :document
                                           :window-title "Z HELP Window"
                                           :window-size (make-point 400 200)
                                           :window-position (make-point 0 280))))
  ;; only changement of font case is implemented

  (let ((previous-font (ask *Z-help-window* (window-font))))
     (if (string= (symbol-name font-case) "BOLD")
         (ask *Z-help-window* 
              (set-window-font (append '(:bold)
                                        (ask *Z-help-window* (window-font))))))
     (pprint message *Z-help-window*)
     (ask *Z-help-window* (set-window-font previous-font))
     nil))


(defun Z-prompt-read (message)

;;; ask the user for an input using a standard *Z-prompt-window*

  (let ((*Z-prompt-window*
                (oneof *listener* :window-type :document
                                  :window-title "Z Prompt window"
                                  :window-size (make-point 300 70)
                                  :window-position (make-point 15 50)
                                  ))
        result)
     (terpri *Z-prompt-window*)
     (princ message *Z-prompt-window*)
     (princ " > " *Z-prompt-window*)
     (setq result (read-line))
     (if (string= result "")
         (setq result nil)
         (setq result (read-from-string result)))
     (ask *Z-prompt-window* (window-close))
     result))


(defun edit-text (text &key (read-only nil))

;;; if read-only is t we read the text 
;;; otherwise, we return the string made by concatenating text with the new entry

(if read-only
    (Z-print-help text)
    (let (new-line)
       (quail-print text)
       (quail-print "")
       (princ "add new note > ")
       (if (equal "" (setq new-line (read-line)))
           (concatenate 'string text "
" new-line)))))


(defun get-last-typed-command ()
 
;;; returns the last command typed in the listener

  +)


(defun get-user-name ()

  'quail-user)


(defun set-window-icon (window icon)

;;; useless for Mac

 (declare (ignore window))
 icon)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; functions to make window shrink into an ICON
;;;

(defobject icon-window *WINDOW*)


(defobfun (window-zoom-event-handler graph-window) (&rest args)

;;; when button is pressed inside the zoom region, we should make an icon with
;;; window and hide window

  (declare (ignore args))
  (declare (object-variable icon-w))
  (let* ((window (self))
         (i-window (ask window icon-w))
         icon)
    (unless i-window
      (when (slot-exists-p (get-window-browser window)
                           'Z::sub-view-icon)
            (setq icon (eval (slot-value (get-window-browser window)
                                         'Z::sub-view-icon))) 
            (setq i-window (oneof icon-window
                            :window-type :single-edge-box
                            :window-show nil
                            :window-size (make-point (bitmap-width icon)
                                                     (bitmap-height icon))
                            :window-position (rref *current-event* event.where)))
            (ask i-window (have 'icon icon))
            (ask i-window (have 'window-owner window))

            (ask window (setq icon-w i-window))))
    
    ;; when the icon window is built we can close the main window

    (when i-window
          (ask i-window (window-show))
          (ask window (window-hide)))))



(defobfun (window-draw-contents icon-window) ()

;;; redraw the icon inside the window

  (declare (object-variable icon window-owner))
  (let ((my-icon (ask (self) icon))
        (title (Z::icon-title 
                     (get-window-browser (ask (self) window-owner)))))
     (canvas-bitblt my-icon (self)
                    :canvas-left 0 :canvas-bottom (- (bitmap-height my-icon)))
     (ask (self) (set-window-font '("helvetica" 10)))
     (canvas-move-to (max 2 (ash (- (bitmap-width my-icon)
                                           (string-width title)) -1))
                     -9 (self))
     (_TextMode :word (penmode-arg :patXor))
     (print-without-length-check title (self))))


(defobfun (window-click-event-handler icon-window) (where)

;;; if the button is double clicked, the icon is closed and the associated
;;; window is opened
;;; simple click is treated in window-event

      (declare (object-variable window-owner))
      (declare (ignore where))
      (when (double-click-p)
            (ask (self) (window-hide))
            (ask (ask (self) window-owner) (window-select))))


(defobfun (window-event icon-window) ()

;;; in the classical event treatment, there are no method to detect mouse down event
;;; we must therefore add the test in the main function and call drag-window if a
;;; mouse down event is detected

  (if (eq (rref *current-event* event.what) 1)
      (_DragWindow :ptr (ask (self) wptr) 
                   :long (rref *current-event* event.where) 
                   :ptr (make-rect 0 0 1000 1000)))
  (usual-window-event))
