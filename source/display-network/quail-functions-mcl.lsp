;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                             quail-functions-mcl.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     G. Desvignes 1988 - 1989
;;;     R.W. Oldford 1985 - 1992.
;;;
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(access-expression
          quail-print-help
          quail-prompt-read
          edit-text
          get-last-typed-command
          get-user-name)))

;;;
;;; functions definitions
;;;

(defun access-expression (variable)
  "Returns an expression which returns variable when it is evaluated."
   #|
   (when (listp variable)
         (setq variable (first variable)))
   (let ((name (pcl::get-name variable)))

     (or name
        (list 'ccl:%int-to-ptr 
              (%ptr-to-int variable))))
   |#
   )


;;; function access-object is useless for mac


;;; function key-for-access is useless for mac


(defun quail-print-help (message &key font-family font-size font-case)
  "Print a Help message in the help window using given font if any."
  (declare (special *quail-help-window*))
  (declare (ignore font-family font-size))
  (unless (and (boundp '*quail-help-window*)
               (wptr *quail-help-window* ))
    (setq *quail-help-window*
          (make-instance 'ccl:listener
                         :window-type :document
                         :window-title "Quail Help Window"
                         :window-size (make-point 400 200)
                         :window-position (make-point 0 280))))
  ;; only changement of font case is implemented
  
  (let ((previous-font (ccl:view-font *quail-help-window*)))
    (if (string= (symbol-name font-case) "BOLD")
      (ccl:set-window-font *quail-help-window*
                       (append '(:bold)
                               (ccl:view-font *quail-help-window*))))
    (pprint message *quail-help-window*)
    (ccl:set-view-font *quail-help-window* previous-font)
    nil))


(defun quail-prompt-read (message)
  "Ask the user for an input using a standard *quail-prompt-window*."
  (wb::prompt-user :prompt-string message)
  )


(defun edit-text (text &key (read-only nil))
  "If read-only is t we read the text, ~
   otherwise we return the string made by concatenating text with the new entry.  ~
   Default of read-only is T."
  
  (if read-only
    (quail-print-help text)
    (let (new-line)
      (quail-print text)
      (quail-print "")
      (princ "Add new note > ")
      (if (equal "" (setq new-line (read-line)))
        (concatenate 'string text "
" new-line)))))


(defun get-last-typed-command ()
  "Returns the last command typed in the listener."
  (declare (special CL:+))
  CL:+)

(defvar *user-name* NIL
  "The current user's name.")

(defun user-name (&key (prompt T))
  "Returns the current user name.  If prompt is  T (the default), and ~
   no name has been cached, then the user is prompted to supply one."
  (declare (special *user-name*))
  (if *user-name*
    *user-name*
    (if prompt
      (setf *user-name*
            (wb::prompt-user
             :read-type :string
             :prompt-string "Please enter your user name:"))
      "Anonymous")))


(defun set-window-icon (window icon)

;;; useless for Mac

 (declare (ignore window))
 icon)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; functions to make window shrink into an ICON
;;;

(defclass icon-window (canvas)
  ((icon :initarg :icon :accessor icon-of :initform NIL)
   (window-owner :initarg :window-owner :accessor window-owner-of :initform NIL))
  )


(defmethod window-zoom-event-handler ((self browser)  message)
  "When middle-button is pressed inside the zoom region, we should make an icon with ~
   window and hide window."
  (declare (ignore args))
  (if (ccl:option-key-p)
    (let* ((i-window (icon-w-of self))
           icon)
      (unless i-window
        (when (slot-exists-p (browser-of self)
                             'sub-view-icon)
          (setq icon (eval (slot-value (browser-of self)
                                       'sub-view-icon))) 
          (setq i-window (make-canvas
                          :canvas-class 'icon-window
                          :window-owner self
                          :icon icon
                          :window-type :single-edge-box
                          :window-show nil
                          :window-size (make-point (wb:bitmap-width icon)
                                                   (wb:bitmap-height icon))
                          :window-position (ccl:rref *current-event* :event.where)))
          (setf (icon-w-of self) i-window)))
      
      ;; when the icon window is built we can close the main window
      
      (when i-window
        (window-show i-window)
        (window-hide self)))
    (call-next-method)))



(defmethod view-draw-contents ((self icon-window))
  "Redraw the icon inside the window."
  (let ((my-icon (icon-of self))
        (title (icon-title 
                     (browser-of (window-owner-of self)))))
     (canvas-bitblt my-icon self
                    :canvas-left 0 :canvas-bottom (- (bitmap-height my-icon)))
     (set-view-font self '("helvetica" 10))
     (canvas-move-to (max 2 (ash (- (bitmap-width my-icon)
                                           (string-width title)) -1))
                     -9 self)
     ;;(#_TextMode :word (penmode-arg :patXor))
     (canvas-draw-string title self)))


(defmethod window-click-event-handler ((self icon-window) where)
  "If the button is double clicked, the icon is closed and the associated ~
   window is opened; a simple click is treated as a window-event."
  (declare (ignore where))
  (when (double-click-p)
    (window-hide self)
    (window-select (window-owner-of self))))


(defmethod window-event ((self icon-window))
  "In the classical event treatment, there are no methods to detect ~
   a mouse-down event.  ~
   We must therefore add the test in the main function and call drag-window if a ~
   mouse down event is detected."

  (if (eq (rref *current-event* :event.what) 1)
      (#_DragWindow :ptr (wptr self) 
                   :long (rref *current-event* :event.where) 
                   :ptr (make-record :rect :top 0 :left 0 :bottom 1000 :right 1000)))
  (call-next-method))
