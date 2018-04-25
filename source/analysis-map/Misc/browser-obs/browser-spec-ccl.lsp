;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               browser-spec-mcl.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1988-1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     G. Desvignes 1988,1989
;;;
;;;
;;;----------------------------------------------------------------------------------
;;;
;;;
;;; functions needed by browser specific to Macintosh
;;;

;;;
;;; uses region-as-list (NO MORE!! ... rwo)
;;; defined in package graph but don't use package graph 
;;; because graph uses specific
;;;

;;;
;;; package definition
;;;

(in-package 'browser)  ; package browser must exist before compiling file

(in-package 'specific)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(browser-prompt-print
          browser-prompt-read
          get-active-region
          get-window-browser
          it
          *graph-format-choices*
          *horiz-lattice*
          *horiz-tree*
          *vertical-lattice*
          *vertical-tree*
          make-menu
          menu-p
          push-in-buffer
          scrolling-window
          select-in-menu
          set-window-browser
          the-shift-key-p)))

;;;
;;; variables definition
;;;

(defvar *horiz-lattice* '(graph:lattice))

(defvar *horiz-tree* '(graph:copies-only))

(defvar *vertical-lattice* '(graph:reverse graph:vertical graph:lattice))

(defvar *vertical-tree* '(graph:reverse graph:vertical graph:copies-only))

(defvar *graph-format-choices* '(("horizontal lattice" (graph:lattice))
                                 ("vertical lattice" (graph:reverse graph:vertical graph:lattice))
                                 ("horizontal tree" (graph:copies-only))
                                 ("vertical tree" (graph:reverse graph:vertical graph:copies-only)))) 

;;;
;;; function definitions
;;;

(defun browser-prompt-print (prompt)

   (quail-print prompt))


(defun browser-prompt-read (message)

  (let ((active-window (front-window))
        result)
     (princ message)
     (princ " > ")
     (setq result (read-line))
     (if (string= result "")
         (setq result nil)
         (setq result (read-from-string result)))
     (ask active-window (window-select))
     result))


(defun get-active-region (window)
  "Return the inside region of the window.  ~
   As the height and width of the clipping region are the inside dimensions ~
   of the window, we use it and the origin position of the window which is ~
   given by the outside region of the window to built the inside region."

       (let ((reg-1 (get-clipping-region window))
             (reg-2 (get-window-region window)))
            (setf (region-left reg-1)
                  (region-left reg-2))
            (setf (region-bottom reg-1)
                  (region-bottom reg-2))
            (setf (region-width reg-1)
                  (- (region-width reg-1) 24))
            (setf (region-height reg-1)
                  (- (region-height reg-1) 16))
            reg-1))


(defun get-window-browser (window)
  "Returns the browser represented in window."
  (declare (object-variable browser))
  (ask window browser))

(defun push-in-buffer (text)

  (ask (front-window) (set-window-layer 1))
  (or (stringp text)
      (setq text (format nil "~s" text)))
  (dotimes (k (length text))
     (_PostEvent :errchk :a0 3 :d0 (char text k) :d0)
     (_PostEvent :errchk :a0 4 :d0 (char text k) :d0)
     (event-dispatch))
  (_PostEvent :errchk :a0 3 :d0 #\NewLine :d0)
  (_PostEvent :errchk :a0 4 :d0 #\NewLine :d0))


(defun scrolling-window (window position)

;;; scrolls the window to position

  (set-window-x-offset window (position-x position))
  (set-window-y-offset window (position-y position))
  (redisplay-graph-in-rect window (graph-rect window)))


(defun set-window-browser (window value)
  "Associate a browser to the window."
  (declare (object-variable browser))
  (install-browser-menubar value)
  (ask window (setq browser value)))

(defun the-shift-key-p () (shift-key-p))


(in-package 'graph)

(defun apply-function-to-node (window node)

   (funcall (case (mouse-state)
              (:left (browser-left-fn-of window))
              (:middle (browser-middle-fn-of window))
              (:right (browser-right-fn-of window)))
            node window))
 
(defun track-node-in-window (window node-lst trans)

   ;; if cursor is in the content region then track node
    
   (declare (object-variable selected-node))
   (when node-lst
      (let ((rect (specific::graph-rect window))
            (now (ask window selected-node))
            cursor near)
         (loop
            (setq cursor (get-cursor-position window))
            (when (and (< (position-x cursor) (rref rect rect.right))
                       (< (position-y cursor) (rref rect rect.bottom)))
               (setf (position-x cursor) (- (position-x cursor)
                                            (position-x trans)))
               (setf (position-y cursor) (- (position-y cursor)
                                            (position-y trans)))
               (setq near (node-lst-as-menu node-lst cursor)))
            (unless (eq now near)
               (if now  (flip-node now window trans))
               (if near (flip-node near window trans))
               (setq now near))
            (when (eq 0 (_StillDown :word))
                    (return)))
        now)))

(defun apply-to-selected-node (window)

  (declare (object-variable selected-node))
  (let ((graph (graph-of window))
        (trans (specific::make-translation-vector window))
        node-lst node)
    
     (when (and graph 
                (setq node-lst (graph-node-lst graph)))
        (if (the-shift-key-p)
            (when (setq node (track-node-in-window window node-lst trans))
                  (apply-function-to-node window node)
                  (flip-node node window trans)
                  (if (setq node (ask window selected-node))
                      (flip-node node window trans)))
            (if (or (command-key-p)
                    (option-key-p))
                (when (setq node (ask window selected-node))
                      (apply-function-to-node window node))
                (ask window 
                     (setq selected-node 
                           (track-node-in-window window node-lst trans))))))))
