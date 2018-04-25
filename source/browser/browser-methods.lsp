;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               browser-methods.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1988-1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     G. Desvignes 1988,1989
;;;     R.W. Oldford 1988-1992
;;;
;;;
;;;----------------------------------------------------------------------------------
;;;

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(sub-items understands)))


(defmethod add-root ((self browser) &optional new-item)
  "Add a named item to the starting list of browser."
  
  (prog nil (or (objectp (setf new-item (new-item self new-item)))
                (return (prompt-print self "Nothing added to Browser.")))
        (when (has-object self new-item)
          (return new-item))
        (push new-item (starting-list-of self))
        (when (good-list-of self)
          (push new-item (good-list-of self)))
        (setf (bad-list-of self)
              (remove new-item (bad-list-of self)))
        (recompute self)
        (return new-item)))


(defmethod browse ((self browser)
                   &optional browse-list window-or-title good-list position)
  "Call show-browser and then Shape to Hold and move for first time."
  
  (show-browser self browse-list window-or-title good-list)
  (if position (wb::move-canvas self position)))


(defmethod browser-objects ((self browser))
  "Return a list of all the objects shown in the Browser."
  
  (let (result node-id)
    (dolist (node (graph-node-lst (wb::display-of self)))
      (when (not (listp (setf node-id (graph-node-id node))))
        (push node-id result)))
    result))


(defmethod change-format ((self browser) &optional format)
  "Change format between Lattice and Tree."
  (cond ((and format (listp format))
         (setf (graph-format-of self) format))
        (T
         (setf format
               (wb::pick-one (graph-format-choices-of self)
                             :item-print-function
                             #'(lambda (i stream)
                                 (format stream (first i)))))
               
         (setf (graph-format-of self) format)))
  (recompute self))


(defmethod choice-menu ((self browser) item-cv)
  "Create a menu and cache it in slot menu-cache."
  
  (prog ((menu (getf (menu-cache-of self) item-cv))
         items)
    (cond ((wb::menu-p menu) (return menu))
          ((not (listp (setf items (slot-value self item-cv))))
           (return nil)))
    (setf menu (wb::make-menu :items items
                          :when-held-fn  #'browser-when-held-fn
                          :when-selected-fn #'browser-body-when-selected-fn
                          :change-offset-fn t))
    (when (cache-menu-p self)
      (setf (getf (menu-cache-of self) item-cv)
            menu))
    (return menu)))


(defmethod clear-label-cache ((self browser) objects)
  "Delete the Cached label for the Objects."
  
  (if (listp objects)
    (dolist (object objects)
      (clear-label-cache self object))
    (if (eq objects t)
      ;;  Delete the whole cache
      (setf (label-cache-of self) nil)
      (let ((cached-label (assoc objects (label-cache-of self))))
        (when cached-label
          (setf (label-cache-of self)
                (remove cached-label
                        (label-cache-of self))))))))


(defmethod do-selected-command ((self browser) command obj)
  "Does the selected command or forwards it to the object."
  
  (prog (args)
    (if (null command)
      (return nil))
    (if (listp command)
      (progn (setf args (cdr command))
             (setf command (car command))))
    (if (listp obj)
      
      ;; Take care of being passed in a dummy node from browser in
      ;; Lattice mode Dummy nodes are indicated by having the
      ;; object in a list
      (setf obj (car obj)))
    (return (if (and (not (member command (local-commands-of self)))
                     (understands obj command))
              (browser-funcall command obj args)
              (if obj
                (browser-funcall command self obj args)
                (browser-funcall command self args))))))


(defmethod flash-node ((self browser)
                       node &optional n flash-time leave-flipped?)
  "Flashes the selected  node."
  
  (let* ((node (get-node-from-id
                node
                (graph-node-lst (wb::display-of self)))))
    (when node
      (dotimes (i (or n 3))
        (flip-node node self)
        (sleep (or flash-time 0.3))
        (flip-node node self)
        (sleep (or flash-time 0.3))))
    (when leave-flipped? 
      (flip-node node self))))


(defmethod get-display-label ((self browser) object)
  "Get the display label. Use the Cache if it provides the answer; If not, ~
   and Max-label-width is set, use it to compute the appropriate Bitmap and ~
   then cache the result."
  
  (let ((cached-label (assoc object (label-cache-of self))))
    (if cached-label
      (cdr cached-label)
      (let ((new-label (get-label self object)))
        (if (listp new-label)
          (setf new-label (format nil "~S" new-label)))
        (push (cons object new-label)
              (label-cache-of self))
        new-label))))


(defmethod get-label ((self browser) object)
  "Get a label for an object to be displayed in the browser."
  (if (quail::class-p object)
    (class-name object)
    (class-name (class-of object))))


(defmethod get-node-list ((self browser) browse-list)
  "Compute the node data structures of the tree starting at BROWSE-LIST. If ~
   GOOD-LIST is given, only includes  elements of it."
  
  (prog* ((graph (wb::display-of self))
          (old-nodes (if graph (graph-node-lst graph)))
          obj-list node obj)
    
    
    ;;; 
    ;;; First make Obj-list which is a list of pairs (Object . Obj-Name). Obj-Name
    ;;; will be used as a title for a node in the browser. This structure will be
    ;;; replaced by a Graph-Node when it is processed. The node ID of the
    ;;; Graph-node will be the Object and the label will be the name
    ;;; 
    
    (declare (special obj-list))
    (dolist (obj-or-name browse-list)
      (add-in-obj-list self obj-or-name))
    (do ((pair obj-list (cdr pair)))
        ((null pair))
      (setf node (or (get-node-from-id 
                      (setf obj (caar pair))
                      old-nodes t)
                     (make-graph-node :id obj)))
      (setf (graph-node-to-nodes node)
            (cddar pair))
      (setf (graph-node-label-symbol node)
            (cadar pair))
      (setf (graph-node-font node)
            *graph-default-node-font*)
      (setf (graph-node-height node)
            nil)
      (setf (graph-node-width node)
            nil)
      (setf (first pair)
            node))
    (return obj-list)))


(defmethod get-links ((self browser) object &key (reverse? NIL))
  (declare (ignore reverse?))
  "Gets a set of subs from an object for browsing."
  (class-direct-subclasses object))


(defmethod has-object ((self browser) object)
  "Check Object in Graph-Nodes and return if it is one of them."
  (if (wb::display-of self)
    (member object (browser-objects self))
    NIL))


(defmethod left-choice ((self browser))
  "Make a selection from the menu build using Left-Button-Items or ~
   Shift-left-Button-Items."
  
  (prog (menu)
    (setf menu (if (wb::shift-key-p)
                 (if (shift-left-button-items-of self)
                   (choice-menu self 'shift-left-button-items)
                   (if (understands self 'left-shift-select)
                     (prog nil (left-shift-select self)
                           (return nil))
                     (choice-menu self 'left-button-items)))
                 (choice-menu self 'left-button-items)))
    (return (and menu (wb::select-in-menu menu)))))


(defmethod left-selection ((self browser)
                           obj)
  "choose an item from the Left button items and apply it."
  
  (declare (special object))
  (setf object obj)
  (prog (selector)
    (setf selector (or (left-choice self)
                       (return nil)))
    (do-selected-command self selector object)))


(defmethod left-shift-select ((self browser))
  
  (declare (special object))
  (push-in-buffer object))


(defmethod middle-choice ((self browser))
  "Make a selection from the menu build using Middle-Button-Items or ~
   Shift-middle-Button-Items."
  
  (prog (menu)
    (setf menu (if (wb::shift-key-p)
                 (if (shift-middle-button-items-of self)
                   (choice-menu self 'shift-middle-button-items)
                   (if (understands self 'middle-shift-select)
                     (prog nil (middle-shift-select self)
                           (return nil))
                     (choice-menu self 'middle-button-items)))
                 (choice-menu self 'middle-button-items)))
    (return (and menu (wb::select-in-menu menu)))))


(defmethod middle-selection ((self browser)
                             obj)
  "choose an item from the Middle button items and apply it."
  
  (declare (special object))
  (setf object obj)
  (prog (selector)
    (setf selector (or (middle-choice self)
                       (return nil)))
    (do-selected-command self selector object)))


(defmethod middle-shift-select ((self browser))
  (declare (special object))
  (push-in-buffer object))


(defmethod new-item ((self browser)
                     &optional new-item)
  "Return Object, Prompt for it if needed."
  
  (find-class
   (or new-item
       (wb::prompt-user :prompt-string "Give name of item to be added : "
                        :read-type :read
                        :type 'symbol))))


(defmethod obj-name-pair ((self browser)
                          obj-or-name)
  "make a pair (Object . (Obj-Name . NIL)) where Obj-Name is label to be ~
   used in browser."
  
  (prog (obj name)
    (unless obj-or-name (return nil))
    (setf obj obj-or-name)
    (setf name (get-display-label self obj-or-name))
    (return (if (and (good-list-of self)
                     (not (member obj (good-list-of self))))
              nil
              (if (member obj (bad-list-of self))
                nil
                (cons obj (cons name nil)))))))


(defmethod prompt-print ((self browser) prompt)
  "Prints out the prompt in the prompt-window."
  (vw::inform-user prompt))


(defmethod recompute ((self browser) &optional dont-reshape-flg)
  "Recomputes the graph in the same window."
  (show-browser self (starting-list-of self) self)
  (unless dont-reshape-flg (shape-to-hold self))
  )


(defmethod recompute-in-place ((self browser))
  "Recompute the graph maintaining the current position."
  (recompute self t)
  )


(defmethod recompute-labels ((self browser))
  "Recompute the graph including the labels."
  (clear-label-cache self t)
  (recompute self t))


(defmethod shape-to-hold ((self browser))
  "Reshape the window (if necessary) to just hold the nodes of the browser."
  
  (declare (special *min-width-graph*  *min-height-graph*
                    ))
  (prog* ((old-reg (wb::canvas-screen-region self))
          ;;  Old-Reg is the previous size of the window 
          (new-reg (graph-region (wb::display-of self)))
          ;;  New-Reg is a list representing the region used by
          ;; the graph 
          )
    
    ;;  we update the new height and width of the window 
    (setf (wb::region-width old-reg)
          (min (max (wb::region-width new-reg)
                    *min-width-graph*)
               (max-width-graph)))
    (setf (wb::region-height old-reg)
          (min (max (wb::region-height new-reg)
                    *min-height-graph*)
               (max-height-graph)))
    ;; and then reshape it
    (wb::shape-canvas self old-reg)
    ))


(defmethod show-browser ((self browser)
                 &optional browse-list window-or-title good-list)
  "Show the items and their subs on a browse window  ~
    If WINDOW-OR-TITLE is not a window it will be used as a title for the ~
   window which will be created."
  
  (prog (node-lst)
    (unless window-or-title
      (setf window-or-title (wb::canvas-title self)))
    (if (not (wb::canvas-p window-or-title))
      (setf (wb::canvas-title self) window-or-title))
    (when (and browse-list (not (listp browse-list)))
      (setf browse-list (list browse-list)))
    (setf (starting-list-of self) nil)
    (setf (good-list-of self) nil)
    (dolist (c browse-list)
      (push c (starting-list-of self)))
    (setf (starting-list-of self)
          (nreverse (starting-list-of self)))
    (and good-list (dolist (c good-list t)
                     (push c (good-list-of self)))
         (setf (good-list-of self)
               (nreverse (good-list-of self))))
    ;;(setf (slot-value self 'window)
    (show-graph (when (and (starting-list-of self)
                           (setf node-lst
                                 (get-node-list
                                  self
                                  (starting-list-of self))))
                  (layout-graph node-lst (tree-roots node-lst)
                                (graph-format-of self)
                                (wb::canvas-font self)))
                self
                (top-align-p self))
          ;;)
    
    (return self)))


(defmethod title-left-choice ((self browser))
  "Make a selection from the menu build using Left-Title-Items."
  
  (prog (menu)
    (setf menu (if (wb::shift-key-p)
                 (if (understands self 'title-left-shift-select)
                   (prog nil (title-left-shift-select self)
                         (return nil))
                   (choice-menu self 'left-title-items))
                 (choice-menu self 'left-title-items)))
    (return (and menu (wb::select-in-menu menu)))))


(defmethod title-left-selection ((self browser))
  "choose an item from the Left title items and apply it."
  
  (prog (selector)
    (setf selector (or (title-left-choice self)
                       (return nil)))
    ;; added NIL ... do-selected.. needs 3 args.
    (do-selected-command self selector NIL)))


(defmethod title-left-shift-select ((self browser))
  
  (push-in-buffer self))


(defmethod title-middle-choice ((self browser))
  "Make a selection from the menu build using Middle-Title-Items."
  
  (prog (menu)
    (setf menu (if (wb::shift-key-p)
                 (if (understands self 'title-middle-shift-select)
                   (prog nil (title-middle-shift-select self)
                         (return nil))
                   (choice-menu self 'middle-title-items))
                 (choice-menu self 'middle-title-items)))
    (return (and menu (wb::select-in-menu menu)))))


(defmethod title-middle-selection ((self browser))
  "choose an item from the Middle button items and apply it."
  
  (prog (selector)
    (setf selector (or (title-middle-choice self)
                       (return nil)))
    ;; added NIL ... do-selected.. needs 3 args.
    (do-selected-command self selector NIL)))


(defmethod unread ((self browser)
                   &optional object)
  "Unread Object into system buffer."
  
  (push-in-buffer object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; functions definitions
;;;


(defun add-in-obj-list (self new-element)
  (declare (special obj-list))
  (let (pair)
    (and (setf pair (obj-name-pair self new-element))
         (not (assoc (first pair)
                     obj-list))
         (push pair obj-list)
         (dolist (sub (get-links self (first pair)))
           (add-in-obj-list self sub)
           (push sub (cddr pair))))))


(defun browser-funcall (command obj &optional (args nil) (more-args nil))
  (if (null more-args)
    (if (null args)
      (funcall command obj)
      (funcall command obj args))
    (funcall command obj args more-args)))


(defun node-object (node)
  
   "Determines the object represented by a node on the graph ~
   the problem is that the node-id of node can be a list if the object is  ~
   represented more than once on the graph."
  
  (let ((node-id (graph-node-id node)))
    (loop
      (if (listp node-id)
        (setf node-id (first node-id))
        (return)))
    node-id))


(defun left-browser-fn (node window)
  

  "What to do when we left-select a node."
  
  (left-selection window (node-object node)))


(defun middle-browser-fn (node window)
  "What to do when we middle-select a node."
  
  (middle-selection window (node-object node)))


(defun title-left-browser-fn (window)
  "What to do if we left-select the title."
  
  (title-left-selection window))


(defun title-middle-browser-fn (window)
  "What to do if we middle-select the title."
  
  (title-middle-selection window))


(defun tree-roots (node-lst)
  "Computes the set of Root nodes for a lattice - Those with no connections ~
   TO them in list of nodes."
  
  (let ((first-element (graph-node-id (first node-lst)))
        root-lst)
    (dolist (n node-lst)
      (push (graph-node-id n)
            root-lst))
    (dolist (n node-lst)
      (dolist (d (to-links n))
        (setf root-lst (remove d root-lst))))
    (or root-lst (list first-element))))


(defun understands (self selector &rest args)
  "Returns NIL if SELF doesn't understand the method SELECTOR, T otherwise."
  
  (if (and (generic-function-p selector) (objectp self))
    (compute-applicable-methods selector (append self args))))
#|
(defun understands (self selector &rest args)
  "Returns NIL if SELF doesn't understand the method SELECTOR, T otherwise."
  
  (if (and (generic-function-p selector) (objectp self))
    (let ((arg-classes
           (make-list (length args) :initial-element (find-class t))))
      (dolist (super (class-precedence-list  (class-of self)) nil)
        (when (member (get-method (symbol-function selector)
                          nil
                          (append (list super) arg-classes)
                          nil)
          (return t))))
    nil)))
|#

