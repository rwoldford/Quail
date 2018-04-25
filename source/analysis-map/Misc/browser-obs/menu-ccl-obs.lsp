;;;
;;; this file should be loaded after class browser has been created
;;;
;;; it contains the functions necessary to build the menubar associated with each 
;;; class of browser, the functions necessary to activate those menubars and to
;;; apply the correct methods
;;;
;;; this file contains also the building of pop-up hierarchical menus
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; title menu : MENUBAR
;;;

(in-package 'pcl)

(defmethod initialize-instance :after ((browser browser::browser) &key)

;;; when a new browser is created, we must build a new menubar for its class
;;; if it doesn't already exist
;;; the building of menu should be specialized for Z-Browser which has both 
;;; left and middle items

  ;; the browse-font slot is using a Xerox format and must be translated into a
  ;; mac format

  (setf (slot-value browser 'browser::browse-font)
        (mapcar #'(lambda (x)
                    (if (symbolp x)
                      (let ((x-name (symbol-name x)))
                         (cond  
                           ((string= x-name "HELVETICA") "helvetica")
                           ((string= x-name "BOLD") ':bold)
                           ((string= x-name "MEDIUM") ':plain)
                           (t x)))
                      x))
                (slot-value browser 'browser::browse-font)))
  (specific::make-menubar browser))


(in-package 'specific)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(make-menu
          menu-p
          select-in-menu)))


(defmethod make-menubar ((browser browser::browser) &aux menu)

;;; create a menubar for this class if it doesn't already exist and cache it

  (declare (special *class-menubar-cache*))

  (or (boundp '*class-menubar-cache*) 
      (setq *class-menubar-cache* nil))
  (unless (assoc (class-of browser) *class-menubar-cache*)

    ;; if menubar doesn't exist, create it and save it in association list

    (setq menu (create-menu-with-list 
                             (slot-value browser 'browser::left-title-items)
                             "Edit"))
    (push (cons (class-of browser)
                (push (first *default-menubar*)
                      menu))
          *class-menubar-cache*)))


(defun create-menu-with-list (the-list title)

;;; returns a list of menus, the first of them having the title title and the other
;;; ones if any the title of the corresponding item of the menu hierarchically 
;;; superior
 

  (let ((menu (oneof *menu* :menu-title title))
        menu-list
        (action '(browser-action nil)))
    (dolist (item the-list)
       (when (second item)
          (setf (second action) (list 'quote (second item)))
          (ask menu (add-menu-items 
                        (oneof *menu-item* 
                               :menu-item-title (first item)
                               :menu-item-action action))))
       (if (> (length item) 3)
           (dolist (sub-menu (create-menu-with-list (cdr (fourth item)) (first item))) 
              (push sub-menu menu-list))))
    (push menu menu-list)
    menu-list))


(defun install-browser-menubar (browser)

;;; make the menubar of the browser represented in window be the active menubar
;;; this function is called when window is activated

  (declare (special *class-menubar-cache*))
  (set-menubar (cdr (assoc (class-of browser)
                           *class-menubar-cache*))))
     

(defobfun (window-close graph-window) ()

;;; closes the window if command is down with mouse button
;;; hides it if just mouse button is down
;;; when a window is closed, we must deactivate its menubar.

  (declare (special *current-event*))
  (if (and (boundp '*current-event*)
           (eq (rref *current-event* event.modifiers) 256))
      (usual-window-close)
      (_HideWindow :ptr (ask (self) wptr)))
  (set-menubar *default-menubar*))



(defun browser-action (action)

;;; apply action to the selected window when a selection is made in a menu

    (browser:do-selected-command (get-window-browser (front-window)) action nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; some function to update pcl object when window characteristics are changed
;;;

(defobfun (set-window-size graph-window) (h &optional v 
                                       &aux (window (self)) browser)

;;; when window is resized, controls must be resized
;;; h and v are the new dimensions of content-region
  
  ;; resize the window

  (usual-set-window-size h v)
  (unless v
      (setq v (point-v h))
      (setq h (point-h h)))
     
  ;; save the new size in the browser object       
  (when (setq browser (get-window-browser window))
        (setf (slot-value browser 'browser::width) (- h 24))
        (setf (slot-value browser 'browser::height) (- v 16)))
  (resize-scroll-controls (self))
  (_InvalRgn :ptr (rref (ask (self) wptr) window.strucRgn))
  (redisplay-graph-in-rect window (graph-rect window))
  (dolist (control (dialog-items))
     (ask control (dialog-item-draw)))
  (ask window (window-draw-grow-icon)))
   
 
(defobfun (set-window-position graph-window) (h &optional v)

;;; moving a window
    
     ;; move the window

     (usual-set-window-position h v)
     (unless v
        (setq v (point-v h))
        (setq h (point-h h)))
     ;; save the new position in the browser object

     (let ((browser (get-window-browser (self))))
        (when browser
           (setf (slot-value browser 'browser::left) h)
           (setf (slot-value browser 'browser::bottom) v))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; pop up hierarchical menus implementation
;;;

(defobject *pop-up-menu* *menu* *menu-item*)

(defobfun (exist *pop-up-menu*) (init-list)
  (have 'menu-rect nil)
  (have 'title-rect nil)
  (have 'default-item 1)
  (usual-exist (init-list-default init-list))
  (set-command-key #x1B)
  (set-menu-item-check-mark menu-id))

(defobfun (menu-select *pop-up-menu*)
          (&aux selection selected-menu selected-menu-item
                (mouse-point (rref *current-event* Event.where)))
  "Update the menu's items then displays the pop-menu.  Default-item is the
  item which will come up selected  when the menu is displayed."
  (menu-update)
  (setq selection (_PopUpMenuSelect
                   :ptr menu-handle
                   :word (point-v mouse-point)
                   :word (- (point-h mouse-point) 2)
                   :word (or default-item 0)
                   :long)
        ;we get the selected menu in case you want to break the rules
        ;and use hierarchical menus in a pop-up menu
        selected-menu (cdr (assoc (ash (logand #xFFFF0000 selection) -16)
                                  *menu-id-object-alist*))
        selected-menu-item (logand #x0000FFFF selection))
  (ask selected-menu
    (unless (eq selected-menu-item 0)
      (setq default-item selected-menu-item)
      (ask (nth (- selected-menu-item 1) (menu-items))
        (menu-item-action)))))

(defobfun (menu-install *pop-up-menu*) ()
  "Creates the actual Macintosh menu with all of the menu's current items."
  (with-pstrs ((menu-title (menu-title)))
    (setq menu-handle (_NewMenu :word menu-id
                                     :ptr menu-title
                                     :ptr))
    (dolist (item (menu-items))
      (with-pstrs ((data (simple-string (ask item (menu-item-title)))))
        (_AppendMenu :ptr menu-handle
                     :ptr data))
        (ask item
          (if (eq (command-key) #x1B)
              (menu-install))
          (set-command-key (command-key))
          (set-menu-item-check-mark (menu-item-check-mark))
          (if (menu-item-enabled-p)
            (menu-item-enable)
            (menu-item-disable))
          (unless (eq (menu-item-style) :plain)
            (set-menu-item-style (menu-item-style))))))
    (_InsertMenu :ptr menu-handle
                 :word -1))

(defun make-menu (&key (items nil)
                       (title nil)
                       (when-selected-fn nil)
                       (when-held-fn nil)
                       (change-offset-fn nil)
                  &aux menu)
 
    (declare (ignore when-selected-fn when-held-fn change-offset-fn)) 
    (labels ((force-string (x) (if (stringp x)  x (format nil "~a" x)))
             (make-submenu (a-menu-item)
              (labels ((make-menu-item (a-menu-item)
                       (oneof *menu-item* 
                              :menu-item-title (force-string (first a-menu-item))
                              :menu-item-action (list 'quote (second a-menu-item)))))
                    (if (>= (length a-menu-item) 4)
                       (oneof *pop-up-menu*
                              :menu-title (force-string (first a-menu-item))
                              :menu-item-title (force-string (first a-menu-item))
                              :menu-item-action (list 'quote (second a-menu-item))
                              :menu-items (mapcar #'make-submenu (rest (fourth a-menu-item))))
                       (make-menu-item a-menu-item)))))
            (setq menu (oneof *pop-up-menu*
                              :menu-title (if (null title) "Untitled" (force-string title))
                              :menu-items (mapcar #'make-submenu items)))
            (ask menu (have 'items items))
            (ask menu (menu-install))
            menu))

(defun menu-p (menu)
  (typep menu *pop-up-menu*))

(defun select-in-menu (menu)

;;; menu-select works only if button is down so if this is not the case, we wait
;;; until button is pressed

  (unless (logbitp 8 (_Button :word))
          (loop
             (when (logbitp 8 (_Button :word))
                   (return)))
          (rlet ((event :event))
             (_GetNextEvent :word 2 :ptr event :word)))
  (ask menu (menu-select)))

