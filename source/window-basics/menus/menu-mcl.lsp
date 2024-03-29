;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               menu-mcl.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;
;;;  Authors:
;;;     H.A. Chipman 1991
;;;     C.B. Hurley 1989-1992
;;;     J.A. McDonald 1988-89
;;;     R.W. Oldford 1989-1995
;;;     J.O. Pedersen 1988-89
;;;     
;;;
;;;----------------------------------------------------------------------------------

;;;  Note that a fair bit of this is *NOT* MCL dependent   ... rwo

(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) 
  (export  '(make-menu wb-pop-up-menu destroy-menu set-menu-fns menu-p put-menu-prop
             get-menu-prop installed-p menu select-in-menu get-menu-item
             disable-menu-item enable-menu-item check-menu-item
             set-menu-item-string)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(ccl::pop-up-menu-default-item ccl::menu-object
            ccl::pop-up-menu-auto-update-default) :wb))

;;; --------------------------------------------------------------------------------------
;;;
;;;                           Hierarchical pop-up menus
;;;
;;; --------------------------------------------------------------------------------------


;;; First set up a slot so that each menu-item knows which menu it belongs to.
;;; 

(defclass wb-menu-mixin ()
  ((user-properties   :initarg :user-properites
                      :initform nil
                      :accessor user-properties-of)
   (sub-item-fn      :initarg :sub-item-fn
                      :initform #'default-sub-item-fn
                      :accessor sub-item-fn-of)
   (when-held-fn     :initarg :when-held-fn
                      :initform #'default-when-held-fn
                      :accessor when-held-fn-of)
   (when-unheld-fn   :initarg :when-unheld-fn
                      :initform #'default-when-unheld-fn
                      :accessor when-unheld-fn-of)
   (when-selected-fn :initarg :when-selected-fn
                      :initform #'default-when-selected-fn
                      :accessor when-selected-fn-of)
   (items             :initarg :items
                      :initform NIL
                      :accessor items-of)
   (super-menu        :accessor super-menu-of
                      :initarg :super-menu
                      :initform NIL
                      :documentation
                      "The menu which contains this menu (or menu-item), if any.")))

(defclass wb-menu (wb-menu-mixin ccl::menu)
  ())

(defclass wb-menu-item (wb-menu-mixin ccl::menu-item)
  ())

(defmethod menu-item-action ((mi wb-menu-item))
  (funcall (when-selected-fn-of mi)
           (items-of mi)
           (super-menu-of mi)
           (mouse-state)))

;;;-----------------------------------------------------------------------------------------
;;;
;;;                          pop-up menu
;;;
;;;-----------------------------------------------------------------------------------------

(defclass wb-pop-up-menu (wb-menu-mixin ccl::pop-up-menu)
  ()
  (:default-initargs :auto-update-default nil))

(defmethod menu-item-action ((mi wb-pop-up-menu))
  (funcall (when-selected-fn-of mi)
           (items-of mi)
           (super-menu-of mi)
           (mouse-state)))

;;;---------------------------------------------------------------------------------------
;;;
;;;                    Making the pop-menu
;;;
;;;---------------------------------------------------------------------------------------
;;;
;;; It would be nice if a single kind of menu record was used.  Sigh ...
;;;
;;;
;;; Don't yet know how to grab the mouse in a menu to tell which item it's
;;; over and for how long.  Hence when-held-fn & when-unheld-fn are never
;;; called.
;;; ... rwo
;;;

(defun make-menu
       (&key items sub-item-fn when-selected-fn when-held-fn when-unheld-fn
             (title NIL)
             (font *default-menu-font*)
             (change-offset-fn nil)
             (menu-type :pop-up)
             &aux menu)
  
  (declare (ignore change-offset-fn font)
           (special *default-menu-font*))
  
  (let ((*menu-class* (if (eq menu-type :pop-up)
                        'wb-pop-up-menu
                        'wb-menu)))
    (labels
      ((force-string (x) (if (stringp x)  x (format nil "~a" x)))
       (make-wb-sub-menu (outer-menu sub-menu-items)
         (labels 
           ((make-wb-menu-item (menu sub-menu-items)
              (let ((mi
                     (make-instance
                      'wb-menu-item
                      :menu-item-title (force-string (first sub-menu-items))
                      :items sub-menu-items
                      :super-menu menu)))
                (set-menu-fns mi
                              :selected when-selected-fn
                              :held when-held-fn
                              :unheld when-unheld-fn
                              :sub-item sub-item-fn)
                mi)))
           
           (if (and (> (length sub-menu-items) 4)
                    (eq (fourth sub-menu-items) :sub-items))
             (let 
               ((sub-menu
                 (make-instance *menu-class*
                                :menu-title (force-string (first sub-menu-items))
                                :menu-item-title (force-string (first sub-menu-items))
                                :items sub-menu-items
                                :super-menu outer-menu)
                 ))
               (set-menu-fns sub-menu
                             :selected when-selected-fn
                             :held when-held-fn
                             :unheld when-unheld-fn
                             :sub-item sub-item-fn)
               (loop for sub-item in (fifth sub-menu-items) when sub-item
                     do
                     (add-menu-items sub-menu (make-wb-sub-menu sub-menu sub-item)))
               sub-menu)
             (make-wb-menu-item outer-menu sub-menu-items)))))
      
      (setq menu (make-instance *menu-class*
                                :menu-title (if title (force-string title) "Untitled")
                                :items items))
      
      (set-menu-fns menu :selected when-selected-fn :held when-held-fn
                    :unheld when-unheld-fn :sub-item sub-item-fn)
      
      
      (loop for item in items when item
            do
            (add-menu-items menu (make-wb-sub-menu menu item)))
      menu)))




(defmethod menu-select ((menu wb-pop-up-menu) num
                        &aux selection
                        selected-menu
                        selected-menu-item
                        ;;(a-rect (pop-up-menu-rect menu))
                        (pos
                         (h-draw:make-point (screen-mouse-x) 
                           (screen-to-host-y (screen-mouse-y))))
                         ;(with-focused-view (view-container menu)
                         ;      (%local-to-global 
                         ;       (wptr menu)
                         ;       (rref a-rect :rect.topleft)))
                         
                        )
  (declare (ignore num))
  (menu-update menu)
  (setq selection (#_PopUpMenuSelect
                   :ptr (slot-value menu 'menu-handle)
                   :word (h-draw:point-y pos)
                   :word (- (h-draw:point-x pos)
                            2)
                   :word (or (pop-up-menu-default-item menu) 0)
                   :long)
        ;we get the selected menu in case you want to break the rules
        ;and use hierarchical menus in a pop-up menu
        selected-menu (menu-object (ash (logand #xFFFF0000 selection) -16))
        selected-menu-item (logand #x0000FFFF selection))
  (unless (eq selected-menu-item 0)
    (when (pop-up-menu-auto-update-default menu)
        (setf (pop-up-menu-default-item selected-menu) selected-menu-item))
    (menu-item-action
     (nth (- selected-menu-item 1) (menu-items selected-menu)))))

;;; ---------------------------------------------------------------------------------
;;;
;;;                                Popping the menu
;;;
;;; ---------------------------------------------------------------------------------

;;; This is the one that does the job!
;;;

;;(defun installed-p (menu)
;; (menu-handle menu))

(defun menu (menu &key position)
  "Pops menu up at position in screen-coordinates.
   (If given, otherwise at mouse-position).
   Title menus on the Mac will appear only on the menu-bar."
  (unless (menu-installed-p menu)  ;; check if menu is already installed... cbh
                                  ;; otherwise function doesn't work w/o
                                  ;; first deinstalling the menu
    (menu-install menu))
  (if (typep menu 'wb-pop-up-menu)
    (select-in-menu menu :position position)))

;;;
;;; The following function needs to operate within an event-handler
;;; (e.g. to be called after a mouse has been depressed inside a window)
;;;

(defun select-in-menu (menu &key position)
  ;; menu-select works only if button is down so
  ;; if this is not the case, we wait
  ;; until the button is depressed.
  (declare (special *quail-query-io*))
  (unless (mouse-down-p)
    (format *quail-query-io*
            "Press and hold the mouse button; release to make your selection. ~&~
             Waiting....")
    (loop until (mouse-down-p) do NIL)
    (rlet ((event :eventRecord))
             (#_GetNextEvent :word 2 :ptr event :word))) ;; cbh add # for carbon
  (menu-select menu position))        


;;;-------------------------------------------------------------------------------------
;;;
;;;                 Miscellaneous menu functions
;;;
;;;-------------------------------------------------------------------------------------

(defun destroy-menu (menu)
  "Destroys menu so that its space can be reclaimed."
  ;;(declare (ignore menu))
  ;; **** FIX SOMETIME
  (when (slot-exists-p menu 'ccl::MENU-ID)
    (when (slot-exists-p menu 'ccl::item-list)
      (loop for item in (slot-value menu 'ccl::item-list)
            do
            (destroy-menu item))
      (setf (slot-value menu 'ccl::item-list) NIL))
    (when (ccl::menu-handle menu)
      (ccl::menu-deinstall menu)
      ;; (print "Deinstalled")
      )
    ;;(#_deletemenu (ccl::menu-id menu))
    ;;(#_disposemenu (ccl::menu-handle menu))
    ;;(#_releaseresource (ccl::menu-handle menu))
    ;;(#_rmveresource (ccl::menu-handle menu))
    )
  )


(defun set-menu-fns (menu &key (selected nil) (held nil) (unheld nil) (sub-item nil))
  (if (and selected (functionp selected))  (setf (when-selected-fn-of menu) selected))
  (if (and held     (functionp held))      (setf (when-held-fn-of menu) held))
  (if (and unheld   (functionp unheld))    (setf (when-unheld-fn-of menu) unheld))
  (if (and sub-item (functionp sub-item))  (setf (sub-item-fn-of menu) sub-item)))

(defun menu-p (menu)
  (or (typep menu 'wb-menu)
      (typep menu 'wb-pop-up-menu)))

(defun put-menu-prop (menu property value)
  "Stores value on property of menu"
  (setf (user-properties-of menu) (acons property value (user-properties-of menu))))


(defun get-menu-prop (menu property)
  "Returns the value associated with the property on menu"
  (cdr (assoc property (user-properties-of menu))))


;;; ---------------------------------------------------------------------------------
;;;
;;;                   Modifying the menu appearance
;;;
;;; ---------------------------------------------------------------------------------
(defun get-menu-item (menu item-string)
  "Returns the menu item identified by item string."
  (let ((item (find-menu-item menu item-string)))
    (if item item
        (loop for m in (menu-items menu)
              thereis (and (menu-p m) 
                           (get-menu-item m item-string))))))

(defun disable-menu-item (menu item-string)   
  "Disables the menu item identified by item-string."
    (let ((item (get-menu-item menu item-string)))
      (if item
        (menu-item-disable item))))


(defun enable-menu-item (menu item-string)   
  "Enables the menu item identified by item-string."
  (let ((item (get-menu-item menu item-string)))
      (if item
        (menu-item-enable item))))

   
(defun check-menu-item (menu item-string &optional (check-char nil) )
 "Places a check-char beside item identified by item-string ~
  if check-char is nil no check mark appears ~
  if check-char is t a default check mark is used."
  (let ((item (get-menu-item menu item-string)))
    (if item
        (set-menu-item-check-mark item check-char))))


(defun set-menu-item-string (menu item-string new-string)
  "Changes the string for the menu item currently ~
   identified by item-string to new-string."
  (let ((item (get-menu-item menu item-string)))
    (if item
      (set-menu-item-title item new-string))))
  
