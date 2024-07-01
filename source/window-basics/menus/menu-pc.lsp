;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                               menu-pc.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;  Authors:
;;;     H.A. Chipman 1991
;;;     C.B. Hurley 1989-1992
;;;     J.A. McDonald 1988-89
;;;     R.W. Oldford 1989-1992
;;;     J.O. Pedersen 1988-89
;;;     G.W. Bennett 1996
;;;     
;;;-----------------------------------------------------------------------------
;;;  Note that a fair bit of this is *NOT* MCL dependent   ... rwo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;-- gwb
(in-package :wb)
(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(make-menu wb-pop-up-menu destroy-menu set-menu-fns menu-p 
   put-menu-prop get-menu-prop installed-p menu select-in-menu get-menu-item
           disable-menu-item enable-menu-item check-menu-item
           set-menu-item-string
           menu-installed-p)))
;;(eval-when (compile eval load)
;;  (import '(ccl::pop-up-menu-default-item ccl::menu-object
;;            ccl::pop-up-menu-auto-update-default) :wb))
;;; Until I can resolve all the problems with embedding the
;;; source code, or until the methods are included in the
;;; ----------------------------------------------------------------------------
;;;                           Hierarchical pop-up menus
;;; ----------------------------------------------------------------------------
;;; First set up a slot so that each menu-item knows which menu it belongs to.
;;; 
(defclass wb-menu-mixin ()
      ((user-properties   :initarg :user-properties
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
        "The menu which contains this menu (or menu-item), if any.")
       (help-string :accessor cg::help-string
         :initarg :help-string
         :initform ""
         :documentation
         "The help-string for this menu")))

(defclass wb-menu (wb-menu-mixin cg::pull-down-menu) ())

#|
;;(defclass wb-menu-item (wb-menu-mixin ccl::menu-item) ())
;; menu-item is a STRUCTURE in ACLPC
(defstruct (wb-menu-item (:include cg::menu-item))
   (user-properties NIL)
   (sub-item-fn #'default-sub-item-fn)
   (when-held-fn #'default-when-held-fn)
   (when-unheld-fn #'default-when-unheld-fn)
   (when-selected-fn #'default-when-selected-fn)
   (items NIL)
   (super-menu NIL)
   )
|#

(defclass wb-menu-item (cg::menu-item)
    ((user-properties :initform NIL :accessor user-properties-of)
     (sub-item-fn :initform #'default-sub-item-fn
      :accessor sub-item-fn-of )
     (when-held-fn :initform #'default-when-held-fn
      :accessor when-held-fn-of)
     (when-unheld-fn :initform #'default-when-unheld-fn
      :accessor when-unheld-fn-of)
     (when-selected-fn :initform #'default-when-selected-fn
      :accessor when-selected-fn-of)
     (items :initform NIL :accessor items-of)
     (super-menu :initform NIL :accessor super-menu-of)
     )
   )

(defmethod when-held-fn-of ((thing cg::menu-item))
   (cdr
    (assoc 'when-held-fn
      (second
       (member 'wb-menu-fns
         (cg::plist thing))))))

(defmethod when-held-fn-of ((thing T))
   NIL)

(defmethod (setf when-selected-fn-of) (new-value (thing T))
  (declare (ignorable new-value)) ; 27JUL2023
   NIL)

(defmethod (setf when-selected-fn-of) (new-value (thing cg::menu-item))
   (setf (cdr
          (assoc 'when-selected-fn
            (second
             (member 'wb-menu-fns
               (cg::plist thing)))))
         new-value))

(defmethod (setf sub-item-fn-of) (new-value (thing T))
  (declare (ignorable new-value)) ; 27JUL2023
   NIL)

(defmethod (setf sub-item-fn-of) (new-value (thing cg::menu-item ))
   (setf (cdr
          (assoc 'sub-item-fn
            (second
             (member 'wb-menu-fns
               (cg::plist thing)))))
         new-value))

(defmethod (setf when-unheld-fn-of) (new-value (thing T))
  (declare (ignorable new-value)) ; 27JUL2023
   NIL)

(defmethod (setf when-unheld-fn-of) (new-value (thing cg::menu-item))
   (setf (cdr
          (assoc 'when-unheld-fn
            (second
             (member 'wb-menu-fns
               (cg::plist thing)))))
         new-value))


;;;-----------------------------------------------------------------------------
;;;                          pop-up menu
;;;-----------------------------------------------------------------------------

(defclass wb-pop-up-menu (wb-menu-mixin cg::pop-up-menu) ())

(defun set-menu-fns (menu &key (selected nil selected?) (held nil held?) 
                                  (unheld nil unheld?) (sub-item nil sub-item?)
                                  (user-props NIL user-props?))
     (cond
               ((typep menu 'wb-menu-item)
                (if user-props?
                   (setf (user-properties-of menu) user-props))
                (if selected?
                   (setf (when-selected-fn-of menu) selected))
                (if held?
                   (setf (when-held-fn-of menu) held))
                (if unheld?
                   (setf (when-unheld-fn-of menu) unheld))
                (if sub-item?
                   (setf (sub-item-fn-of menu) sub-item))
                )
               
               ((cg::menu-item-p menu) 
                (let ((wb-menu-fns (member 'wb-menu-fns (cg::plist menu))))
                    (cond
                              (wb-menu-fns
                               (if user-props?
                                  (setf (cdr (assoc 'user-properties wb-menu-fns))
                                          user-props))
                               (if selected?
                                  (setf (cdr (assoc 'when-selected-fn wb-menu-fns))
                                          selected))
                               (if held?
                                  (setf (cdr (assoc 'when-held-fn wb-menu-fns))
                                          held))
                               (if unheld?
                                  (setf (cdr (assoc 'when-unheld-fn wb-menu-fns))
                                          unheld))
                               (if sub-item?
                                  (setf (cdr (assoc 'sub-item-fn wb-menu-fns))
                                          sub-item))
                               )
                              (T
                                 (setf (cg::plist menu)
                                         (append
                                            (list 'wb-menu-fns
                                              (list
                                                (cons 'user-properties user-props)
                                                (cons 'when-selected-fn selected)
                                                (cons 'when-held-fn held)
                                                (cons 'when-unheld-fn unheld)
                                                (cons 'sub-item-fn sub-item)))
                                            (cg::plist menu)))))))
               ((cg::menup menu)
                (when (typep menu 'wb-menu-mixin)
                     (if selected?
                        (setf (when-selected-fn-of menu) selected))
                     (if held?
                        (setf (when-held-fn-of menu) held))
                     (if unheld?
                        (setf (when-unheld-fn-of menu) unheld))
                     (if sub-item?
                        (setf (sub-item-fn-of menu) sub-item)))
                
                
                )
               (T NIL)))
 
;;;----------------------------------------------------------------------------
;;;                    Making the pop-menu
;;;---------------------------------------------------------------------------
;;; It would be nice if a single kind of menu record was used.  Sigh ...
;;; Don't yet know how to grab the mouse in a menu to tell which item it's
;;; over and for how long.  Hence when-held-fn & when-unheld-fn are never
;;; called.
;;; ... rwo

;; An ACLPC version of make-menu follows

;; NOTES on make-menu specific to ACLPC :-
;; (1) menus are OBJECTS which inherit from STANDARD-CLASS
;; (2) menu-items are STRUCTURES which inherit from STRUCTURE-CLASS
;; (3) to make sub-menus, ie menu-items which are in fact menus,
;;     proceed as follows :
;;      (cg::make-menu-items :title what-should-appear-in-the-supermenu
;;          :value menu-to-become-the-submenu)
;;     (add-to-menu some-higher-level-menu this-thing-just-made)
;; (4) to get a sub-menu's :title correct try setting it to 
;;     (cg::stream-title some-higher-level-menu)
;; (5) because menu-items are STRUCTURES I had to change the name of the
;;     MCL function make-wb-menu-item to build-wb-menu-item since
;;     make-STRUCTURE is a primitive..
;; (6) .. and I had to replace make-instance by just that primitive to
;;     create mi inside build-wb-menu-item.
;; gwb - on February 08, 1996 
;;;;;;;;;
;;; Revised version of make-menu allowing for when-x-fns

(defun make-menu
      (&key items sub-item-fn 
        (when-selected-fn #'default-when-selected-fn) 
        when-held-fn when-unheld-fn
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
            (selection-fn-wrap (menu menu-item window)
             ;; Get the when-selected-fn -- first from the menu-item and if not
             ;; found there, then from the menu
             (let ((selected-fn
                      (cdr
                         (assoc 'when-selected-fn
                            (second
                               (member 'wb-menu-fns 
                                   (cg::plist menu-item)))))))
                 (unless (functionp selected-fn)
                      (setf selected-fn
                              (or (when-selected-fn-of menu)
                                   #'(lambda (a b c)
                                        (declare (ignore a b c))
                                        (cg::funcall-menu-item
                                         menu menu-item window)))))
                 ;; Now call the selected function on the appropriate arguments
                 (funcall selected-fn
                   (cg::value menu-item)
                   menu
                   :left  ;; should be the current (mouse-button)
                   ) 
                 )
             )
            (make-wb-sub-menu (outer-menu sub-menu-items)
             (labels 
               ((build-wb-menu-item (menu sub-menu-item)
                 (let ((mi
                          (make-instance 'wb-menu-item
                           :title (force-string (first sub-menu-item))
                           :value sub-menu-item
                           :help-string (third sub-menu-item)
                           :items sub-menu-item
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
                          (cg::open-menu '() *menu-class* (cg::screen cg::*system*)
                           :title (force-string (first sub-menu-items))
                           :items sub-menu-items
                           :super-menu outer-menu
                           :help-string (third sub-menu-items)
                           :selection-function #'selection-fn-wrap)
                          ))
                      (set-menu-fns sub-menu
                       :selected when-selected-fn
                       :held when-held-fn
                       :unheld when-unheld-fn
                       :sub-item sub-item-fn)
                      (loop for sub-item in (fifth sub-menu-items) when sub-item
                        do
                        (cg::add-to-menu sub-menu 
                         (make-wb-sub-menu sub-menu sub-item)))
                      ;; Here is where I must convert to a cg::menu-item
                      (make-instance 'cg::menu-item
                       :title (cg::title sub-menu)
                       :help-string (cg::help-string sub-menu)
                       :value sub-menu))
                  (build-wb-menu-item outer-menu sub-menu-items)))))
           (setq menu (cg::open-menu '() *menu-class* (cg::screen cg::*system*)
                               :title (if title (force-string title) "Untitled")
                               :help-string (third items)
                               :items items
                               :selection-function #'selection-fn-wrap))
           (set-menu-fns menu :selected when-selected-fn :held when-held-fn
            :unheld when-unheld-fn :sub-item sub-item-fn)
           (loop for item in items when item
             do
             (cg::add-to-menu menu (make-wb-sub-menu menu item)))
           menu)))

;;; ----------------------------------------------------------------------------
;;;                                Popping the menu
;;; ----------------------------------------------------------------------------
;;; This is the one that does the job!
;;; In ACLPC terms the menu of the following function will be
;;; assumed to have its handler already defined inside
;;; whatever made it - presumably make-menu
(defun menu (menu &key (position NIL position?))
     "Pops menu up at position in screen-coordinates.
      (If given, otherwise at mouse-position).
      Title will appear on PC only if it's made into the first item
      otherwise :title slot is ignored completely for pop-up-menus."
 (when (menu-installed-p menu) ; check if it's already visible
   (cond
     (position?
       (cg::pop-up-menu menu position))
     (T (cg::pop-up-menu menu)))
   )
)

;; Here is an elementary menu-installed-p
(defun menu-installed-p (menu)
   (menu-p menu)
   )

;;; The following function needs to operate within an event-handler
;;; (e.g. to be called after a mouse has been depressed inside a window)

;;; In ACLPC terms the selection function is supplied as an option
;;; to the creation of the menu and the mouse actions are integral
;;; although the selection processes are quite different for the
;;; two types pull-down and pop-up

;;;-----------------------------------------------------------------------------
;;;                 Miscellaneous menu functions
;;;-----------------------------------------------------------------------------
;; Added 19 June 1998. See kill-menu.lsp
(defun submenus? (mnu)
   "Does mnu have submenus ?~
    Be careful about types!"
   (let* ((mi (cg::menu-items mnu))
          (miv (mapcar #'cg::value mi))
          (mit (mapcar #'type-of miv)))
      (values miv 
        (or (position 'wb-menu mit)
            (position 'wb-pop-up-menu mit)))
      ))

;;; Added 19 June 1998. See kill-menu.lsp
;;; was killmenu now is destroy-menu
(defun destroy-menu (mnu)
   "Finds the first outer submenu and kills it.~
    Then checks backwards etc. until mnu is closed."
    (declare (special a b))
   ;; Following is necessary since submenus? doesn't work on closed streams
  (unless  (not (cg::open-stream-p mnu))
    ;;(typep mnu 'cg::closed-stream)
   (multiple-value-setq (a b)
     (submenus? mnu))
   (cond (b
      (destroy-menu (elt a b))
          mnu)
         (T 
           ;; Grab super-menu NOW in case we need to back-up
           (let ((sm (super-menu-of mnu)))
           (cg::close mnu)
              (when sm
           (destroy-menu sm)))
           mnu)))
  )

(defun menu-p (menu)
     (or (typep menu 'wb-menu)
          (typep menu 'wb-pop-up-menu)))

(defun put-menu-prop (menu property value)
     "Stores value on property of menu"
     (setf (user-properties-of menu) (acons property value 
                                                           (user-properties-of menu))))

(defun get-menu-prop (menu property)
     "Returns the value associated with the property on menu"
     (cdr (assoc property (user-properties-of menu))))
;;; ----------------------------------------------------------------------------
;;;                   Modifying the menu appearance
;;; ----------------------------------------------------------------------------
;;; These seem to have to do with disabling and
;;; enabling items of a menu
;;; and with checking which are built-in to ACLPC
;;; fairly directly
;;; Jan 24, 1996 GWB

;;; A list version of finding menu-items
#|
(defun list-get-menu-item  (mi-list item-string)
   (let ((result NIL))
      (loop for m-i in mi-list
        until result
        do
        (if (cg::menup (cg::value m-i))
            (setf result 
             (list-get-menu-item
              (cg::menu-items (cg::value m-i))
              item-string))
            (if (equal (string (cg::title m-i)) item-string)
               (setf result m-i))
           )
        )
      (print result)
      result))
|#

(defun list-get-menu-item (mi-list item-string)
   (labels
     ((lgmi-aux (mi-list)
        (cond ((and (cg::menu-item-p mi-list)
                    (equal (string (cg::title mi-list)) item-string)
                    )
               mi-list)
              ((null mi-list) 
               NIL)
              ((listp mi-list)
               (or (lgmi-aux (first mi-list))
                   (lgmi-aux (rest mi-list))))
              ((cg::menup  (cg::value  mi-list))
               (if (string-equal (string (cg::title mi-list)) item-string)
                            mi-list)
               (lgmi-aux (cg::menu-items (cg::value  mi-list)))
                  )
           )))
     (lgmi-aux mi-list))
   )

;; Now the function we need
(defun get-menu-item (menu item-string)
   (when menu
      (cond ((cg::menu-item-p menu)
             (list-get-menu-item (list menu) item-string))
            (t (list-get-menu-item (cg::menu-items menu) item-string)))
      ))

(defun disable-menu-item (menu item-string)
     "Disables the top-level menu item identified by item-string."
     (let ((item (get-menu-item menu item-string)))
         (if item
             ;(cg::set-menu-item-available-p item NIL) 18oct05
             (setf (cg::available item) NIL) ;18oct05
           )))

(defun enable-menu-item (menu item-string)
 "Enables the menu item identified by item-string."
  (let ((item (get-menu-item menu item-string)))
      (if item  
          ;(cg::set-menu-item-available-p item T) 18oct05
          (setf (cg::available item) T) ;18oct05
        )))

(defun check-menu-item (menu item-string &optional (check-char nil))
   "Places a check-char beside item identified by item-string ~
    if check-char is nil no check mark appears ~
    if check-char is t a default check mark is used."
   (let ((item (get-menu-item menu item-string)))
      (if item
          ;(cg::set-menu-item-selected-p item check-char) 18oct05
          (setf (cg::selected item) check-char) ;18oct05
        )))

(defun set-menu-item-string (menu item-string new-string)
     "Changes the TITLE for the menu item currently ~
      identified by item-string to new-string."
     (let ((item (get-menu-item menu item-string)))
         (if item
             ;(cg::set-stream-title item new-string) 18oct05
             (setf (cg::title item) new-string) ;18oct05
           )))
