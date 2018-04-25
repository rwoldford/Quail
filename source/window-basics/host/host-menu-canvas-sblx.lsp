;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              host-menu-canvas-sblx.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) Statistical Computing Laboratory
;;;               University of Waterloo
;;;               Canada
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;  Authors:
;;;     R.W. Oldford 1989-1991
;;;     G.W. Bennett 1996 2017
;;; This file supersedes mn-c-apc.lsp
;;; was new-hmc-pc.lsp but became hmc-pc.lsp of May 19 1998
;;; to test building of Quail menubar ... gwb 051998
;;;-------------------------------------------------------------------------

(in-package :wb)
(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(*last-menubar* *system-default-menubar*)))

(defclass host-menu-canvas ()
  ()
  (:documentation "A class to define how title menus should appear."))

(defvar *system-default-menubar*
  NIL
  "The default menubar of the system.")

;; Added 042098 gwb
(defvar *extra-bar-height* NIL
   "The width of an extra line on *quail-menubar*")

;; Added 042098 gwb
(defun set-extra-bar-height (&key (height 23))
     "Sets the vertical adjustment of *quail-menubar* when an extra~
       line is needed"
     (setf *extra-bar-height* height))

;; Added 042098 gwb
(defvar *quail-menubar-window* NIL
   "The window which hosts the Quail Menubar~
     needed also to host the font wrt which menu calculations are performed.
     Its value will be established in make-default-system-menubar
     which is called by set-system-default-menubar.")


(defun menu-title-width (mnu &optional (stream *quail-menubar-window*))
     "Finds the pixel width of mnu on the stream (default
      *quail-menubar-window*).
       mnu can be a menu or a menu-item."
     ;; 20 is a fudge factor.
     (if (cg::menu-item-p mnu)
         (+ 20 (cg::with-device-context (hdc (cg::screen cg::*system*)) 
             (cg::stream-string-width stream 
                     (cg::title mnu)))) 
         (+ 20 (cg::with-device-context (hdc (cg::screen cg::*system*))
               (cg::stream-string-width stream 
                     (cg::title  mnu))))
         ))

(defun total-menu-title-width (mnb &optional
                                (stream *quail-menubar-window*))
     "Find the sum of the stream-string-widths of the titles of the menus
       currently on mnb wrt stream (default *quail-menubar-window*)."
     ;; 20 is a factor which seems adequate.
   (let ((s 0))
      (loop for mnu in (cg::menu-items mnb)
        do
        (setq s (+ s (cg::with-device-context (hdc (cg::screen cg::*system*)) 
                    (cg::stream-string-width 
                      (cg::screen cg::*system*)  
                      (cg::title mnu)) 
                  20))))
      s))

(defun total-menu-width (mnb)
   "Find the sum of the stream-string-widths of the titles of the menus
currently on menubar mnb"
   (let ((s 0))
      (loop for mnu in (cg::menu-items mnb)
        do
        (setq s (+ s 
            (cg::with-device-contex (hdc (cg::screen cg::*system*))
              (cg::stream-string-width (cg::screen cg::*system*)
                      (cg::title mnu)))
             )))
      s))


;; Added 042098 gwb
(defvar *quail-window-base-height* NIL
   "The height (pixels) of just the title strip of *quail-menubar-window*.")

;; Added 042098 gwb
(defun set-quail-window-base-height (&key (height 31))
     "Sets the height (pixels) of just the title strip of *quail-menubar-window*."
     (setf *quail-window-base-height* height))

;; Added 042098 gwb
(defvar *quail-window-min-width* NIL
   "The minimum width (pixels) to hold title plus controls.")

;; Added 042098 gwb
(defun set-quail-window-min-width (&key (width 250))
     "Sets the minimum width (pixels) to hold title plus controls."
     (setf *quail-window-min-width* width))

(defun make-system-default-menubar ()
  "Makes the default quail menubar which is a bitmap-window + menubar~
which cannot be user-scrolled or user-resized.~
Returns the menubar of the window."
  (unless (and *quail-window-min-width* *quail-window-base-height*
               *extra-bar-height*)
    (set-quail-window-min-width)
    (set-quail-window-base-height)
    (set-extra-bar-height))
  (let* (( win (cg::make-window ':qmbwin
                 :device 'cg::bitmap-window :parent (cg::screen cg::*system*) :title "QUAIL MENUBAR"
                 :user-resizable NIL :user-scrollable NIL
                 :window-exterior  (cg::make-box 1 1
                                                 (+ 1 *quail-window-min-width*)
                                                 (+ *quail-window-base-height* *extra-bar-height*))
                 :font (cg::make-font-ex nil :Arial 13 nil)))
         ( mb1 (cg::make-window ':qmb :device 'cg::menu-bar :parent win )))
    (setf *quail-menubar-window* win)
    (cg::menu win) ;(cg::window-menu win) try this 15jun2004
    )
  )

(defun set-system-default-menubar 
      (&optional (menubar (make-system-default-menubar)))
     "Sets the *system-default-menubar* to be the value of the optional 
       argument menubar. Defaults to a the menubar of a 
       non-scrollable, non-resizable bitmap-window."
;     (declare (special cg::*screen*))
     (setf  *system-default-menubar* menubar)
     )

(eval-when (load)
  (set-system-default-menubar)) 

(defvar *last-menubar* *system-default-menubar*
   "The list of elements of the last menubar.  To be updated 
and downdated as canvases are activated and deactivated.") 

(defun spot-on-menubar (mnb mnu)
     "Finds position of mnu on mnb using  titles
        in menu-item-titles of mnb.
        mnu can be a menu or a menu-item."
     (let* ((title (if (cg::menu-item-p mnu) (cg::title mnu)
                              (cg::title mnu)))
                (items (cg::menu-items mnb))
                (menu-titles (mapcar #'cg::title items))
                (spot (position title menu-titles :test #'string-equal)))
          spot))

(defun add-line-to-menubar (win mnb mnu)
     "If necessary increases the height of win by enough to accomodate
       a new line of menus. *extra-bar-height* is that amount"
     (unless *extra-bar-height*
           (set-extra-bar-height))
     (let* ((winbox (cg::exterior win))
                (wintopleft (cg::box-top-left winbox))
                (winwidth (cg::box-width winbox))
                (win-height (cg::box-height winbox))
                (n-full-bars (floor (/ (total-menu-title-width mnb) (screen-width))))
                )
          (when (and (not (spot-on-menubar mnb mnu))
                 (> (+ (total-menu-title-width mnb) (menu-title-width mnu))
                           (* (+ 1 n-full-bars) (screen-width))))
                 (setf (cg::exterior win)
                          (cg::make-box-relative-from-corner wintopleft winwidth
                           (+ win-height *extra-bar-height*)))))
     NIL)

(defun remove-lines-from-menubar (win mnb mnu)
     "If necessary decreases the height of win by enough to accomodate~
       removal of one or more lines of menus. *extra-bar-height* is that amount"
     (declare (ignore mnu)) ;It's already gone
     (unless *quail-window-base-height* (set-quail-window-base-height))
     (let* ((winbox (cg::exterior win))
                (wintopleft (cg::box-top-left winbox))
                (winwidth (cg::box-width winbox))
                (win-height (cg::box-height winbox))
                (n-full-bars (floor (/ (total-menu-title-width mnb) (screen-width))))
                (nbars (floor (/ win-height wb::*extra-bar-height*)))
                )
     (when  (and  ;(> n-full-bars 0)
                           (> nbars 2)
                           (< (total-menu-title-width mnb)
                                (* (+ 1 n-full-bars) (screen-width))))
            (setf (cg::exterior win)
                     (cg::make-box-relative-from-corner wintopleft (+ winwidth 20)
                      (+ *quail-window-base-height* (* (+ 1 n-full-bars) *extra-bar-height*))))
            )
     NIL))


(defun extend-menubar-by-menu (mnb mnu)
   "If necessary extends the window of mnb 
     -  to accomodate addition of mnu"
   (let* ((win (cg::parent mnb))
          (update (+ (total-menu-title-width mnb)
                     ;; may need to update wrt win
                     (+ 20 (menu-title-width mnu))))  
          (wintopleft (cg::exterior-top-left win))
          (winwidth (cg::exterior-width win))
          (winheight (cg::exterior-height win))
          )
      (when  (and (< update (screen-width))
                  (< winwidth   update))
         ;;; It seems that I have to setf the window-interior for I cannot just
         ;;; setf its box-right 
         ;;; 10 is a fudge factor which seems sufficient.
         ;; Following comment from quail-menubar version ... rwo
         ;;; 4 is a fudge factor which seems sufficient.
         (setf (cg::exterior win) 
               (cg::make-box-relative-from-corner 
                wintopleft (+ update 10) winheight))
         )
      NIL))

(defun contract-menubar-by-menu (mnb mnu)
   "If necessary contracts  the window of mnb  to accomodate removal of mnu
    which has gone anyway so ignore it"
   (declare (ignore mnu))
   (let* ((win (cg::parent mnb))
          (update (total-menu-title-width mnb ))
          (wintopleft (cg::exterior-top-left win))
          (winwidth (cg::exterior-width win))
          (winheight (cg::exterior-height win))
          )
      (when  (> winwidth update)
         ;;; It seems that I have to setf the window-interior for I cannot just
         ;;; setf its box-right . Again 20 (10?)
         ;; is a fudge factor which seems to work.
         (setf (cg::exterior win)  
               (cg::make-box-relative-from-corner  
                wintopleft (max (+ update 20 )
                             *quail-window-min-width*) winheight))
         )
      NIL ))

;;; New version after change to single QMB
(defun put-title-menus-on-menubar (canvas)
   "title-menubar seems to be a list of things"
   (declare
     (special *current-canvas*
       *system-default-menubar*
       *last-menubar*))
   (setf *current-canvas* canvas)
   (let ((title-menus (title-menus-of canvas))
         (title-menubar *system-default-menubar*))
      (when
          title-menus
         (setf title-menus
               (sort (loop for m in title-menus collect m)
                 #'(lambda (k1 k2)
                     (string< (string k1) (string k2)))
                 :key #'car))
         (setf title-menubar
               (loop for (title . m) in title-menus
                 do 
                 (unless (spot-on-menubar title-menubar m)
                    (extend-menubar-by-menu title-menubar m)
                    (cg::add-to-menu title-menubar 
                     (make-instance 'cg::menu-item :title (cg::title m)
                      :value m)))
                 ))
         )
      (setf *last-menubar* title-menubar)
      )
   )
;;; end new version.
(defun add-menu-in-menubar (menu &optional
                            (menubar *system-default-menubar*))
   "Adds the menu to a menubar. ~
   if it is not already there. Adjusts the width of the menubar if necessary.
   menu may be a menu or a menu-item
   (:see-also remove-menu-from-menubar)"
   (declare (special *system-default-menubar*))
   (unless (spot-on-menubar menubar menu)
      (add-line-to-menubar (cg::parent menubar) menubar menu) 
      (extend-menubar-by-menu menubar menu)
      (cg::add-to-menu menubar 
       (if (cg::menu-item-p menu)
          menu
          (make-instance 'cg::menu-item :title (cg::title menu)
                              :value menu)))))


(defun remove-menu-from-menubar (menu &optional (menubar
                                                  *system-default-menubar*))
   "Removes the menu from a menubar. ~
     if it is there. Adjusts the width of the menubar after removal.
   (:see-also install-quail-menubar add-menu-in-menubar)"
   (let* ((items (cg::menu-items menubar))
          (menu-titles (mapcar #'cg::title items))
          (title (cg::title menu))
          (spot (position title menu-titles :test #'string-equal)))
      (when spot (loop for i from spot to (1- (length items))
                   do
                   (cg::remove-from-menu menubar (elt items i))
                   )) ;; strip everything off from spot to the end - OK
      (remove-lines-from-menubar (cg::parent menubar)
       menubar menu)             
      (contract-menubar-by-menu menubar menu)
      (when spot (loop for i from (1+ spot) to (1- (length items))
                   do
                   (add-menu-in-menubar (elt items i) menubar)))
      ;;(release-menu-space menu)
      ))

(defmethod initialize-instance :after ((self host-menu-canvas)
                                                           &rest initargs)
     (declare (ignore initargs))
     (if (title-menus-of self)
        (put-title-menus-on-menubar self)))

;;; Added 19 June 1998. See closing-hmc.lsp
(defmethod cg::user-close ((w host-menu-canvas))
   (declare (special *current-canvas*))
   (let ((tms (title-menus-of w))
         (result (call-next-method)))
      (when result
         (loop for (title . tm) in tms
           do
           (wb::remove-menu-from-menubar tm)
           (wb::release-menu-space tm)
           )
         (setf *current-canvas* (canvas-at-top))
         (if *current-canvas*
            (put-title-menus-on-menubar *current-canvas*)))
      result))

#+:aclpc-linux (setf (cg::height wb::*quail-menubar-window*) 51)

