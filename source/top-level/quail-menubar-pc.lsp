;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          quail-menubar-pc.lsp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1991.
;;;     G.W. Bennett 1997.
;;;
;;; was new-qmb-pc.lsp but became qmb-pc.lsp May 19 1998
;;; to test the building of Quail menubar ... gwb 051998
;;;--------------------------------------------------------------------------------
;;; See also Q/Q/S/Top-Level/quail-menubar-pc-log.lsp
(in-package :quail)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(*quail-menubar* add-menu-in-quail-menubar remove-menu-from-quail-menubar
          install-quail-menubar
          )))
#| Replaced March 25 1998
;; Nov 11+ 1997 redefined *q-mnb* to be cg::*lisp-menu-bar*
(defvar *quail-menubar*
  (if (typep wb::*system-default-menubar* 'cg::closed-stream)
     (wb::set-system-default-menubar) wb::*system-default-menubar*)
  "The default menubar put up for the Quail system.~
   This is to be the main menubar for ACL.")
|#
(defvar *quail-menubar* NIL
  "The default menubar put up for the Quail system.")
#|   06JUL2023   not used in this file!
(defvar *system-default-menubar-items*
  (cg::menu-items wb::*system-default-menubar*)
  "The default menubar items of the system.")
  |#
;; Added 042098 gwb
(defun add-menu-in-quail-menubar (menu)
   "Adds the menu to Quail's menubar. ~
   if it is not already there. Adjusts the width of the menubar if necessary.~
   menu may be a menu or a menu-item
   (:see-also install-quail-menubar remove-menu-from-quail-menubar)"
   (declare (special *quail-menubar* wb::*system-default-menubar*))
;;   (if (typep *quail-menubar* 'cg::closed-stream)
;;      (setf *quail-menubar* wb::*system-default-menubar*))
     (cond
      (*quail-menubar*
        (wb::add-menu-in-menubar menu *quail-menubar*)
        )
      (T
        (install-quail-menubar)
        (add-menu-in-quail-menubar menu))
      ))
(defun remove-menu-from-quail-menubar (menu)
   "Removes the menu from Quail's menubar. ~
    if it is there. Adjusts the width of the menubar after removal.
    (:see-also install-quail-menubar add-menu-in-quail-menubar)"
   (declare (special *quail-menubar*))
   (wb::remove-menu-from-menubar menu *quail-menubar*)
   (unless (cg::menu-items *quail-menubar*)
      (if (quail-yes-or-no-p
            "~&You have just stripped the menubar bare!! ~%Is that OK?")
         (warn "To get the default one back type ~%(install-default-quail-menubar). ")
         (install-quail-menubar)))
 )
;; Nov 11+ changed wb::*sy-def-mnb-its* to *sy-def-mnb-its*
;; since that variable is defined at the top of this file!
(defun install-default-quail-menubar ()
  "Creates and installs the default top level Quail menubar."
  (declare (special *quail-menubar* wb::*system-default-menubar*))
     #|
  (when (and *quail-menubar* (cg::streamp *quail-menubar*))
    (cg::close *quail-menubar*)
    (cg::close (cg::parent *quail-menubar*)))
    |#
  (when (or (null wb::*system-default-menubar*)
            ;;             (typep wb::*system-default-menubar* 'cg::closed-stream)
            (not (cg::open-stream-p (cg::parent wb::*system-default-menubar*)))
            )
    (wb::set-system-default-menubar))
  (setf *quail-menubar* wb::*system-default-menubar*)
  (add-menu-in-quail-menubar (quail-menu))
  (add-menu-in-quail-menubar (quail-plot-menu))
  ) 

(defun install-quail-menubar ()
  "Installs the top-level Quail's menubar according to the ~
   value of *quail-menubar*."
   ;; Carry over MCL implementation where this would have to set the
   ;; menubar on top of the Mac menubar.  Pay no attention to the
   ;; man behind the curtain!
    (install-default-quail-menubar))

(eval-when (:load-toplevel :execute)
  (add-restore-lisp-functions
    #'wb::set-system-default-menubar
    #'q::install-default-quail-menubar))
;;;
;;;  Spot to get rid of pop-up menus when a view-window is closed
;;;
 (excl::without-package-locks
   (defmethod cg::device-close :around ((w vw::view-window) abort)
   (declare (ignore abort))
   (let ((vps-vws (vw::viewports-and-views-of w)) ;(viewports-and-views-of w)) ; 30JUL2030
         vws
         (result (call-next-method)))
      (when vps-vws
         (setf vws (loop for vp-vw in vps-vws collect (cdr vp-vw)))
         (labels
           ((doit (a-view)
             (loop for m in '(vw::left-menu
                              vw::middle-menu
                              vw::right-menu
                              vw::ctrl-left-menu
                              vw::ctrl-middle-menu
                              vw::ctrl-right-menu 
                              )
               when 
               (and (slot-exists-p a-view m)
                    (wb::menu-p (slot-value a-view m)))
               do
               (wb::release-menu-space (slot-value a-view m))
               (setf (slot-value a-view m) NIL))
             (when (slot-exists-p a-view 'vw::subviews)
                (loop for sv in (vw::subviews-of a-view) ;(subviews-of a-view) ; 30JUL2023
                  do (doit sv))))
            )
           (loop for vw in vws do (doit vw)))
         )
      result)
   )
)
