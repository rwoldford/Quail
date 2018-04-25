;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          quail-menubar-mcl.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1991.
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(*quail-menubar* add-menu-in-quail-menubar remove-menu-from-quail-menubar
          install-quail-menubar
          )))

(defvar *quail-menubar*
  NIL
  "The default menubar put up for the Quail system.")

(defun add-menu-in-quail-menubar (menu)
  "Adds the menu to Quail's menubar. ~
   (:see-also install-quail-menubar remove-menu-from-quail-menubar)"
  (declare (special *quail-menubar*))
  (if *quail-menubar*
    (unless (member menu *quail-menubar*)
      (setf *quail-menubar* 
            (append *quail-menubar* (list menu))))
    (setf *quail-menubar* (list menu)))
  (install-quail-menubar))

(defun remove-menu-from-quail-menubar (menu)
  "Removes the menu from Quail's menubar. ~
   (:see-also install-quail-menubar add-menu-in-quail-menubar)"
  (declare (special *quail-menubar*))
  (setf *quail-menubar* (remove menu *quail-menubar*))
  (wb::release-menu-space menu)
  (if *quail-menubar*
    (install-quail-menubar)
    (progn
      (warn "~&You have just stripped the menubar bare! ~%~
             To get the default one back type ~%~
             (install-default-quail-menubar)")
      (ccl::set-menubar wb::*system-default-menubar*))))

(defvar *quail-system-menus*
  NIL
  "The system menus peculiar to Quail which are to appear on the menubar ~
   at all times")

(defun install-default-quail-menubar ()
  "Creates and installs the default top level Quail menubar."
  (declare (special *quail-menubar*))
  (cond
   (*quail-system-menus*
    (loop for m in *quail-system-menus*
          when (not (member m *quail-menubar*))
          do
          (setf *quail-menubar*
                (append  wb::*system-default-menubar*
                         (list m)))))
   (T
    (setf *quail-system-menus*
          (list (quail-menu)
                (quail-plot-menu)))
    
    (setf *quail-menubar*
          (remove-duplicates (append  wb::*system-default-menubar*
                                      *quail-system-menus*)))
    (setf wb::*system-default-menubar*
          *quail-menubar*)))
  (install-quail-menubar))
  
        

(defun install-quail-menubar ()
  "Installs the top-level Quail's menubar according to the ~
   value of *quail-menubar*."
  (declare (special *quail-menubar*))
  (if  *quail-menubar*
    (ccl::set-menubar *quail-menubar*)
    (install-default-quail-menubar)))

  


  
