;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                           fixes-mcl.lsp                            
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1995 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1995.
;;;
;;;
;;;--------------------------------------------------------------------------------
;;; 
;;;
;;;  This file sets up some MCL specific fixes.
;;;

(in-package :quail-user)

(export '(re-enable-menubars))

(defvar *all-menus*
  (list ccl:*apple-menu*
        ccl:*edit-menu*
        ccl:*eval-menu*
        ccl:*file-menu*
        ccl:*tools-menu*
        ccl:*windows-menu*
        (q::quail-menu)
        (q::quail-plot-menu)))
        
(defun re-enable-menubars (&rest whatever)
  "Re-enables the menubars in case they have been ~
   mysteriously rendered inactive."
  (declare (ignore whatever))
  (loop
    for m in
    (union
     *all-menus*
     (union
      (ccl::menubar)
      (loop for vw in (ccl::windows :class 'view-window)
            append
            (loop for (key . m) in (slot-value vw 'wb::title-menus)
                  ;; using key here just suppresses a warning message
                  collect (progn key m))) 
      :test #'eq)
     :test #'eq)
    do (ccl::menu-enable m)))


(defun restart-menus ()
  "Removes all Quail menus cached on any view and on the menubar ~
   and releases the space."
  (loop
    for menu in
    (set-difference *quail-menubar* wb::*system-default-menubar*)
    do
    (wb::release-menu-space menu)
    finally
    (ccl::set-menubar wb::*system-default-menubar*))
  (labels
    ((doit (&optional class)
       (unless (class-p class)
         (setf class (find-class class)))
       (let ((an-instance (allocate-instance class))
             (sub-classes (class-direct-subclasses class))
             )
         (loop for m in '(vw::left-menu
                          vw::middle-menu
                          vw::right-menu
                          vw::ctrl-left-menu
                          vw::ctrl-middle-menu
                          vw::ctrl-right-menu 
                          )
               when 
               (and (slot-exists-p an-instance m)
                    (wb::menu-p (slot-value an-instance m)))
               do
               (wb::release-menu-space (slot-value an-instance m))
               (setf (slot-value an-instance m) NIL))
         (when sub-classes
           (loop for sub-class in sub-classes
                 do (doit sub-class))))
       ))
    (doit (find-class 'vw::button-mixin)))
  (q::install-default-quail-menubar))
                 
           
           
           

