;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                 Known bugs and workarounds for the Mac                            
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1995 Statistical Computing Laboratory,
;;;                    University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1995.
;;;
;;;
;;;--------------------------------------------------------------------------------
;;; 
;;;

(in-package :quail-user)

;;;   CODE FOR ALL FIXES MAY BE FOUND IN "q:comments;fixes-mcl.lsp"
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Suddenly inactive menubars
;;;
;;;   Occasionally some or all of the menubar becomes inactive.
;;;   We have not been able to determine the cause of this, but offer
;;;   the following fix.
;;;
;;;   Evaluate the form (re-enable-menubars)
;;;
;;;   This evaluation can be programmed to be executed when you press
;;;   either
;;;
;;;   ... the f12 function key (if you have one), as in
;;;       (ccl::def-fred-command (:function #\c) re-enable-menubars)
;;;
;;;   or
;;;
;;;   ... CTRL-META-M (the control key + the option key + the m key)
;;;      (ccl::def-fred-command (:control :meta #\m) re-enable-menubars)
;;;
;;;   You may want to put either form (or both forms) in your initialization
;;;   file quail-init.lsp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Disappearing menubar  (never actually seen this one, but ... rwo)
;;;
;;;  Should the menubar ever disappear, it can be reconstructed by
;;;  evaluating (q::install-default-quail-menubar)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   MIXED UP MENUS
;;;
;;;  For some bizarre reason, after prolonged use, MCL can start
;;;  messing up all the pointers to the contents of their menus.
;;;  
;;;  I think that the reason this happens is that the heap MCL uses
;;;  for menus gets confused after awhile.
;;;  
;;;  So to release all of the space we use for Quail's pop-up menus, evaluate
;;;
;;;    (restart-menus)
;;;
;;;  This removes all menus and releases their space.  This just gives you
;;;  a clean slate.  When you next need a pop-up menu it will be built from
;;;  scratch and cached as before.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
