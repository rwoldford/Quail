;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;                         dialog-items-mcl.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  Authors:
;;;     R.W. Oldford 1994
;;;
;;;
;;;


(in-package :wb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;     dialog items in ccl are used.
;;;

;; this *all-menus* parameter is used by the re-enable-menubars function to work
;; around a bug on the Mac which occasionally leaves menus disabled after errors
(defparameter *all-menus*
  (list ccl:*apple-menu*
        ccl:*edit-menu*
        ccl:*eval-menu*
        ccl:*file-menu*
        ccl:*tools-menu*
        ccl:*windows-menu*))

;; this re-enable-menubars function uses the *all-menus* parameter to work
;; around a bug on the Mac which occasionally leaves menus disabled after errors.
;; on other systems, this should probably be empty.

(defun re-enable-menubars (&rest whatever)
  (declare (ignore whatever))
  (loop for m in (union *all-menus*
                        (union (ccl::menubar)
                               (loop for vw in (canvases)
                                     append (loop for (key . m) in
                                                  (slot-value vw 'title-menus)
                                                  ;; using key here just suppresses
                                                  ;; a warning message
                                                  collect (progn key m))) :test #'eq)
                        :test #'eq)
        do (ccl::menu-enable m)))
