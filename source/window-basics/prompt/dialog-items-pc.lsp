;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                         dialog-items-pc.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  Authors:
;;;     R.W. Oldford 1994
;;;     G.W. Bennett 1996, 2023
(in-package :wb)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     dialog items in ACL-PC are used.
;;; But the following form is called in canvas-to-ps.lsp
;;; ACL doesn't need to re-enable the menubars.
(defun re-enable-menubars (&rest whatever)
  (declare (ignore whatever)))

;;; Many characteristics of Quail's acl dialogs are common to all of them, for example:
;;;  dialog-outer-left - the position of the left margin of the dialog box on the screen
;;;  dialog-outer-top  - the position of the top of the dialog box on the screen
;;; They are collected here
;;; Others depend on the structure of the individual dialog
;;;  prompt-text-width - depends of the particular prompt
;;; They are defined in the appropriate file
(defparameter *dialog-outer-left* 10) ;; left of window-exterior of whole dialog on the screen
(defparameter *dialog-outer-top* 10) ;; top of window-exterior of whole dialog on the screen
(defparameter *scroll-bar-width* 25) ;; a guess at this
(defparameter *min-separator* 15) ;; distance between widgets (H and V)
(defparameter *displayed* 5) ;; Number of items visible
(defparameter *temp-font-info* 
  (let ((scrn (cg::screen cg::*system*))
        (a-font (cg::make-font :roman "times\ new\ roman" 20);(wb::canvas-font-to-host-font wb::*prompt-normal-font*)
                ))
    (cg::with-font (scrn a-font)
                   (cg::fontmetrics scrn)))) ;; the various characteristics of *prompt-normal-font* .. ascent, descent, etc..
(defparameter *text-height* 
              (+ (cg::font-ascent *temp-font-info*) (cg::font-descent *temp-font-info*) 
                 (cg::font-leading *temp-font-info*)))
(defparameter *text-box-height* (+ 10 *text-height*))
(defparameter *button-height* (+ 2 *text-height*))
(defparameter *select-text* "select")
(defparameter *cancel-text* "cancel") 
(defparameter *ok-text* "OK") 



;;; new auxiliary form to get string-width
(defun quail-string-width (an-input)
  "Returns string width for *prompt-normal-font* on screen"
  (let ((scrn (cg::screen cg::*system*))
        (coming (if (stringp an-input) an-input
                    (format NIL "~s" an-input))))
    (cg::with-font (scrn (cg::make-font :roman "times\ new\ roman" 20);(canvas-font-to-host-font *prompt-normal-font*)
                         )
                   (cg::stream-string-width scrn coming))
    ))