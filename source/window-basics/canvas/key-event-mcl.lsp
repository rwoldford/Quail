;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               key-event-mcl.lisp
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
;;;     R.W. Oldford 1996
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export
 '(;*backspace-event* *tab-event* *linefeed-event* *page-event* *return-key-event*
   ;*rubout-event* *delete-event* *enter-key-event* *escape-key-event*
   ;*home-key-event*
   ;*up-arrow-event* *down-arrow-event* *back-arrow-event* *forward-arrow-event*
   +backspace-event+ +tab-event+ +linefeed-event+ +page-event+ +return-key-event+
   +rubout-event+ +delete-event+ +enter-key-event+ +escape-key-event+
   +home-key-event+
   +up-arrow-event+ +down-arrow-event+ +back-arrow-event+ +forward-arrow-event+  
   )
 ))


(defmethod ccl::view-key-event-handler ((canvas canvas) event)
  "Hands off to the window-basics key-event handler."
  (handle-key-event canvas event)
  )

;;; The Semi-standard characters of CL


(defconstant +backspace-event+ #\Backspace ;*backspace-event* #\Backspace 23FEB2022 gwb
  "The value of the backspace keyboard event.")

(defconstant  +tab-event+ #\Tab ;*tab-event* #\Tab 23FEB2022 gwb
  "The value of the tab keyboard event.")

(defconstant +linefeed-event+ \Linefeed;*linefeed-event* #\Linefeed 23FEB2022 gwb
  "The value of the linefeed keyboard event.")

(defconstant  +page-event+ #\Page ;*page-event* #\Page 23FEB2022 gwb
  "The value of the Page keyboard event.")

(defconstant  +return-key-event+ #\Return ;*return-key-event* #\Return 23FEB2022 gwb
  "The value of the return keyboard event.")

(defconstant  +rubout-event+ #\Rubout ;*rubout-event* #\Rubout 23FEB2022 gwb
  "The value of the rubout keyboard event.")


;;;  Some non-standard and hence non-portable characters.

(defconstant  +delete-event+ #\Delete ;*delete-event* #\Delete 23FEB2022 gwb
  "The value of the delete keyboard event.")

(defconstant  +enter-key-event+ #\Enter ;*enter-key-event* #\Enter 23FEB2022 gwb
  "The value of the enter keyboard event.")

(defconstant  +escape-key-event+ #\ESC ;*escape-key-event* #\ESC 23FEB2022 gwb
  "The value of the escape key keyboard event.")

(defconstant  +home-key-event+ #\Home ;*home-key-event* #\Home 23FEB2022 gwb
  "The value of the Home key keyboard event.")

;;; The (again non-standard) arrow movement keys.

(defconstant +up-arrow-event+ #\UpArrow ;*up-arrow-event* #\UpArrow 23FEB2022 gwb
  "The value of the up arrow keyboard event.")

(defconstant +down-arrow-event+ #\DownArrow ;*down-arrow-event* #\DownArrow 23FEB2022 gwb
  "The value of the down arrow keyboard event.")

(defconstant +back-arrow-event+ #\BackArrow ;*back-arrow-event* #\BackArrow 23FEB2022 gwb
  "The value of the left or back arrow keyboard event.")

(defconstant +forward-arrow-event+ #\ForwardArrow ;*forward-arrow-event* #\ForwardArrow 23FEB2022 gwb
  "The value of the right or forward arrow keyboard event.")


