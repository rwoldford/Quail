;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;    key-event-sblx.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A preliminary version to capture what is
;;;  available in aclwin
;;;
;;;  G.W.Bennett 2020
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export 
        '(;*backspace-event* *space-event* *tab-event* *linefeed-event* *page-event*
          ;*return-key-event* *delete-event* *enter-key-event* *escape-key-event*
          ;*home-key-event* *up-arrow-event* *down-arrow-event*
          ;*back-arrow-event* *forward-arrow-event* *rubout-event*
          +backspace-event-event+ +space-event+ +tab-event+ +linefeed-event+ +page-event+
          +return-key-event+ +delete-event+ +enter-key-event+ +escape-key-event+
          +home-key-event+ +up-arrow-event+ +down-arrow-event+
          +back-arrow-event+ +forward-arrow-event+ +rubout-event+
          )
        ))

;(defmethod cg::character-message ((pane host-pane) buttons data )
;   (declare (ignore buttons))
;   (let ((canvas (cg::parent pane)))
;      (wb::handle-key-event canvas data)
;      ))

;;; The Semi-standard characters of CL

(defconstant +backspace-event+ #\backspace ;*backspace-event* #\backspace 23FEB2022 gwb
 "Captures the backspace keyboard event")

(defconstant +space-event+ #\space ;*space-event* #\space 23FEB2022 gwb
 "Captures the space keyboard event")

(defconstant +tab-event+ #\tab ;*tab-event* #\tab 23FEB2022 gwb
 "Captures the tab keyboard event")

(defconstant +linefeed-event+ #\return ;*linefeed-event*  #\Newline ;#\return 23FEB2022 gwb
  "Captures the linefeed keyboard event")
 
(defconstant +page-event+ #\page ;*page-event* #\page ;cg::vk-pagedown 23FEB2022 gwb
  "Captures the page keyboard event")
 
(defconstant +return-key-event+ #\Return ;*return-key-event*  #\Return ;#\newline 23FEB2022 gwb
 "Captures the return keyboard event")
 
(defconstant +rubout-event+ #\Rubout ;*rubout-event* #\Rubout 23FEB2022 gwb
  "Captures the rubout keyboard event")
 
;;; Some non-standard and hence non-portable characters.

(defconstant +delete-event+ #\backspace ;*delete-event*  #\backspace 23FEB2022 gwb
 "Captures the delete keyboard event")
 
(defconstant +enter-key-event+ #\esc ;*enter-key-event* #\esc 23FEB2022 gwb
 "Captures the enter keyboard event")
 
(defconstant +escape-key-event+ #\Esc ;*escape-key-event* #\Esc 23FEB2022 gwb
 "Captures the escape key keyboard event")
 
(defconstant +home-key-event+ :home ;*home-key-event* :home  23FEB2022 gwb
 "Captures the home keyboard event")
 
;;; The (again non-standard) arrow movement keys.

(defconstant +up-arrow-event+ :up ;*up-arrow-event* :up  23FEB2022 gwb
 "Captures the up arrow keyboard event")
 
(defconstant +down-arrow-event+ :down ;*down-arrow-event* :down 23FEB2022 gwb
 "Captures the down arrow keyboard event")
 
(defconstant +back-arrow-event+ :left ;*back-arrow-event* :left 23FEB2022 gwb
 "Captures the back arrow keyboard event")
 
(defconstant +forward-arrow-event+ :right ;*forward-arrow-event* :right 23FEB2022 gwb
 "Captures the forward arrow keyboard event")
 
;;; And not some which Unix and Apple may not have
;;; One of the pageup / pagedown may correpsond to page
;;; The rest remain to be checked

(defconstant +end-event+ :end ;*end-event* :end 23FEB2022 gwb
 "Captures the end keyboard event")
 
(defconstant +pageup-event+ :prior ;*pageup-event* :prior  23FEB2022 gwb
 "Captures the page up keyboard event")
 
(defconstant +pagedown-event+ :next ;*pagedown-event* :next 23FEB2022 gwb
 "Captures the pagedown keyboard event")
 
(defconstant +insert-event+ :insert ;*insert-event* :insert 23FEB2022 gwb
 "Captures the insert keyboard event")
 
(defconstant +cancel-event+ #\Can ;*cancel-event* #\Can  23FEB2022 gwb
 "Captures the cancel keyboard event")


;(defconstant *backtab-event* cg::vk-backtab 
; "Captures the backtab keyboard event")

#|
;;; (in-package :clim-user) and therefore (in-package :wb)
;;; output from key presses:
delete
event-type is :KEY-PRESS 
character associated with k-event is #\Rubout 
keyboard-event-key-name for k-event is :DELETE 
home
event-type is :KEY-PRESS 
character associated with k-event is NIL 
keyboard-event-key-name for k-event is :HOME 
enter
event-type is :KEY-PRESS 
character associated with k-event is #\Return 
keyboard-event-key-name for k-event is :RETURN 
escape
event-type is :KEY-PRESS 
character associated with k-event is NIL 
keyboard-event-key-name for k-event is :ESCAPE 
up-arrow
event-type is :KEY-PRESS 
character associated with k-event is NIL 
keyboard-event-key-name for k-event is :UP 
down-arrow
event-type is :KEY-PRESS 
character associated with k-event is NIL 
keyboard-event-key-name for k-event is :DOWN 
back-arrow
event-type is :KEY-PRESS 
character associated with k-event is NIL 
keyboard-event-key-name for k-event is :LEFT 
forward-arrow
event-type is :KEY-PRESS 
character associated with k-event is NIL 
keyboard-event-key-name for k-event is :RIGHT 
end
event-type is :KEY-PRESS 
character associated with k-event is NIL 
keyboard-event-key-name for k-event is :END 
pageup
event-type is :KEY-PRESS 
character associated with k-event is NIL 
keyboard-event-key-name for k-event is :PRIOR 
pagedown
event-type is :KEY-PRESS 
character associated with k-event is NIL 
keyboard-event-key-name for k-event is :NEXT 
insert
event-type is :KEY-PRESS 
character associated with k-event is NIL 
keyboard-event-key-name for k-event is :INSERT 
cancel
NO KEY
backtab
NO KEY
backspace
event-type is :KEY-PRESS 
character associated with k-event is #\Backspace 
keyboard-event-key-name for k-event is :BACKSPACE 
space
event-type is :KEY-PRESS 
character associated with k-event is #\  
keyboard-event-key-name for k-event is :| | 
tab
event-type is :KEY-PRESS 
character associated with k-event is #\Tab 
keyboard-event-key-name for k-event is :TAB 
linefeed
NO KEY
page
NO KEY
return
event-type is :KEY-PRESS 
character associated with k-event is #\Return 
keyboard-event-key-name for k-event is :RETURN 
rubout (presumably = delete)
event-type is :KEY-PRESS 
character associated with k-event is #\Rubout 
keyboard-event-key-name for k-event is :DELETE 
|#