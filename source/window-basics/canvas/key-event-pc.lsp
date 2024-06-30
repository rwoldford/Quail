;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;    key-event-pc.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A preliminary version to capture what is
;;;  available in aclwin
;;;
;;;  G.W.Bennett 1997
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export 
         '(;*backspace-event* *tab-event* *linefeed-event* *page-event*
          ;*return-key-event* *delete-event* *enter-key-event* *escape-key-event*
          ;*home-key-event* *up-arrow-event* *down-arrow-event*
          ;*back-arrow-event* *forward-arrow-event* *rubout-event*
          +backspace-event+ +tab-event+ +linefeed-event+ +page-event+ 
          +return-key-event+ +delete-event+ +enter-key-event+ +escape-key-event+
          +home-key-event+ +up-arrow-event+ +down-arrow-event+
          +back-arrow-event+ +forward-arrow-event+ +rubout-event+ 
          )
         ))

(defmethod cg::character-message ((pane host-pane) buttons data )
   (declare (ignore buttons))
   (let ((canvas (cg::parent pane)))
      (wb::handle-key-event canvas data)
      ))

;;; The Semi-standard characters of CL

(defconstant +backspace-event+ #\backspace ;*backspace-event* #\backspace 23FEB2022 gwb
 "Captures the backspace keyboard event")

(defconstant +space-event+ #\space ;*space-event* #\space 23FEB2022 gwb
 "Captures the space keyboard event")

(defconstant +tab-event+ #\tab ;*tab-event* #\tab 23FEB2022 gwb
 "Captures the tab keyboard event")

(defconstant +linefeed-event+ #\return ;*linefeed-event*  #\return 23FEB2022 gwb
  "Captures the linefeed keyboard event")

(defconstant +page-event+ cg::vk-pagedown ;*page-event* cg::vk-pagedown 23FEB2022 gwb
  "Captures the page keyboard event")

(defconstant +return-event+ #\newline ;*return-key-event*  #\newline 23FEB2022 gwb
 "Captures the return keyboard event")

(defconstant +rubout-event+ #\rubout ;*rubout-event* #\rubout 23FEB2022 gwb
  "Captures the rubout keyboard event")

;;; Some non-standard and hence non-portable characters.

(defconstant +delete-event+ #\backspace ;*delete-event*  #\backspace 23FEB2022 gwb
 "Captures the delete keyboard event")

(defconstant +enter-key-event+ #\esc ;*enter-key-event* #\esc 23FEB2022 gwb
 "Captures the enter keyboard event")

(defconstant +escape-key-event+ #\esc ;*escape-key-event* #\esc 23FEB2022 gwb
 "Captures the escape key keyboard event")

(defconstant +home-key-event+ cg::vk-home ;*home-key-event* cg::vk-home 23FEB2022 gwb 
 "Captures the home keyboard event")

;;; The (again non-standard) arrow movement keys.

(defconstant +up-arrow-event+ cg::vk-up ;*up-arrow-event* cg::vk-up 23FEB2022 gwb
 "Captures the up arrow keyboard event")

(defconstant +down-arrow-event+ cg::vk-down ;*down-arrow-event* cg::vk-down 23FEB2022 gwb
 "Captures the down arrow keyboard event")

(defconstant +back-arrow-event+ cg::vk-left ;*back-arrow-event* cg::vk-left 23FEB2022 gwb
 "Captures the back arrow keyboard event")

(defconstant +forward-arrow-event+ cg::vk-right ;*forward-arrow-event* cg::vk-right 23FEB2022 gwb
 "Captures the forward arrow keyboard event")

;;; And not some which Unix and Apple may not have
;;; One of the pageup / pagedown may correpsond to page
;;; The rest remain to be checked

(defconstant +end-event+ cg::vk-end ;*end-event* cg::vk-end 23FEB2022 gwb
 "Captures the end keyboard event")

(defconstant +pageup-event+ cg::vk-pageup ;*pageup-event* cg::vk-pageup  23FEB2022 gwb
 "Captures the page up keyboard event")

(defconstant +pagedown-event+ cg::vk-pagedown ;*pagedown-event* cg::vk-pagedown  23FEB2022 gwb
 "Captures the pagedown keyboard event")

(defconstant +insert-event+ cg::vk-insert ;*insert-event* cg::vk-insert 23FEB2022 gwb
 "Captures the insert keyboard event")

(defconstant +cancel-event+ cg::vk-cancel ;*cancel-event* cg::vk-cancel  23FEB2022 gwb
 "Captures the cancel keyboard event")

(defconstant +backtab-event+ cg::vk-backtab ;*backtab-event* cg::vk-backtab  23FEB2022 gwb
 "Captures the backtab keyboard event")