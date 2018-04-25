;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;    key-event-pc.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A preliminary version to capture what is
;;;  available in aclwin
;;;
;;;  G.W.Bennett 1997
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(*backspace-event* *tab-event* *linefeed-event* *page-event*
          *return-key-event* *delete-event* *enter-key-event* *escape-key-event*
          *home-key-event* *up-arrow-event* *down-arrow-event*
          *back-arrow-event* *forward-arrow-event* *rubout-event*)))

(defmethod cg::character-message ((pane host-pane) buttons data )
   (declare (ignore buttons))
   (let ((canvas (cg::parent pane)))
      (wb::handle-key-event canvas data)
      ))

;;; The Semi-standard characters of CL

(defconstant *backspace-event* #\backspace
 "Captures the backspace keyboard event")

(defconstant *space-event* #\space
 "Captures the space keyboard event")

(defconstant *tab-event* #\tab
 "Captures the tab keyboard event")

(defconstant *linefeed-event*  #\return
  "Captures the linefeed keyboard event")

(defconstant *page-event* cg::vk-pagedown
  "Captures the page keyboard event")

(defconstant *return-key-event*  #\newline
 "Captures the return keyboard event")

(defconstant *rubout-event* #\rubout
  "Captures the rubout keyboard event")

;;; Some non-standard and hence non-portable characters.

(defconstant *delete-event*  #\backspace
 "Captures the delete keyboard event")

(defconstant *enter-key-event* #\esc
 "Captures the enter keyboard event")

(defconstant *escape-key-event* #\esc
 "Captures the escape key keyboard event")

(defconstant *home-key-event* cg::vk-home 
 "Captures the home keyboard event")

;;; The (again non-standard) arrow movement keys.

(defconstant *up-arrow-event* cg::vk-up 
 "Captures the up arrow keyboard event")

(defconstant *down-arrow-event* cg::vk-down
 "Captures the down arrow keyboard event")

(defconstant *back-arrow-event* cg::vk-left
 "Captures the back arrow keyboard event")

(defconstant *forward-arrow-event* cg::vk-right
 "Captures the forward arrow keyboard event")

;;; And not some which Unix and Apple may not have
;;; One of the pageup / pagedown may correpsond to page
;;; The rest remain to be checked

(defconstant *end-event* cg::vk-end
 "Captures the end keyboard event")

(defconstant *pageup-event* cg::vk-pageup 
 "Captures the page up keyboard event")

(defconstant *pagedown-event* cg::vk-pagedown 
 "Captures the pagedown keyboard event")

(defconstant *insert-event* cg::vk-insert
 "Captures the insert keyboard event")

(defconstant *cancel-event* cg::vk-cancel 
 "Captures the cancel keyboard event")

(defconstant *backtab-event* cg::vk-backtab 
 "Captures the backtab keyboard event")
