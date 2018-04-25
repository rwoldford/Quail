;;;  This is alternate-mouse-down-p.lsp
;;; holding the original from Q/S/W-B/Mouse/mouse-pc.lsp
;;; which seems to cause a 'freeze' in the form of an infinite loop
;;; since the call  (cg::key-is-down-p cg::left-mouse-button) [and its relatives]
;;; returns the same as (values): absolutely nothing!

;;; and a possible alternative which came from
;;; (1) checking   mouse   in ACL's permuted index, and
;;; (2) following up on these virtual buttons as mentioned 
;;;       on the page for key-is-down-p
;;; from which I followed to key-names where there are the trio vk-[l,m,r]button.

;;; Here is the original (in-package :wb), of course
(defun mouse-down-p ()
     (or (cg::key-is-down-p cg::left-mouse-button)
          (cg::key-is-down-p cg::middle-mouse-button)
         (cg::key-is-down-p cg::right-mouse-button)))

;;; Here is the alternate
(defun mouse-down-p ()
     (or (cg::key-is-down-p cg::vk-lbutton)
          (cg::key-is-down-p cg::vk-mbutton)
         (cg::key-is-down-p cg::vk-rbutton)))

;;; This alternate definition allows sweeping to start and avoids the 'freeze',
;;; but it does not 'release' when the button is released.

;;; Perhaps we also need (cg::wait-for-mouse-buttons-up) ?
