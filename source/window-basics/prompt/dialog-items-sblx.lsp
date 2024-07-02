;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                         dialog-items-sblx.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  Authors:
;;;     R.W. Oldford 1994
;;;     G.W. Bennett 1996, 2020
(in-package :wb)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     dialog items in ACL-PC are used.
;;; But the following form is called in canvas-to-ps.lsp
;;; ACL doesn't need to re-enable the menubars.
(defun re-enable-menubars (&rest whatever)
  (declare (ignore whatever)))