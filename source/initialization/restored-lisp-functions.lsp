;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          restored-lisp-functions.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1991.
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail)

;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Top-level functions to be restored first.
;;;

(add-restore-lisp-functions  #'setup-system-top-level-loop-variable
                             #'setup-legal-top-level-loop-functions
                             #'setup-legal-quail-loop
                             #'qk::find-quail
                            ;; #'(lambda () (ccl:eval-enqueue '(quail)))
                            ;;#'install-quail-menubar
                             #'qk::load-quail-init
                            ;; The following seems to cause a problem in MCL 4.1 +
                            ;; #'qk::quail-release-print
                            )

(add-save-exit-functions #'qk::quail-release-unprint)
