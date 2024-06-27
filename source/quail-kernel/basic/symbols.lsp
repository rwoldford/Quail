;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               symbols.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1989, 1990.
;;;
;;;
;;;--------------------------------------------------------------------------------
;;;
;;;  Includes:
;;;          list-symbols
;;;          list-owned-symbols
;;;          list-external-symbols
;;;          variable-symbols
;;;          function-symbols
;;;          nuke-variable-symbols
;;;          nuke-function-symbols
;;;          quail-cleanup-symbols))

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(quail-cleanup-symbols
          push-extension-class
          import-quail-extension-symbols
          list-symbols)))

;----------------------------------------------------------------------------------

;;; symbol finding routines

(defun list-symbols (package-name &key (test #'(lambda (s) 
                                                 (declare (ignore s))
                                                 t)))
  (let* ((y nil))
    (do-symbols (x (find-package package-name) y)
      (if (funcall test x) (setf y (list* x y))))))

(defun list-symbols-containing (name-subseq package-name)
  (let* ((name-subseq (string-downcase (string name-subseq)))
         (test #'(lambda (s) (search name-subseq (string-downcase (string s))))))
    (list-symbols package-name :test test)))

;----------------------------------------------------------------------------------

;;; owned-symbol finding routines

(defun list-owned-symbols (package-name)
  (let* ((p (find-package package-name))
         (test #'(lambda (s) (eq (symbol-package s) p))))
    (list-symbols package-name :test test)))

(defun list-owned-symbols-containing (name-subseq package-name)
  (let* ((p (find-package package-name))
         (name-subseq (string-downcase (string name-subseq)))
         (test #'(lambda (s) (and (eq (symbol-package s) p)
                                  (search name-subseq
                                          (string-downcase (string s)))))))
    (list-symbols package-name :test test)))

;----------------------------------------------------------------------------------

;;; external-symbol (which are always owned) finding routines

(defun list-external-symbols (package-name &key (test #'(lambda (s) 
                                                          (declare (ignore s))
                                                          t)))
  (let* ((y nil))
    (do-external-symbols (x (find-package package-name) y)
      (if (funcall test x) (setf y (list* x y))))))

(defun list-external-symbols-containing (name-subseq package-name)
  (let* ((name-subseq (string-downcase (string name-subseq)))
         (test #'(lambda (s) (search name-subseq (string-downcase (string s))))))
    (list-external-symbols package-name :test test)))

;----------------------------------------------------------------------------------

;;; filters for various types of symbols

(defun variable-symbols (list-of-symbols)
  (let* ((vsymbolp (mapcar #'boundp list-of-symbols)))
    (list-if-t list-of-symbols vsymbolp)))

(defun function-symbols (list-of-symbols)
  (let* ((fsymbolp (mapcar #'fboundp list-of-symbols)))
    (list-if-t list-of-symbols fsymbolp)))

;----------------------------------------------------------------------------------

;;;  nuking various types of symbols in current package

(defun nuke-variable-symbols (list-of-symbols)
  (let ((vsymbols (variable-symbols list-of-symbols)))
    (mapcar #'makunbound vsymbols)))

(defun nuke-function-symbols (list-of-symbols)
  (let ((fsymbols (function-symbols list-of-symbols)))
    (mapcar #'makunbound fsymbols)))

;----------------------------------------------------------------------------------

(defun quail-cleanup-symbols ()
  (let ((z-symbols (list-owned-symbols 'z)))
    (nuke-variable-symbols z-symbols)
    (nuke-function-symbols z-symbols)
    (mapcar #'unintern z-symbols))
  nil)

;----------------------------------------------------------------------------------

(defvar *extension-symbols*)

(eval-when (:load-toplevel :execute) (setf *extension-symbols* nil))

(defun push-extension-symbols (&rest symbols)
  (declare (special *extension-symbols*))
  (loop for symbol in symbols
        do (pushnew symbol *extension-symbols*)))

;; Remark (Greg Anglin 93 12): this should really use mop-<system>.lisp or whatever
;;
(defun push-extension-class (class-name)
  (let (slot-names)
    #+:pcl
    (let* ((wrapper (progn
                      (make-instance class-name)    ;; otherwise pcl::wrapper will be NIL
                      (slot-value (find-class class-name) 'pcl::wrapper))))
      (if wrapper
        (setf slot-names (elt wrapper 9))
        (warn "Wrapper in class ~S is nil, even after a make-instance."
              class-name)))
    #+:ccl-2
    (let* ((direct-slots (progn
                           (make-instance class-name)    ;; might be a good test, for now
                           (slot-value (find-class class-name) 'ccl::direct-slots))))
      (setf slot-names (cdr (mapcar #'first direct-slots))))
    #+(and :aclunix (not :aclpc))
    (let* ((direct-slots (clos::class-direct-slots (find-class class-name))))
      (setf slot-names (mapcar #'(lambda (slot-def) (slot-value slot-def 'clos::name))
			       direct-slots)))
    (apply #'push-extension-symbols class-name slot-names)))
  
;;; this routine imports symbols useful in extending z.
;;; Typical usage:
;;;    (def-package :neat-new-package)
;;;    (in-package :neat-new-package)
;;;    (eval-when (compile load)
;;;      (zk:import-quail-extension-symbols 'quail-kernel::quail-kernel :neat-new-package)
;;;      (zk:import-quail-extension-symbols 'zffi::zffi :neat-new-package)
;;;        ... etc ...
;;;      )

(defun import-quail-extension-symbols (from-package to-package)
  (import (variable-symbols (list-owned-symbols from-package))
          to-package)
  (import (function-symbols (list-owned-symbols from-package))
          to-package)
  (import *extension-symbols* to-package)
  (import (list-symbols-containing "path-" "MAKE") to-package))

#|
;;;  Commented out by Greg Anglin during Allegro port, Dec '93.
;;;  I wonder how much of this file is really necessary now that we have
;;;  the -package.lisp files??

;;;  Get the path- functions now for the quail-kernel package

#-:ccl-1   ;; ccl-1 seems to have trouble with these ...
(import (list-symbols-containing "path-" "MAKE") "quail-kernel")
|#
