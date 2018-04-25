;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                        host-fonts-mcl.lisp                             
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1994 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     N.G. Bennett 1993.
;;;     R.W. Oldford 1994.
;;;
;;;--------------------------------------------------------------------------------

(in-package :host-draw)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(font-transfer-modes font-styles font-transfer-modes font-names
          cached-host-font clear-host-font-cache
          get-canvas-font-name get-canvas-font-size get-canvas-font-style
          get-canvas-font-transfer-mode host-font-description)
        ))


(defvar
  *font-transfer-modes*
  (list :srcCopy :srcOr :srcXor :srcBic
        :srcPatCopy :srcPatOr :srcPatXor :srcPatBic)
  "Font transfer-modes cache")

(defun font-transfer-modes ()
  *font-transfer-modes*)

(defvar
  *font-styles*
  (loop
    for
    s in ccl:*style-alist*
    collect (car s))
  "Font styles cache")

(defun font-styles ()
  *font-styles*)

(defun font-names ()
  (declare (special ccl::*font-list*))
  ccl::*font-list*)


(defun cached-host-font (canvas-font)
  "Returns the host-font that has been cached for this canvas-font. ~
   If one is not cached, it is found, cached, and returned." 
  (let ((host-font (sixth canvas-font)))
    (unless host-font
      (setf host-font
            (remove NIL
                    (concatenate 'list
                                 (list (second canvas-font))
                                 (third canvas-font)
                                 (cdddr canvas-font))))
      (nconc canvas-font (list host-font)))
    host-font))

(defun clear-host-font-cache (canvas-font)
  "Clears the host-font cache for this canvas-font."
  (if (sixth canvas-font)
    (rplacd (last canvas-font 2) NIL)))


(defun get-canvas-font-name (host-font)
  "Translates the host Common Lisp font representation to get the name ~
   of the corresponding canvas-font in window-basics."
  (unless (listp host-font) (setf host-font (list host-font)))
  (loop for n in (font-names)
        when (find n host-font
                   :test #'(lambda (a b)
                             (and (stringp a) (stringp b)
                                  (string-equal a b))))
        return n))

(defun get-canvas-font-size (host-font)
  "Translates the host Common Lisp font representation to get the size ~
   of the corresponding canvas-font in window-basics."
  (unless (listp host-font) (setf host-font (list host-font)))
  (find-if #'numberp host-font))

(defun get-canvas-font-style (host-font)
  "Translates the host Common Lisp font representation to get the style ~
   of the corresponding canvas-font in window-basics."
  (unless (listp host-font) (setf host-font (list host-font)))
  (loop for s in (font-styles)
        when (find s host-font)
        collect s))


(defun get-canvas-font-transfer-mode (host-font)
  "Translates the host Common Lisp font representation to get the transfer-mode ~
   of the corresponding canvas-font in window-basics."
  (unless (listp host-font) (setf host-font (list host-font)))
  (loop for m in
        (font-transfer-modes)
        when (find m host-font)
        return m))


(defun host-font-description (host-font)
  "Returns four values that represent (in pixels) the ascent, ~
   descent, max-width, and leading (suggested spacing between ~
   lines) of the host-font."
  (ccl::font-info host-font))
