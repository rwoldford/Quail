;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               window-basics-package.lisp

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1991.
;;;
;;;
;;;----------------------------------------------------------------------------
(in-package :quail)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(class-browse)))


(defun class-browse (class-name &rest more-class-names)
  "Produces a graphical browser of the network of classes and their subclasses ~
   rooted at class-name and more-class-names, if more are given."
  (let* ((classes
          (loop
            for c-n
            in (if more-class-names
                 (cons  class-name more-class-names)
                 (list class-name))
            collect (find-class c-n))))
    (browse (make-browser) classes)))
