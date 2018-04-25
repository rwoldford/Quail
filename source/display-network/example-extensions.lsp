;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                             example-extensions.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     G. Desvignes 1988 - 1989
;;;     R.W. Oldford 1985 - 1991.
;;;
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail)

;;; this file is only an example to show how we can deal with more complex data

;;;
;;; class definition
;;;

(defclass junk-extract (quail-object)
       ((a :initform (required-init ($! (quail-prompt-read 
                                               "Initialization for A : ")))
           :initarg :a)
        (b :initform (required-init ($! (quail-prompt-read 
                                               "Initialization for B : ")))
           :initarg :b)
        (sum :initform nil)
        (product :initform nil)))


;;;
;;; method definitions
;;;

(defmethod after-init ((self junk-extract))
       

;;; 
;;; Specialization : Computes IVs of SELF
;;; 

       (call-next-method)
       (setf (slot-value self 'sum)
             (+ (slot-value self 'a)
                    (slot-value self 'b)))
       (setf (slot-value self 'product)
             (* (slot-value self 'a)
                    (slot-value self 'b))))



(defmethod list-subs ((self junk-extract)
                      &optional dont-signal-error)
       

;;; 
;;; Returns the subs of SELF
;;; 

       (let (quail-object-ivs result)
            (dolist (iv (class-slots (find-class 'quail-object)))
                (push (slot-value iv 'name)
                      quail-object-ivs))
            (dolist (iv (class-slots (class-of self)))
                (unless (member (setq iv (slot-value iv 'name))
                               quail-object-ivs)
                    (push (cons (slot-value self iv)
                                iv)
                          result)))
            result))


