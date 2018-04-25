;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               z-slots.lisp                               
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
;;;          collect-slot-inits
;;;          output-make-instance
;;;          *indent-size*
;;;

(in-package :quail-kernel)

;------------------------------------------------------------------------------

#|
(defvar *indent-size*)
(setf *indent-size* 2)

(defgeneric collect-slot-inits (thing)
  (:method-combination list))

(defun slot-initarg (initarg value default-value)
  (if (equal value default-value)
    nil
    (list initarg value)))

(defun string-slot-initarg (slot-initarg indent)
  (declare (special *indent-size*))
  (let* ((initarg (car slot-initarg))
         (initarg (if (and (symbolp initarg)
                           (not (keywordp initarg)))
                    `(quote ,initarg)
                    initarg))
         (initarg-string (format nil "~S " initarg))
         (initarg-len (length initarg-string))
         (value (cadr slot-initarg))
         (new-indent (+ indent
                        *indent-size*
                        initarg-len)))
    (format nil "~&~A~A~A"
                (make-sequence 'string (+ indent *indent-size*)
                               :initial-element #\space)
                initarg-string 
                (make-instance-string value new-indent))))

(defgeneric make-instance-string (thing &optional indent))

(defmethod make-instance-string ((self standard-object)
                                 &optional (indent 0))
  (let* ((slot-inits 
          (delete-duplicates (delete nil 
                                     (apply #'append 
                                            (collect-slot-inits self)))
                             :key #'car
                             :test #'equal))
         (string-slot-inits 
          (mapcar #'(lambda (x) (string-slot-initarg x indent))
                  slot-inits)))
    (format nil "(make-instance '~S~{~A~})"
            (class-name (class-of self))
            string-slot-inits)))

(defmethod make-instance-string ((self number) &optional (indent 0))
  (format nil "~S" self))

(defmethod make-instance-string ((self t) &optional (indent 0))
  (format nil "'~S" self))

(defun output-make-instance (stream obj &optional name)
  (declare (special *indent-size*))
  (if name
    (format stream "(setf ~S~&~A~A)"
                          name
                          (make-sequence 'string *indent-size*
                                                 :initial-element #\Space)                          
                          (format nil "~A"
                                      (make-instance-string obj
                                                            *indent-size*)))
    (format stream "~A" (make-instance-string obj)))
  (values))

|#

;------------------------------------------------------------------------------

;;  Examples

;;  These show how to write collect-slot-inits methods, which should be
;;  defined in the same file as the classes to which they apply.

#|

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(foo bar foo1 bar1)))
 
(defclass foo ()
  ((foo1 :accessor foo1
         :initarg :foo1
         :initform "f")))

(defclass bar (foo)
  ((bar1 :accessor bar1
         :initarg bar1
         :initform "b")))

(defmethod collect-slot-inits list ((self foo))
  (with-slots ((f foo1)) self
    (list (slot-initarg :foo1 f "f"))))

(defmethod collect-slot-inits list ((self bar))
  (with-slots ((b bar1)) self
    (list (slot-initarg 'bar1 b "b"))))

|#


         

