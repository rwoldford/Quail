;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               scan.lisp                               
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

(in-package :quail-kernel)

;--------------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(scan)))

(defvar *scan-file-default* nil)
(defvar *scan-delimiter-default* #\\)
(defvar *scan-ignore-default* nil)
(defvar *eof* (gensym "this-better-not-show-up-in-the-input-anywhere"))

(defclass scan-env (quail-file)
  ((delimit :initarg :delimit)
   (ignore :initarg :ignore)))

(defun scan (file &key
                  (delimit nil)
                  (ignore nil))
  (setf delimit (if delimit
                  (if (eq delimit t)
                    *scan-delimiter-default*
                    delimit)
                  nil))
  (setf ignore (if (eq ignore :default)
                 *scan-ignore-default*
                 (copy-list ignore)))
  (setf file (quail-file-pathname file))
  (make-instance 'scan-env
                 :pathname file
                 :delimit delimit
                 :ignore ignore))

(defun scan-reset (scan-env)
  (quail-file-close scan-env)
  (quail-file-open scan-env))

(defun scan-cleanup (scan-env)
  (quail-file-destroy scan-env))
       
(defun scan-1 (scan-env)
  ;;  This function assumes the stream is open ...
  ;;  should have a with-open-quail-objects wrapped around it !!
  (with-slots (stream ignore delimit) scan-env
    (let (the-input)
      ;;
      ;;  The delimited case isn't very useful
      ;;
      (if delimit
        ;;
        ;;  Assumes the list is well-structured:  The delimiter is the
        ;;  last non-whitespace character in the file.  Thus this type of scanning
        ;;  is probably only useful for programmatically constructed files.
        ;;
        (progn
          (setf the-input (if (eq *eof* (peek-char t stream nil *eof* nil))
                            *eof*
                            (read-delimited-list delimit stream t)))
          (if (not (eq *eof* the-input))
            (loop for junk in ignore
                  do (setf the-input (delete junk the-input)))))
        ;;
        ;;  This is the more useful case.
        ;;
        (let ((junk-input t))
          (loop while junk-input
                do 
                (setf the-input (read stream nil *eof* nil))
                (if (not (member the-input ignore))
                  (setf junk-input nil)))))
      the-input)))

(defun scan-into-adjustable-vector (scan-env &optional (size-guess 16))
  (with-open-quail-objects (scan-env) quail-file
    (let ((eof nil)
          (count 0)
          (size size-guess)
          (vec (make-array (list size-guess) :adjustable t))
          the-input)
      (loop while (not eof)
            do
            (setf the-input (scan-1 scan-env))
            (when (not (setf eof (eq the-input *eof*)))            
              (when (>= count size)
                (setf size (* 2 size))
                (setf vec (adjust-array vec (list size))))
              (setf (elt vec count) the-input)
              (incf count)))
      (if (eq size count)
        vec
        (adjust-array vec (list count))))))
             
              
      

    
    


