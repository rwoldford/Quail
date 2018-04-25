;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               with-ref.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1991.
;;;
;;;
;;;--------------------------------------------------------------------------------

(in-package :qk)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(with-ref)))

(defun matching-p (form dummy-form)
  (or (eq dummy-form :dummy)
      (equal form dummy-form)
      (if (and (listp form) (listp dummy-form))
        (let ((matching t))
          (loop while matching
                for f in form
                as df in dummy-form
                do (setf matching (matching-p f df)))
          matching)
        nil)))

;;  This next function collects up the pieces from form which match the
;;  :dummy indicators in dummy-form, knowing in advance that the forms
;;  are lists and match in the sense of matching-p.

(defun collect-dummy-pieces (form dummy-form)
  (loop for f in form
        as df in dummy-form
        when (eq df :dummy) append (list f)
        when (listp df) append (collect-dummy-pieces f df)))

;;;  Given the right number of dummies, this function DESTRUCTIVELY 
;;;  substitutes them for the :dummy indicators in specs.

(defun substitute-dummy-pieces (specs dummies)
  (loop for i from 0 to (- (length specs) 1)
        when (eq (elt specs i) :dummy) do (progn
                                            (setf (elt specs i) (first dummies))
                                            (setf dummies (rest dummies)))
        when (listp (elt specs i)) do 
        (multiple-value-bind (new-specs new-dummies)
                             (substitute-dummy-pieces (elt specs i) dummies)
          (declare (ignore new-specs))
          (setf dummies new-dummies)))
  (values specs dummies))

;; make-ref-template makes a list which will eventually hold:
;;
;;  1.  the original ref'd object
;;  2.  a template of the ref, initially :none
;;  3.  the specs including the :dummy indicators
;;

(defun make-ref-template (r dummy-specs)
  (vector r
          :none
          dummy-specs))

(defmacro original-ref (ref-template)
  `(svref ,ref-template 0))

(defmacro template (ref-template)
  `(svref ,ref-template 1))

(defmacro args (ref-template)
  `(svref ,ref-template 2))

;;;  This is the function which will take the place of the ref in code.  The (only)
;;;  win here is not doing many different make-instances.

(defun template-ref (ref-template &rest dummy-pieces)
  (let ((args (substitute-dummy-pieces (copy-tree (args ref-template))
                                       dummy-pieces)))
    (if (eq (template ref-template) :none)
      ;; This'll happen the first time thru, since template initially :none
      (setf (template ref-template) 
            (apply #'ref 
                   (original-ref ref-template)
                   args))
      ;; This is the usual case, avoiding a new make-instance
      (ref-kernel (template ref-template)
                  (original-ref ref-template)
                  args))))

(defun template-setf-ref (new-value ref-template &rest dummy-pieces)
  (let ((targ (apply #'template-ref ref-template dummy-pieces)))
    (setf-ref-kernel new-value targ)))

(defun convert-with-ref-body-recursion (body dummy-ref ref-template-symbol)
  (loop for i from 0 to (- (length body) 1)
        as b in body
        when (and (listp b)
                  (eq (first b) 'setf)
                  (listp (second b))
                  (eq (first (second b)) 'ref)
                  (matching-p (rest (second b)) dummy-ref))
        do (setf (elt body i)
                 `(template-setf-ref ,(third b)
                                     ,ref-template-symbol
                                     ,@(collect-dummy-pieces (rest (second b))
                                                             dummy-ref)))
        when (and (listp b)
                  (eq (first b) 'ref)
                  (matching-p (rest b) dummy-ref))
        do (setf (elt body i)
                 `(template-ref ,ref-template-symbol
                                ,@(collect-dummy-pieces (rest b)
                                                        dummy-ref)))
        when (listp b)
        do (convert-with-ref-body-recursion b dummy-ref ref-template-symbol)
        )                    
  body)
                
(defun convert-with-ref-body (body dummy-refs dummy-ref-template-symbols)
  (loop for dr in dummy-refs
        as drts in dummy-ref-template-symbols
        do (convert-with-ref-body-recursion body dr drts))
  body)

(defun dummy-lets (dummy-refs dummy-ref-template-symbols)
  (loop for dr in dummy-refs
        as drts in dummy-ref-template-symbols
        collect `(,drts (make-ref-template ,(first dr) (quote ,(rest dr))))))

(defmacro with-ref (dummy-refs &rest body)
  (let* ((dummy-ref-template-symbols (loop for dr in dummy-refs collect (gensym))))
    `(let* ,(dummy-lets dummy-refs dummy-ref-template-symbols)
       ,@(convert-with-ref-body (copy-tree body)
                                dummy-refs
                                dummy-ref-template-symbols))))

#|
;;; Seems to be a substantial improvement !!

(defun test-it (x count)
  (let ((size (first (dimensions-of x))))
    (time (let ((xc (make-ref-template x '(:dummy t)))
                (xr (make-ref-template x '(t :dummy))))
            (loop for j from 1 to count do
                  (loop for i from 0 to (- size 1)
                        do (progn
                             (template-ref xc i)
                             (template-ref xr i))))))
    (time (loop for j from 1 to count do
                (loop for i from 0 to (- size 1)
                      do (progn
                           (ref x i t)
                           (ref x t i)))))))

(defun test1 (x count)
  (with-ref ((x :dummy t)
             (x t :dummy))
    (let ((size (first (dimensions-of x))))
      (time (loop for j from 1 to count do
                  (loop for i from 0 to (- size 1)
                        do (progn
                             (ref x i t)
                             (ref x t i))))))))

(defun test2 (x count)
    (let ((size (first (dimensions-of x))))
      (time (loop for j from 1 to count do
                  (loop for i from 0 to (- size 1)
                        do (progn
                             (ref x i t)
                             (ref x t i)))))))

|#

    

