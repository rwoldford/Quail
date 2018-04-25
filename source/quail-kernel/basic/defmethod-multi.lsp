;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               defmethod-multi.lisp                             
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1991, 1995.
;;;
;;;----------------------------------------------------------------------------

(in-package :qk)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(defmethod-multi defmethod-multi-one-body)))

(defmacro defmethod-multi (&rest defmethod-args)
  (let* ((split (position-if-not #'(lambda (x) (and x (atom x)))
                                 defmethod-args))
         (lambda-list (if split
                        (elt defmethod-args split)
                        (quail-error "Malformed defmethod-multi: ~
                                no lambda-list provided.")))
         (function-name-and-method-qualifiers (subseq defmethod-args 0 split))
         (body (subseq defmethod-args (+ 1 split)))
         (split (position-if #'(lambda (x) (member x lambda-list-keywords))
                             lambda-list))
         (multi-specialized-parameters (if split 
                                         (subseq lambda-list 0 split)
                                         lambda-list))
         (other-parameters (if split
                             (subseq lambda-list split)
                             nil))
         specialized-parameters-cross-product)
    (setf specialized-parameters-cross-product
          (defmethod-cross-product multi-specialized-parameters))
    `(progn ,@(loop for specialized-parameters 
                    in specialized-parameters-cross-product
                    collect
                    `(defmethod ,@function-name-and-method-qualifiers
                       (,@specialized-parameters ,@other-parameters)
                       ,@body)))
    ))

(defmacro defmethod-multi-one-body (&rest defmethod-args)
  (let* ((split (position-if-not #'(lambda (x) (and x (atom x)))
                                 defmethod-args))
         (lambda-list (if split
                        (elt defmethod-args split)
                        (quail-error "Malformed defmethod-multi: ~
                                      no lambda-list provided.")))
         (function-name-and-method-qualifiers (subseq defmethod-args 0 split))
         (body (subseq defmethod-args (+ 1 split)))
         (split (position-if #'(lambda (x) (member x lambda-list-keywords))
                             lambda-list))
         (multi-specialized-parameters (if split 
                                         (subseq lambda-list 0 split)
                                         lambda-list))
         (other-parameters (if split
                             (subseq lambda-list split)
                             nil))
         body-par-list
         specialized-parameters-cross-product)
    (setf specialized-parameters-cross-product
          (defmethod-cross-product multi-specialized-parameters))
    (setf body-par-list (mapcar #'(lambda (x)
                                    (if (listp x) (first x) x))
                                (append
                                 (first specialized-parameters-cross-product)
                                 other-parameters)))
`(flet ((body-fn ,body-par-list
              ,@body))
       ,@(loop for specialized-parameters 
               in specialized-parameters-cross-product
               collect
               `(defmethod ,@function-name-and-method-qualifiers
                  (,@specialized-parameters ,@other-parameters)
                  (funcall #'body-fn ,@body-par-list))))
    ))

(defun defmethod-cross-product (multi-specialized-parameters)
  (if multi-specialized-parameters
    (let* ((sp (first multi-specialized-parameters))
           (xprod-rest (defmethod-cross-product 
                         (rest multi-specialized-parameters)))
           (var (if (listp sp)
                  (first sp)
                  sp))
           (specializers (if (listp sp)
                           (prog1
                             (second sp)
                             (if (nthcdr 2 sp)
                               (error "Malformed multiple specialization in DEFMETHOD-MULTI: ~S. ~
                                       ~&Should probably read (~S ~S)." sp var (rest sp))))
                           :not-provided)))
      (if (and (listp specializers)
               (not (eq 'eql (first specializers))))
        (loop for s in specializers
              append (xprod-with var s xprod-rest))
        (xprod-with var specializers xprod-rest)))))

(defun xprod-with (var spec xprod)
  (let ((var-spec (if (eq spec :not-provided)
                    var
                    (list var spec))))
    (if xprod
      (loop for xp in xprod
            collect (list* var-spec xp))
      (list (list var-spec)))))

