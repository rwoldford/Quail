;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               function-info.lisp
;;;
;;;                     Catherine Hurley 1994
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(get-function)))

(defun function-string (f &optional (default "??"))
  (cond ((symbolp f)
         (string f))
        ((and (functionp f) (qk::function-name f))
        (string (qk::function-name f)))
        ((functionp f) default)
        (t nil)))


(defgeneric get-function(f)
  (:documentation "Returns a function for f."))


(defun get-quail-function(f) 
  (loop for pkg in (list "QUAIL" "QUAIL-KERNEL")
                              when (and (find-package pkg)
                                        (find-symbol (string f) pkg))
                              do
                              (return (symbol-function
                                       (find-symbol (string f) pkg)))))

(defmethod get-function (f)
  (cond ((null f)   #'identity)
        ((symbolp f)
         (if (fboundp f)
           (symbol-function f)
           (or (get-quail-function f)
               (loop for f1  in (find-all-symbols f)
                     when (fboundp f1) do (return (symbol-function f1))))))
        ((functionp f) f)
        ((and (listp f) (eq (car f) 'lambda))
         (eval `(function ,f)))
        ((and (listp f) (eq (car f) 'function))
         (eval f))
        (t #'identity)))



(defmethod get-function ((f (eql 'median)))
  (if (fboundp 'median) 
    (symbol-function 'median)
    (setf (symbol-function 'median) 
          (get-quail-function 'median))))

(defmethod get-function ((f (eql 'mean)))
  (if (fboundp 'mean) 
    (symbol-function 'mean)
    (setf (symbol-function 'mean) 
          (get-quail-function 'mean))))


(defmethod get-function ((f (eql 'var)))
  (if (fboundp 'var) 
    (symbol-function 'var)
    (setf (symbol-function 'var) 
          (get-quail-function 'var))))



(defmethod get-function ((f (eql 'iqr)))
  (if (fboundp 'iqr) 
    (symbol-function 'iqr)
    (setf (symbol-function 'iqr) 
          (get-quail-function 'iqr))))


(defmethod get-function ((f (eql 'sd)))
  (if (fboundp 'sd) 
    (symbol-function 'sd)
    (setf (symbol-function 'sd) 
          (get-quail-function 'sd))))
