;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                                utility-args.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; copyright (c) 1991 statistical computing laboratory, university of waterloo
;;;
;;;
;;;  authors:
;;;     m.e. lewis 1991.
;;;     r.w. oldford 1991.
;;;
;;;
;;;----------------------------------------------------------------------------


(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(untangle-lambda-list)))

;;;;;;;;;;;;;;;
;;;
;;;

(defun untangle-lambda-list (l-list)
  "Parses the lambda list into an association list, keyed ~
   by the appropriate lambda list keyword."
  (labels ((set-difference-preserving-order (set1 set2)
             (cond ((null set2) set1)
                   ((null set1) nil)
                   (t
                    (set-difference-preserving-order
                     (remove (car set2) set1) (cdr set2)))))
           (up-to-& (a-list)
             (loop for x in a-list
                   until (member x '(&rest &key &optional &aux))
                   collect x)))
    (let ((rest (member '&rest l-list))
          (key (member '&key l-list))
          (optional (member '&optional l-list))
          (aux (member '&aux l-list))
          (body (member '&body l-list))
          (whole (member '&whole l-list))
          (environment (member '&environment l-list))
          (allow-other-keys (member '&allow-other-keys l-list))
          required
          result)
      (setf result
            (acons '&required
                   (setf required
                         (set-difference-preserving-order
                          l-list
                          (concatenate 'list rest key optional aux body)))
                   result))
      (setf result (acons '&rest (up-to-& (cdr rest)) result))
      (setf result (acons '&key (up-to-& (cdr key)) result))
      (setf result
            (acons '&optional (up-to-& (cdr optional)) result))
      (setf result (acons '&aux (up-to-& (cdr aux)) result))
      (setf result
            (acons '&allow-other-keys
                   (up-to-& (cdr allow-other-keys))
                   result))
      (setf result (acons '&whole (up-to-& (cdr whole)) result))
      (setf result (acons '&body (up-to-& (cdr body)) result))
      (setf result
            (acons '&environment (up-to-& (cdr environment)) result))
      (setf result
            (acons '&other-lambda-keys
                   (set-difference-preserving-order l-list
                                                    (concatenate 'list
                                                                 required
                                                                 rest
                                                                 key
                                                                 optional
                                                                 aux
                                                                 allow-other-keys
                                                                 whole
                                                                 body
                                                                 environment))
                   result))
      result)))

;-----

(defgeneric matching-arg-list-p (a b)
  (:documentation "tests whether lambda-lists match."))

(defmethod matching-arg-list-p (lam-list (f-d function-documentation))
  (equal lam-list (lambda-list f-d)))

(defmethod matching-arg-list-p ((f-d function-documentation) lam-list)
  (equal lam-list (lambda-list f-d)))

(defmethod matching-arg-list-p
  ((f-d-1 function-documentation) (f-d-2 function-documentation))
  (equal (lambda-list f-d-1) (lambda-list f-d-2)))
