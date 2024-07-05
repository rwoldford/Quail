;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               class-info.lisp
;;;
;;;                     Catherine Hurley 1992
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(view-classes-with-spec list-view-classes)))

(defun list-view-classes (&optional (class-name 'view-mixin))
  "Returns a list of all view classes with class-name on~
   the class precedence list.~
   class-name defaults to 'view-mixin."
   
  (let ((base-class (find-class class-name nil ))
        class-names a-class)
      (if base-class
      (do-symbols (sym (find-package 'views) )
        (unless  (eq sym class-name)
          (setq a-class (find-class sym nil))
          (if (and a-class 
                   (member base-class (qk:class-precedence-list a-class)
                           ))
            (setq class-names (push sym class-names))))))
    class-names))




(defun view-classes-with-spec(class-spec)
  (let ((view-classes (list-view-classes 'view)))
    (labels ((subclass (a b)
               (if (symbolp a)
                 (setq a (find-class a)))
               (if (symbolp b)
                 (setq b (find-class b)))
               (cond ((eq a b)
                      nil)
                     ((and (listp b) (eq 'not (car b)))
                      (not (subclass a (second b))))
                     (t (member b (qk:class-precedence-list a)
                      ))))
             
             (view-classes-not-spec(spec)
               (if (symbolp spec)
                 (setq spec (find-class spec nil)))
               (if spec
                 (loop for class in view-classes 
                       unless (subtypep class spec)
                       collect class))))
      
      (cond ((and class-spec (listp class-spec) (eq 'not (car class-spec)))
             (view-classes-not-spec (second class-spec)))
            ((and class-spec (listp class-spec) ) 
             (let* ((class-op 
                     (case (car class-spec)
                       (or #'some)
                       (and #'every)
                       (t nil)))
                    (spec-classes (cdr class-spec)))
               (when class-op
                 (loop for v in view-classes
                       when (funcall class-op #'(lambda(x) (subclass v x)) spec-classes)
                       collect v))))
            ((symbolp class-spec) 
             (list-view-classes class-spec))
            (t nil))))
  )
