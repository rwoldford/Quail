
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               class-info-mcl.lisp
;;;
;;;                     Catherine Hurley 1992
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(view-classes-with-spec list-view-classes)))

(defun class-precedence-list(class-name)
  (let ((class (find-class class-name nil)))
    (if class (mapcar #'class-name 
                      (qk:precedence-list class))))))






(defun list-view-classes (&optional (class-name 'view-mixin))
  "Returns a list of all view classes with class-name on the class precedence list.~
   class-name defaults to 'view-mixin."
   
  (let ((base-class (find-class class-name nil ))
        class-names a-class)
      (if base-class
      (do-symbols (sym ( find-package 'views) )
        (unless  (eq sym class-name)
          (setq a-class (find-class sym nil))
          (if (and a-class 
                   (member base-class 
                           (qk:precedence-list a-class)))
            (setq class-names (push sym class-names))))))
    class-names))




(defun view-classes-with-spec(class-spec)
  (if (and class-spec (listp class-spec))
    (let* ((spec-classes (cdr class-spec))
           (spec-op (car class-spec))
           (base-class-objs (mapcar #'find-class spec-classes))
           class-names a-class-obj)
      
      (flet ((test (b)
               (if (eq spec-op 'and)
                 (subsetp base-class-objs b)
                 (intersection base-class-objs b))))
        
        (do-symbols (sym (find-package 'views) )
          (unless  (member sym spec-classes)
            (setq a-class-obj (find-class sym nil))
            (if (and a-class-obj 
                     (test (slot-value a-class-obj 'ccl::precedence-list)))
              (setq class-names (push sym class-names)))))
        class-names))
    (list-view-classes class-spec)))
