;;; -*- Mode: LISP -*-


(in-package 'Z)


;;;
;;; class definitions
;;;

(defclass quail-object (prompt-mixin linked-object-mixin indexed-object-mixin 
                              editable-object-mixin named-object-mixin 
                              documented-object-mixin)
       ((zoom-window-cache :initform nil)))


(defclass ref-array (quail-object)
       
       ;; 
       ;; An abstract class from which instantiable Array classes inherit
       ;; generic behaviors
       ;; 

       ((the-cml-array :initform nil)
        
        ;; 
        ;; The array of data values is installed here once and for all when
        ;; the instance of the array is made. Thereafter this instance
        ;; variable may not be changed
        ;; 
        ))


(defclass memo (quail-object) nil)


;;;
;;; method definitions
;;;

(defmethod zoom ((self ref-array))
       
;;; 
;;; Brings up Array inspector on the contents of this array.
;;; 

       (inspect (slot-value self 'the-cml-array)))


(defmethod init-ivs ((self ref-array))
       

;;; 
;;; Specialization
;;; 

       self)


(defmethod zoom ((self quail-object))
       

;;; 
;;; Zooms in on the current object by displaying it and its sub-structures in
;;; a new view.
;;; The browser is cached in self and self is deposited in the browser. This
;;; circular structure is removed by the specialized Destroy methods for
;;; quail-objectS and Microscopic-View
;;; 

       (if (and (slot-value self 'zoom-window-cache)
                (understands (slot-value self 'zoom-window-cache)
                       'zoom))
           (zoom (slot-value self 'zoom-window-cache))
           (let ((zoomed-view (make-instance 'microscopic-view)))
                (setf (slot-value zoomed-view 'zoomed-object)
                      self)
                (setf (slot-value self 'zoom-window-cache)
                      zoomed-view)
                (zoom zoomed-view))))


(defmethod list-subs ((self quail-object)
                      &optional
                      (dont-signal-error nil))
       

;;; 
;;; Return a list comprised of the sub-structures of self. Invoked by a
;;; MicroView. SubClassResponsibility to provide the definitive description
;;; 

       (if dont-signal-error
           nil
           (sub-class-responsibility self 'list-subs)))


(defmethod descriptive-label ((self quail-object))
       

;;; 
;;; Construct a descriptive string label SuClass responsibility to specialize
;;; with more elaborate description.
;;; 

       (let ((my-name (get-name self))
             (my-class (class-name (class-of self))))
            (concatenate 'string (if my-name
                                     (string my-name)
                                     " ")
                   " "
                   (string my-class))))


(defmethod zoom ((self memo))
       

;;; 
;;; Specialized to edit notes
;;; 

       (read-notes self))


;;;
;;; function definitions
;;;


(defun mode-of-list (the-list)
       

;;; 
;;; Determines the common type of the elements of a list in order to build an
;;; array of the correct type
;;; The different available modes are STRING, FLOAT and BOOLEAN
;;; 

       (let ((common-mode 'i-dont-know))
            (dolist (item the-list)
                (case common-mode
                    (i-dont-know (cond ((or (null item)
                                            (eq item 't))
                                        (setq common-mode 'boolean))
                                       ((or (integerp item)
                                            (floatp item))
                                        (setq common-mode 'float))
                                       (t (setq common-mode 'string))))
                    ((boolean) (cond ((or (null item)
                                          (eq item 't))
                                      nil)
                                     (t (setq common-mode 'string))))
                    ((float) (cond ((or (integerp item)
                                        (floatp item))
                                    nil)
                                   (t (setq common-mode 'string))))
                    ((string) (return))))
            common-mode))


(defmacro required-init (init-form)
       (declare (ignore init-form))
       nil)


(defun requires-variable (var)
       

;;; 
;;;  prompts for var
;;; 

       (let ((result (Z-prompt-read (concatenate 'string 
                                                     "Enter the value for " 
                                                     (symbol-name var)))))
          (if (symbolp result)
              (or ($! result t) 
                  (list 'quote result))
              (eval result))))