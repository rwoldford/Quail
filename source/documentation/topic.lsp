;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                                topic.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; copyright (c) 1990 statistical computing laboratory, university of waterloo
;;;
;;;
;;;  authors:
;;;     r.w. oldford 1991.
;;;     M.E. LEWIS 1991.
;;;
;;;
;;;----------------------------------------------------------------------------



(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(install-super-topic install-sub-topic
          uninstall-super-topic uninstall-sub-topic
          )))

(defun topic (arg)
  "Returns the documentation object for the topic as given by the argument.  ~
   The argument may be a symbol or a list of a symbol and a doc-type.  ~
   If the former then the topic documentation is returned.  If the latter, then ~
   the quail documentation object of type specified by the second element in the ~
   list is returned."
  (flet
    ((get-topic-doc (name type)
       (or (doc name type)
           (and (eq (cadr arg) :topic)
                (make-instance
                 'topic-documentation
                 :name name
                 :doc-capsule
                 (format NIL "No documentation available on this topic."))))))
    (cond
     ((symbolp arg) (doc arg :topic))
     ((listp arg)
      (if (listp (cdr arg))
        (get-topic-doc (car arg) (cadr arg))
        (get-topic-doc (car arg) (cdr arg))))
     ((typep arg 'documentation-object)  arg)
     (T  (quail-error "Cannot determine the topic ~
                       documentation for ~s." arg))
     )
    ))
  

(defun eq-topic (a b)
  "True if the two arguments are equivalent expressions for the same topic, ~
   NIL otherwise."
  (or (equal a b)
      (and (string= (name a) (name b))
           (eq (doc-type a) (doc-type b)))))

(defgeneric add-super-topic (sub super)
  (:documentation
   "Add the second argument to the first argument's list of super-topics."))

(defgeneric add-sub-topic (super sub)
  (:documentation
   "Add the second argument to the first argument's list of sub-topics."))

(defgeneric remove-super-topic (sub super)
  (:documentation
   "Remove the second argument from the first argument's list of super-topics."))

(defgeneric remove-sub-topic (super sub)
  (:documentation
   "Remove the second argument from the first argument's list of sub-topics."))

(defun install-super-topic (super sub)
  "Establishes the super-topic--sub-topic relationship between the first ~
   and second arguments."
  (add-sub-topic super sub)
  (add-super-topic sub super))

(defun install-sub-topic (super sub)
  "Establishes the super-topic--sub-topic relationship between the first ~
   and second arguments."
  (add-sub-topic super sub)
  (add-super-topic sub super))

(defun uninstall-super-topic (super sub)
  "Destroys the super-topic--sub-topic relationship between the first ~
   and second arguments."
  (remove-sub-topic super sub)
  (remove-super-topic sub super))

(defun uninstall-sub-topic (super sub)
  "Destroys the super-topic--sub-topic relationship between the first ~
   and second arguments."
  (remove-sub-topic super sub)
  (remove-super-topic sub super))
  

(defmethod add-super-topic ((sub documentation-object) (super topic-documentation))
  (with-accessors
    ((sub-supers super-topics))
    sub
    (if (not (member super sub-supers :test #'eq-topic))
      (push super sub-supers))
    )
  )


(defmethod add-sub-topic ((super topic-documentation) (sub documentation-object))
  (with-accessors
    ((super-subs sub-topics))
    super
    (if  (not (member sub super-subs :test #'eq-topic))
      (push sub super-subs))
    )
  )

(defmethod add-super-topic ((sub T) (super T))
  (let ((sub-topic (topic sub))
        (super-topic (topic super)))
    (add-super-topic sub-topic super-topic)))

(defmethod add-sub-topic ((sub T) (super T))
  (let ((sub-topic (topic sub))
        (super-topic (topic super)))
    (add-sub-topic super-topic sub-topic)))



(defmethod initialize-instance :after ((self topic-documentation)
                                       &key)
  ;;(if (sub-topics self)
  ;;  (loop for st in (sub-topics self) do
  ;;        (add-super-topic st self)))
  )


(defun set-topic (symbol
                  &rest keyword-pairs
                  &key (topic-class 'quail-kernel::topic-documentation)
                  )
  "Creates a topic documentation object and installs it."
  (setf (doc symbol :topic)
        (apply
         #'make-instance topic-class :allow-other-keys T keyword-pairs)))
