(setf a (make-doc 'deriv :generic-function))
(qk::method-list a)

(setf q::methods (qk::method-list a))
(setf q::symbol 'deriv)

(step (make-methods-display-list
                :symbol q::symbol
                :title "Methods"
                :methods q::methods
                :group? T))

(defun foo () (+ 2 3 4))

(defclass foo ()
  ((a :initform (+ 2 3) :accessor a-of
      :reader aa-of :writer aa
      :documentation "This is a."
      :initarg :a-BIG-a)
   (b :initform #'foo
      :reader bb-of :writer bb
      :documentation "This is b."
      :initarg :a-BIG-b)
   (c :initform (foo)
      :accessor c-of :writer c
      :documentation "This is c."
      :initarg :a-BIG-c)
   ))

(defmethod a-of :before ((thing foo)) (print "hi there"))

(defclass bat (foo)
  ((a :initform (+ 2 3)
      :reader aa-of :writer aa
      :documentation "This is a."
      :initarg :a-BIG-bat-a)
   (c :initform (list 1 2 3)
      :accessor c-of :writer c
      :documentation "This is c."
      :initarg :a-BIG-c)
   ))

(defclass fu (foo)
  ())

(defclass baz (bat)
  ((a :initform "baz-a"
      :reader aa-of :writer aa
      :documentation "This is baz's a."
      :initarg :a-BIG-bat-a)
   (d :initform NIL
      :accessor d-of :writer d
      :documentation "This is d."
      :initarg :a-BIG-d)
   ))

(setf s-f (qk::collect-slot-definitions 'foo))
(qk::collect-accessors 'foo)
(qk::collect-accessors 'fu)
(help 'deriv)
(clear-doc 'foo)
(class-name (find-class 'foo))
(clear-doc 'fu)
(clear-doc 'bat)
(clear-doc 'bar)
(help 'foo :class)
(inspect 'foo)
(inspect (doc 'foo :class))

(defun look-at-slots (class)
  (loop for s in (qk::collect-slot-definitions class)
        when (cdr s)
        do
        (format *terminal-io*
                      "~&~%CLASS: ~s ~10T slot-type: ~s"
                      (qk::doc-class-object (first (cdr s)))
                      (car s))
        (loop for s-d in (cdr s)
              do
              
              (format *terminal-io*
                      "~&~%~5TSlot-name: ~s"
                      (qk::slot-definition-name s-d))
              (format *terminal-io*
                      "~&~10TInitargs: ~s"
                      (qk::slot-definition-initargs s-d))
              (format *terminal-io*
                      "~&~10TInitform: ~s"
                      (qk::slot-definition-initform s-d))
              (format *terminal-io*
                      "~&~15TInitform's value: ~s"
                      (let ((initform (qk::slot-definition-initform s-d)))
                      (cond
                       ((functionp initform)
                        (funcall initform))
                       ((and (listp initform)
                             (= 1 (length initform)))
                        (car initform))
                       (T initform))))
              (format *terminal-io*
                      "~&~10TInitfunction: ~s"
                      (qk::slot-definition-initfunction s-d))
              (format *terminal-io*
                      "~&~10TAllocation: ~s"
                      (qk::slot-definition-allocation s-d))
              (format *terminal-io*
                      "~&~10TReaders: ~s"
                      (qk::slot-definition-readers s-d))
              (format *terminal-io*
                      "~&~10TWriters: ~s"
                      (qk::slot-definition-writers s-d))))
  "done")

(look-at-slots 'foo)
(look-at-slots 'bat)
(look-at-slots 'baz)
(look-at-slots 'qk::slot-definition)
(class-slots (find-class 'foo))
(defmethod print-em ((x foo) (y bat) (z t))
  (print (list x y z)))

(setf dm-f (specializer-direct-methods (find-class 'foo)))
(inspect dm-f)
(setf dm-b (specializer-direct-methods (find-class 'bat)))
(inspect dm-b)
(setf dm-sd (specializer-direct-methods (find-class 'qk::slot-definition)))
(inspect dm-sd)

(setf dgf-f (specializer-direct-generic-functions (find-class 'foo)))
(inspect dgf-f)
(setf dgf-b (specializer-direct-generic-functions (find-class 'bat)))
(inspect dgf-b)
(setf dgf-sd (specializer-direct-generic-functions
              (find-class 'qk::slot-definition)))
(inspect dgf-sd)

(setf r-f (qk::slot-definition-readers (find-class 'foo)))
