
(in-package :quail-kernel)

(setf a (z::array '(3) :initial-contents '(0 1 2)))

(defclass foo (quail-object quail-open-mixin) ())

(defmethod quail-open-object ((self foo))
  )

(defmethod quail-close-preserve-object ((self foo))
  )

(defmethod quail-close-destroy-object ((self foo))
  )

(defmethod quail-close-physically-object ((self foo))
  )

(setf b (make-instance 'foo))

(defun foop (x) (typep x 'foo))

(pprint (macroexpand '(with-open-quail-objects (a b :temp c d) #'foop
                        (setf d (make-instance 'foo))
                        (quail-print (setf c (tp a)))
                        (describe b)
                        14)))

(with-open-quail-objects (a b :temp c d) #'foop
  (setf d (make-instance 'foo))
  (quail-print (setf c (tp a)))
  (describe b)
  14)

(pprint (macroexpand
  '(with-open-quail-objects (x :temp z) #'foop
     (Q+ x (setf z (Q* 3 y))))))

(defun try-it (x y)
  (with-open-quail-objects (x :temp z) #'foop
    (Q+ x (setf z (Q* 3 y)))))

(pprint (macroexpand
  '(with-open-quail-objects (self :keep result) #'quail-pred1
     (with-slots (stream) self
       (file-position stream index)
       (setf result (read stream))
       result))))

(pprint (macroexpand '(with-open-quail-objects nil nil nil)))

(pprint (macroexpand '(with-open-quail-objects (a :list b c :temp d) #'pred (body))))