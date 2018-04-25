(in-package 'pcl)

(defun missing-method (method &rest specializers)
  (let ((message-&-specializers
         (loop
           for s in specializers as i from 1 by 1
           with message = ""
           collect (class-name (class-of s))
           into name-list
           do
           (setf message
                 (concatenate 'string
                  message
                  " ~s"
                  (if (> i 5)
                     (progn 
                       (setf i 1)
                       " ~%"))
                  ))
           finally (return (cons message name-list)))))

    (apply #'quail-error 
        (concatenate 'string
            "The method ~S has not been specialized for the combination:~%"
            (car message-&-specializers))
         method
        (cdr message-&-specializers))))

(defmacro := (accessor new-value &rest args)
  (if args
    (append `(setf , accessor , new-value) args)
    `(setf , accessor , new-value)))

(defmacro flet* (&rest args)
  (cons 'labels args))

(defmacro mlet (&rest args)
  (cons 'macro-let args))