
(unless (quail-y-or-n-p "Current value of *Z-directory* is ~s ~%~
                   Is this correct?"
                  (if (boundp '*Z-directory*)
                      *Z-directory*
                      (setf *Z-directory* NIL)))
  (setf *Z-directory*
        (progn
          (write-line
           (format NIL "~%Enter the new value of *Z-directory*") *quail-query-io*)
          (read-line *quail-query-io*))))

(let ((old-path-name *default-pathname-defaults*))

   (setf *default-pathname-defaults* *Z-directory*)
   (load 'z-load-browser)
   (load 'z-load-base)
   (setf *default-pathname-defaults* old-path-name))
   
(in-package 'Z)
(init-list-of-icons)
      