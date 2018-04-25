;;;
;;; Loader for the Browser
;;;

(let ((old-path-name *default-pathname-defaults*))

   (setf *default-pathname-defaults* *Z-directory*)

   (load 'records)
   (load 'traps)
   (load 'graph-var)
   (load 'graph-spec)
   (load 'scroll)
   (load 'graph)
   (load 'Z-window)
   (load 'browser-pcl)
   (load 'browser-spec)
   (load 'browser)
   (load 'menu)

   (setf *default-pathname-defaults* old-path-name))