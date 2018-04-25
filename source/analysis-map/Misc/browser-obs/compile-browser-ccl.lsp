;;;
;;; Compiler for Browser : To compile the code, just load compile-browser
;;;

(let ((old-path-name *default-pathname-defaults*))

   (setf *default-pathname-defaults* 
         *z-directory*)
   
   (load 'records.fasl)
   (load 'Traps.fasl)

   (load 'graph-var.mac)
   (compile-file 'graph-var.mac)
   (load 'graph-spec.mac)
   (compile-file 'graph-spec.mac)
   (load 'scroll.mac)
   (compile-file  'scroll.mac)
   (load 'graph.lisp)
   (compile-file 'graph.lisp)
   (load 'Z-window.lisp)
   (compile-file 'Z-window.lisp)
   (load 'browser-pcl.lisp)
   (compile-file 'browser-pcl.lisp)
   (load 'browser-spec.mac)
   (compile-file 'browser-spec.mac)
   (load 'browser.lisp)
   (compile-file 'browser.lisp)
   (load 'menu.mac)
   (compile-file 'menu.mac)

   (setf *default-pathname-defaults* old-path-name))