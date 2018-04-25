;;;
;;; loader for Z
;;;

(let ((old-path-name *default-pathname-defaults*))

   (setq *default-pathname-defaults*
         *Z-directory*)

   (load 'Z-functions)
   (load 'Z-utility)
   (load 'Z-icon)
   (load 'Z-mixins)
   (load 'Z)
   (load 'ref-arrays)
   (load 'Z-browser)
   (load 'network-view)
   (load 'micro-view)
   (load 'analysis-network)
   (load 'toolbox-network)
   (load 'analysis-path)
   (load 'extended-ops)
   (load 'Z-binops)
   (load 'Z-reexps)
   (load 'junk-extract)
   (load 'Z-methods)
 
   ;; This line should be useless, but it seems there is a problem with pcl, and
   ;; without it, the after method of initialize-instance defined in pcl is lost
   ;; when another after method of initialize-instance is defined in Z-mixins
   
   (load 'menu)


   (setq *default-pathname-defaults* old-path-name))

;;;
;;; define a function to display a browser of the Z classes
;;;

(in-package 'Z)

(defun class-browse-Z (&aux browser)

   (browse (setq browser (make-instance 'browser::browser))
           (mapcar (function find-class) 
                   '(find-where-mixin
                     tool-box-link-mixin
                     body-menu-mixin
                     title-bar-mixin
                     named-object-mixin
                     prompt-mixin
                     documented-object-mixin
                     indexed-object-mixin
                     linked-object-mixin
                     initable-object-mixin)))
   (recompute browser))

;;;
;;; display the Z classes
;;;

(class-browse-Z)

