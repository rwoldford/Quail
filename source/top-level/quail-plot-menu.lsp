;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          quail-plot-menu.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 
;;; Copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1992.
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(*quail-plot-menu-items* add-to-plot-menu delete-from-plot-menu)))


(defvar *quail-plot-menu-items*
  NIL
  "The menu items to appear in the default plot menu.")


(setq *quail-plot-menu-items* (vw:plot-menu-items))

(defmethod vw::single-plot-by-dim :around ((dim number)
                               &key
                               plot-fn 
                               dataset  cases 
                               vars  args)
  (declare (ignore dataset cases))
  (if (>= dim 3) 
    (let ((x (or (first vars) :prompt))
          (y (or (second vars) :prompt))
          (z (or (third vars) :prompt))
          (v (or vars :prompt))
          (items (append (if (= dim 3) '("Rotating Plot"))
                         (list "Scatterplot Matrix" "Plot matrix" "Side by side"
                               "Andrews' Trace" "Tukey's Trace"))))
      (setq plot-fn (or plot-fn (car (wb:prompt-for-items items))))
      (cond ((functionp plot-fn) (apply plot-fn  :vars v args))
            ((not (stringp plot-fn)) nil)
            ((string-equal plot-fn "Scatterplot Matrix")
             (apply #'scat-mat  :vars v args))
            ((string-equal plot-fn "Plot Matrix")
             (apply #'scat-mat :pairs-view :prompt :vars v args))
            ((string-equal plot-fn "Rotating Plot")
             (apply #'rotating-plot  :x x :y y :z z args))
            ((string-equal plot-fn "Rotating Lines")
               (apply #'rotating-lines-plot  :x x :y y :z z args))
            ((string-equal plot-fn "Andrews' Trace")
             (apply #'prompt-projection-trace
                    :andrews :vars v  args))
            ((string-equal plot-fn "Tukey's Trace")
             (apply #'prompt-projection-trace :tukey :vars v  args))
            ((string-equal plot-fn "Side by side")
             (if (wb:prompt-true-or-false "Common scale?")
               
               (apply #'1d-layout-plot   :vars v args )
               (apply #'1d-layout-plot :left-view nil :link-bounds-x? nil
                      :bottom-view nil :link-bounds-y? nil :vars v args )))
            
            (t nil))
      )
    (call-next-method)))


(defun quail-plot-menu ()
  "Creates and returns the default plot menu for the Quail menubar."
  (wb:make-menu
   :items 
   *quail-plot-menu-items*
   :title "Plots" 
   :menu-type :title
   ))

(defun add-to-plot-menu (item &rest other-items)
  "Adds one or more items to the list of items in the ~
   quail-plot-menu-items. Returns the new plot-menu-items list. ~
   Destructive to *quail-plot-menu-items* ~
   (:required (:arg item The menu item list to be added.) )~
   (:rest (:arg other-items NIL Other item lists to be added.)) ~
   (:see-also wb:make-menu delete-from-plot-menu *quail-plot-menu-items* ~
   remake-quail-menubar) ~
   "
  (declare (special *quail-plot-menu-items*))
  (nconc *quail-plot-menu-items* (list item) other-items)
  *quail-plot-menu-items*)


(defun delete-from-plot-menu (item &rest other-items)
  "Deletes one or more items from the list of items in the ~
   quail-plot-menu-items. Returns the new menu-items list. ~
   Destructive to *quail-plot-menu-items* ~
   (:required (:arg item The menu item list to be deleted.) )~
   (:rest (:arg other-items NIL Other item lists to be deleted.)) ~
   (:see-also wb:make-menu add-to-quail-menu *quail-plot-menu-items* ~
   remake-quail-menubar) ~
   "
  (declare (special *quail-plot-menu-items*))
  (delete item *quail-plot-menu-items* :test #'equal)
  (when other-items
    (dolist (item other-items)
      (delete item *quail-plot-menu-items* :test #'equal)))
  *quail-plot-menu-items*)
