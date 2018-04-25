(in-package :q-user)

(import 'wb::doit)

(defun ps () mp:*all-processes*)

(defun undoit (&optional (kill? T))
   (if kill? (mp::process-kill (first (ps))))
   (setf wb::*default-display* (xlib::close-display wb::*default-display*))
   (setf q::*quail-menubar* NIL)
 )

(defun c () (wb::make-canvas))

(defun d () (setf d (array '(1 2 3 4) :dimensions '(2 2))))

(defun s () (scatterplot :data d :x 0 :y 1))
