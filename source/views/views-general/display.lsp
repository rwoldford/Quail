;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  
;;;                               display.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  
;;;
;;;  Authors:
;;;     R.W. Oldford 1993, 1995, 1996
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(display signposts display-viewed-object)))


(defclass displayable-mixin ()
  ((displayed-view :accessor displayed-view-of :initform NIL
                 :documentation
                 "Slot to cache the display view of a quail-object.")
   (displayed-args :accessor displayed-args-of :initform NIL
                 :documentation
                 "Slot to cache the arguments used to ~
                  create the cached display."))
  (:documentation "A class used to cache the display view of any quail-object."))





(eval-when (:execute :load-toplevel) ;(eval load) 10MAR2022 gwb
  (qk::add-mixin-to-quail-object 'displayable-mixin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;   DISPLAY
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric display (thing &rest keyword-args &key draw? &allow-other-keys)
  (:documentation
   "Produces a display of thing.  Keyword arguments are specialized by methods. ~
    These will include at least view style arguments."))


(defun display-viewed-object (view &rest keyword-args)
  "A function that will call display on the viewed-object of the given ~
   view.  Keword-args are passed on to the display method for that viewed-object."
  (apply #'display (viewed-object-of view) keyword-args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;   SIGNPOSTS
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric signposts (quail-object &rest keyword-args &key &allow-other-keys)
  (:documentation
   "Returns a single view or list of views (typically control buttons) ~
    that will act as signposts to new analysis steps from a display of this ~
    quail-object."))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;   Caching the displayed view
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod display :around
           ((thing displayable-mixin)
            &rest keyword-args
            &key (viewport NIL)
            (draw? T) (new? NIL)
            &allow-other-keys)
  "Caches the displayed view and re-uses it when it exists."
  (if (or new?
          (null (displayed-view-of thing))
          (not (equal keyword-args (displayed-args-of thing))))
    (let ((result (call-next-method)))
      (setf (displayed-view-of thing) result)
      (setf (displayed-args-of thing) keyword-args)
      result)
    (let ((result (displayed-view-of thing)))
      (if draw?
        (if viewport 
          (apply #'draw-view result :viewport viewport keyword-args)
          (apply #'draw-view result :viewport (or viewport (make-viewport)) keyword-args)
          ))
      result)
    )
  )
