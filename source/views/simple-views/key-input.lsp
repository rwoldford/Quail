;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               key-input.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is part of the Views statistical graphics package.
;;;  
;;;
;;;  Authors:
;;;     R.W. Oldford 1996
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)

;;; Now handled in the package def of VIews (import '(wb::handle-key-event))

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(handle-key-event key-input-view)))

(defclass key-input-view (view)
  ((active-input? :initform NIL :initarg :active?
                  :accessor input-mode-p
                  :documentation "Flag to indicate whether the view ~
                                  is accepting input.")
   (input-positions :initform NIL
                   :accessor input-positions
                   :documentation
                   "The present entry position of the incoming events, by viewport.")
   )
  )


;;;
;;;  Generic methods for starting, stopping, and directing input.
;;;

(defgeneric start-input (key-input-view viewport)
  (:documentation
   "Initializes view to accept keyboard input."))

(defgeneric stop-input (key-input-view viewport)
  (:documentation
   "Stops view from accepting further input."))

(defgeneric refocus-input (key-input-view viewport &optional position)
  (:documentation
   "Moves the focus of the input to a new location determined from the ~
    given position."))

(defgeneric end-event-input-p (key-input-view event)
  (:documentation
   "Test whether the given event should trigger an end to keyboard input."))


;;;
;;;  Interaction begins with the left button
;;;

(defmethod left-button-fn ((self key-input-view) &key viewport )
  "Left button activates or deactivates the view for keyboard input."
  ;;(call-next-method)
  (cond
   ((and (eq self (selected-view)) (input-mode-p self))
    ;; refocus of input
    (refocus-input self viewport (wb::mouse-position (window-of viewport)))
    )
   ((eq self (selected-view))
    (cond
     ((quail-y-or-n-p "Edit input?")
      (start-input self viewport))
     (T (call-next-method)))
    )
   ((input-mode-p self)
    (stop-input self viewport)
    )
   (T 
    (call-next-method)
    (start-input self viewport))))

;;; end input by handle-key-event  when enter is hit.


;;;
;;;  The methods
;;;

(defmethod start-input ((self key-input-view) viewport)
  "Does nothing"
  (declare (ignorable self viewport)) ;(declare (ignore self viewport)) ; 29JUL2023
  )

(defmethod start-input :before ((self key-input-view) viewport)
  "Ensures that the input mode is turned on before anything else happens."
  (declare (ignorable viewport)) ;(declare (ignore viewport)) ; 29JUL2023
  (setf (input-mode-p self) T))

(defmethod stop-input ((self key-input-view) viewport)
  "Does nothing"
  (declare (ignorable self viewport)) ;(declare (ignore self viewport)) ; 29JUL2023
  NIL
  )

(defmethod stop-input :after ((self key-input-view) viewport)
  "Ensures that the input mode is turned off after all else is done."
  (declare (ignorable viewport)) ;(declare (ignore viewport)) ; 29JUL2023
  (setf (input-mode-p self) NIL))

(defmethod refocus-input ((self key-input-view) viewport
                          &optional position)
   "Moves the focus of the input to a new location determined from the ~
    given position."
   (if (assoc viewport (input-positions self))
     (setf (cdr (assoc viewport (input-positions self)))
           position)
     (push (cons viewport position) (input-positions self))))

(defmethod end-event-input-p ((view key-input-view) (event T))
  (declare (ignorable view event)) ;(declare (ignore view event)) ; 29JUL2023
  NIL)

(defmethod end-event-input-p
           ((view key-input-view) (event (eql wb:+escape-key-event+))) ;(eql wb:*escape-key-event*))) 23FEB2022 gwb
  (declare (ignorable view event)) ;(declare (ignore view event)) ;29JUL2023
  T)


(defmethod handle-key-event ((view key-input-view) event)
  "Doesn't do much.  Just informs user of the events received."
  (inform-user 
   (format NIL
           "~&~%Handle-key-event -- View = ~s ~%~20T Event = ~s.~%"
           view event)
   )
  )

(defmethod handle-key-event ((view view) event)
  "Hands off to selected view."
  (let ((selected-view (selected-view)))
    (unless (eq view selected-view)
      (handle-key-event selected-view event))))

(defmethod handle-key-event :around ((view key-input-view) event)
  "Checks to see if input is to be ended."
  (cond
   ((end-event-input-p view event)
    (if (eq (selected-view) view) (select-one view))
    (stop-input view NIL)
    )
   ((eq (selected-view) view)
    (call-next-method))
   )
  )

#|
(setf test (make-instance 'key-input-view))
(draw-view test)
(inspect test)
|#
