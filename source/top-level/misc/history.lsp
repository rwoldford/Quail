;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               history.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1991.
;;;
;;;
;;;----------------------------------------------------------------------------

(in-package :quail)

;;;-------------------------------------------------------------------------------
;;;
;;; an eval that takes a history
;;;
;;;-------------------------------------------------------------------------------

(defmacro history-eval (form &optional (history '*current-history*))
  (declare (special *current-history*))
  `(let ((result (eval , form))
         (event (make-instance 'history-event :form (quote , form))))
     (record-event , history event)
     result))

;;;--------------------------------------------------------------------------------
;;;
;;;  History structure
;;;
;;;--------------------------------------------------------------------------------


(defclass history ()
  ((contents :accessor contents-of)
   (size :initarg :size :initform 25 :reader size-of)
   (saved-events :initarg :saved-cmds :initform () :accessor saved-events-of)
   (current-line-no :initform 1 :accessor current-line-no-of)
   (current-line :initform 0 :initarg :current-line :accessor current-line-of)))


(defmethod initialize-instance :after ((self history) &key)
  (setf (contents-of self ) (make-list (size-of self)))
  self)


;;;;;;;;;;;;;
;;;
;;; Record an event on the history
;;;

(defmethod record-event ((self history) event)
  (setf (elt (contents-of self) (current-line-of self)) event)
  (setf (current-line-of self) (mod (+ (current-line-of self) 1) (size-of self)))
  (setf (current-line-no-of self) (+ (current-line-no-of self) 1)))

;;;;;;;;;;;;
;;;
;;; Change the size of the history
;;;

(defmethod (setf size-of) (new-value (self history))
  
  (if (not (and (integerp new-value) (> new-value 0)))
    (error "Size must be a positive integer.  Not ~s !" new-value)
    
    (labels
      ((insert-list (list at size)
          (append
            (subseq list 0 at)
            (make-list size)
            (subseq list at)))
       
       (delete-list (list at amount)
          (if (> at 0)
            (append
              (subseq list 0 at)
              (subseq list (+ at amount)))
            (subseq list amount))))
      
      (let ((num-events-to-add (- new-value (size-of self))))
        
        (cond
         ((> num-events-to-add 0)
          (setf (contents-of self)
                (insert-list (contents-of self)
                             (current-line-of self) 
                             num-events-to-add)))
         ((< num-events-to-add 0)
          (setf (contents-of self)
                (delete-list (contents-of self)
                             (current-line-of self)
                             (- num-events-to-add))))))
      (setf (slot-value self 'size) new-value))))



;;;--------------------------------------------------------------------------
;;;
;;;  History-events
;;;
;;;--------------------------------------------------------------------------

(defclass history-event ()
  ((form :reader form-of :initarg :form)
   (time-stamp :reader time-stamp-of :initform (get-universal-time))
   (creator :accessor creator-of :initform 'User)))



;;;;;;;;
;;;
;;; Some silly methods for interacting with the time-stamp of an event
;;;

(defmethod date-of ((self history-event))
  (multiple-value-bind 
    (second minute hour day month year day-of-week)
    (decode-universal-time (time-stamp-of self))
    (setf month
          (case month
            (1 "January")
            (2 "February")
            (3 "March")
            (4 "April")
            (5 "May")
            (6 "June")
            (7 "July")
            (8 "August")
            (9 "September")
            (10 "October")
            (11 "November")
            (12 "December")))
    (setf day-of-week
          (case day-of-week
            (0 "Monday")
            (1 "Tuesday")
            (2 "Wednesday")
            (3 "Thursday")
            (4 "Friday")
            (5 "Saturday")
            (6 "Sunday")))
    
    (concatenate
     'string
     (format NIL
            "~S:~S:~S "
            hour minute second)
     day-of-week
     " "
     month
     (format NIL " ~S ~S" day year))))
;;;--------------------------------------------------------------------------
;;;
;;;  Redo selected event
;;;
;;;--------------------------------------------------------------------------


#| Fancy redo below is not finished

(defun redo (&rest args)
  (cond
   ((null args)
    (redo-event (get-last-history-event)))
   ((= 1 (length args))
    (redo-event (car args)))
   (T
    (let* ((history (position :history args))
           (from (position :from args))
           (to (position :to args))
           (with (position :with args))
           (for (position :for args)))
      (cond
       ((and with for)
        "replacing")
       ((and from to)
        "range")
       (from "to current-line less one")
       )
      )
    )
   )
 )

|#

;;; Instead the following one works:

(defun redo (&rest args)
  (apply #'redo-event args))

(defmethod redo-event ((self history-event) &key)
  (eval `(history-eval , (form-of self))))

(defmethod redo-event ((self t)
                 &key (history *current-history*))
  (declare (special *current-history*))
  (redo-event (get-event history self)))

(defmethod get-last-history-event
  (&optional (history *current-history*))
  (get-event history (- (current-line-no-of history) 1)))

(defmethod get-event ((self history) (line-no integer))
  (let ((c-l-n (current-line-no-of self)))
    (cond
     
      ((<= line-no 0)
       (cond
        ((< (size-of self) (abs line-no))
         (print (format nil "Sorry, event numbered ~a has dropped off the history."
                        line-no))
         (values))
        (T
         (elt (contents-of self)
            (mod (+ (current-line-of self) line-no)
                 (size-of self))))))
      
      ((>= line-no c-l-n)
       (print (format nil "Redo? I haven't even done that one yet!~
                          ~%Current line is ~A, you asked for ~A"
                      c-l-n
                      line-no))
       (values))
      
      ((< (size-of self) (- c-l-n line-no))
       (print (format nil "Sorry, event numbered ~a has dropped off the history."
                      line-no))
       (values))
      
      (T
       (elt (contents-of self)
            (mod (+ (current-line-of self) (- line-no c-l-n))
                 (size-of self)))))))

(defmethod get-event ((self history) (symbol symbol))
  (let* ((current-line (current-line-of self))
         (one-piece (= 0 current-line))
         (contents (contents-of self))
         the-event)
    (labels
      ((test-event (event)
          (member symbol (form-of event))))
      (setf the-event
          (if one-piece
            (find-if #'test-event contents :from-end T)
            (or (find-if #'test-event contents
                         :from-end T
                         :start 0 :end (- current-line 1))
                (find-if #'test-event contents
                         :from-end T
                         :start current-line))))
      the-event)))
                 
            

;;;--------------------------------------------------------------------------
;;;
;;;  Permanent events that don't drop off the history stack
;;;  are stored on the slot saved-events.
;;;  the following functions are used to interact with this slot.
;;;
;;;--------------------------------------------------------------------------


(defmethod save-event ((self history) event)
  (setf (saved-events-of self) (push event (saved-events-of self)))
  event)

(defmethod rm-saved-event ((self history) event)
  (setf (saved-events-of self)
        (remove event (saved-events-of self)))
  event)
