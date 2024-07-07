;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                              top-level.lisp   
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     M.E. Lewis 1991.
;;;     R.W. Oldford 1991.
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(*legal-top-level-loop-functions*
          install-legal-top-level-loop top-level-loop-p
          ? *pending-top-levels* pop-level pop-to-top-level push-level
          *pending-packages* pop-package push-package
          eval-print quail-loop quail increment-quail-level
          decrement-quail-level reset-quail-level quail-running-p quit-lisp
          add-quail-command delete-quail-command)))

#|
;;;----------------------------------------------------------------------
;;; 
;;;  Make the toplevel quail available in the cl package.
;;;
;;;----------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute) (import 'quail:quail :cl))
(eval-when (:compile-toplevel :load-toplevel :execute) (export 'quail :cl))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Defining the legal top-level loop functions.
;;;

(defvar *legal-top-level-loop-functions* nil
  "Variable containing a list of the legal top-level loop functions." )

(defun setup-legal-top-level-loop-functions ()
  (declare (special *system-top-level-loop-function*
                    *legal-top-level-loop-functions*))
  (setf *legal-top-level-loop-functions* 
        (list *system-top-level-loop-function*)))

(eval-when (load eval)
  (setup-legal-top-level-loop-functions))

(defun top-level-loop-p (fun)
  "T if the function is a legal top-level loop function,
   NIL otherwise."
  (declare (special *legal-top-level-loop-functions*))
  (find fun *legal-top-level-loop-functions*))


(defun install-legal-top-level-loop (fun)
  "Installs the argument as a legal top-level loop function.
   Need only be done once."
  (declare (special *legal-top-level-loop-functions*))
  (if (not (top-level-loop-p fun))
    (push fun *legal-top-level-loop-functions*)))

;;;;;;;;;;;
;;;
;;;
;;;  Pushing and popping levels in quail's top-level loop.
;;;

(defvar *pending-top-levels* nil
  "Variable containing the stack of top-level loop functions." )

(defun pop-level (&aux next-level)
  "Pops the next pending level off the stack of top-level loop functions."
  (declare
   (special *pending-top-levels*
            *system-top-level-loop-function*
            *quail-terminal-io*))
  (setq next-level (or (top-level-loop-p (pop *pending-top-levels*))
                       *system-top-level-loop-function*))
  (format *quail-terminal-io* "~&popping levels from ~s to ~s."
          (get-function-name (current-top-level))
          (get-function-name next-level))
  (install-top-level next-level))



(defun pop-to-top-level ()
  "Pops the next pending levels off the stack of top-level loop functions
   until the system top-level loop function is reached."
  (declare
   (special *pending-top-levels*
            *pending-packages*
            *system-top-level-loop-function*
            *quail-terminal-io*))

  (setf *pending-top-levels* NIL)
  
  (format *quail-terminal-io* "~&popping levels from ~s to ~s."
          (get-function-name (current-top-level))
          (get-function-name *system-top-level-loop-function*))

  (let ((top-package (last *pending-packages*)))
    (set-listener-package (package-name (first top-package)))
    (setf *pending-packages* NIL))
  
  (install-top-level *system-top-level-loop-function*))



(defun push-level (top-level-fn)
  "Pushes the given pending level onto the stack of top-level loop functions."
  (declare
   (special *pending-top-levels*))
  (push top-level-fn *pending-top-levels*))


;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Ensure that the quail-loop is running in quail-user when first
;;;  installed.
;;;

(defvar *pending-packages* nil
  "Variable containing the stack of listener in-packages.")

(defun set-listener-package (package)
  "Sets the listener's package to package."
  (eval `(eval (in-package ,package))))

(defun pop-package (&aux next-package)
  "Pops the next pending package off the stack of listener in-packages."
  (declare
   (special *pending-packages*))
  (setq next-package (pop *pending-packages*))
  (set-listener-package (package-name next-package)))

(defun push-package (package)
  "Pushes the given pending package onto the stack of listener in-packages."
  (declare 
   (special *pending-packages*))
  (push package *pending-packages*))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Common Lisp variables
;;;  that should behave equivalently in the
;;;  quail loop.
;;;
;;;

(defvar +    nil "See Common Lisp documentation.")

(defvar ++   nil "See Common Lisp documentation.")

(defvar +++  nil "See Common Lisp documentation.")

(defvar *    nil "See Common Lisp documentation.")

(defvar **   nil "See Common Lisp documentation.")

(defvar ***  nil "See Common Lisp documentation.")

(defvar /    nil "See Common Lisp documentation.")

(defvar //   nil "See Common Lisp documentation.")

(defvar ///  nil "See Common Lisp documentation.")

(defvar -    nil "See Common Lisp documentation.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Monitor the number of levels of quail's top-level loop are running.
;;;

(defvar *quail-levels* 0
  "Variable containing the number of quail top-level loop functions pending." )

(defun quail-running-p ()
  "True if a quail top-level loop is pending, nil otherwise."
  (if (> *quail-levels* 0) T))

(defun increment-quail-level ()
  "Increments the counter of quail top-level loop functions pending."
  (declare (special *quail-levels*))
  (incf *quail-levels* 1))

(defun decrement-quail-level ()
  "Decrements the counter of quail top-level loop functions pending."
  (declare (special *quail-levels*))
  (if (quail-running-p) (decf *quail-levels* 1)))

(defun reset-quail-level ()
  "Resets the counter of quail top-level loop functions pending to
   its original value."
  (declare (special *quail-levels*))
  (setf *quail-levels* 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Establish Quail top-level loop commands
;;;

(defvar *quail-top-level-commands* NIL
  "A hash table containing the commands that are special to the
   quail top-level loop.")


(defun pop-quail ()
  "Pops one level of quail."
  (decrement-quail-level)
  (pop-package)
  (pop-level))



(defun exit-quail ()
  "Exits from the present and all pending Quail top-level loops."
  (declare (special *quail-terminal-io*))
  (reset-quail-level)
  (format *quail-terminal-io* "~&Bye ~%")
  (pop-to-top-level))



(defun quit-lisp ()
  "Politely quits lisp."
  (when (quail-yes-or-no-p "Really quit LISP?")
    (if (quail-yes-or-no-p "Save your quail objects?")
      (save-quail-objects))
    (system-quit-lisp)))

;;;;;;;;;;;;;;;;;
;;;
;;;  Making infix available
;;;

(defvar *read-as-infix* NIL)

(defun infix ()
  (declare (special *read-as-infix*
                    *extra-prompt-info*))
  (push "INFIX" *extra-prompt-info*)
  (setf *read-as-infix* T))

(defun prefix ()
  (declare (special *read-as-infix*
                    *extra-prompt-info*))
  (setf *extra-prompt-info* 
        (loop for s in *extra-prompt-info* when (not (string-equal s "INFIX"))
              collect s))
  (setf *read-as-infix* NIL))

(defun infix-p ()
  (declare (special *read-as-infix*))
  *read-as-infix*)




(defun show-quail-commands ()
  "Shows the non-LISP commands understood by the Quail top-level loop."
  (declare (special *quail-top-level-commands*
                    *quail-terminal-io*))
  (format *quail-terminal-io*
          "~&~%
           Besides any legal LISP expression, Quail provides a collection of ~%
           top-level commands.  These are usually keywords. ~%
           Currently available quail top-level commands are: ~2%"
          )
  (maphash
   #'(lambda (key value)
       (declare (special *quail-terminal-io*))
       (fresh-line *quail-terminal-io*)
       (format *quail-terminal-io*
               "~&~15@<~s~>~A" key (second value)))
   *quail-top-level-commands*)
  (fresh-line *quail-terminal-io*)
  (terpri *quail-terminal-io*)
  (values))



(defun quail-command-p (command-name)
  "Returns true if the argument names a Quail top-level command, NIL otherwise."
  (declare (special *quail-top-level-commands*))
  (multiple-value-bind (command-info found?)
                       (gethash command-name *quail-top-level-commands*)
    (declare (ignore command-info))
    found?))



(defun add-quail-command
       (command-name command-fn
        &optional (doc-string "No documentation available."))
  
  "Adds a command to Quail's set of top-level commands.
   First argument is a symbol representing the command name
   (typically a keyword symbol); second is a function of no arguments
   to be funcalled when the command is executed.
   Optionally, a documentation string may be given."
  
  (declare (special *quail-top-level-commands*))
  (if (quail-command-p command-name)
    (if (quail-yes-or-no-p "The command ~s already exists.  ~%
                            Replace it anyway?" command-name)
      (setf (gethash command-name *quail-top-level-commands*)
            (list command-fn doc-string)))
    (setf (gethash command-name *quail-top-level-commands*)
          (list command-fn doc-string))))


(defun delete-quail-command (command-name)
  "Removes the Quail top-level command named by the argument.
   Returns T if command was present, NIL otherwise."
  (declare (special *quail-top-level-commands*))
  (remhash command-name *quail-top-level-commands*))


(defun do-quail-command (command-name)
  "Executes the named top-level Quail command if it exists.
   Error otherwise."
  (declare (special *quail-top-level-commands*))
  (multiple-value-bind (command-fn-info exists?)
                       (gethash command-name *quail-top-level-commands*)
    (if exists?
      (funcall (car command-fn-info))
      (quail-error
       "~&Sorry, ~s is not a top-level Quail command." command-name))))
  


(defun setup-quail-top-level-commands ()
  "Sets up Quail's basic set of top-level commands."
  (declare (special *quail-top-level-commands*))
  (setf *quail-top-level-commands*
        (make-hash-table :size 10))
  (add-quail-command   '?         #'show-quail-commands
                       "Displays all available Quail top-level commands.")
  (add-quail-command :infix
                     #'infix "Read ALL top-level expressions in infix notation.")
  (add-quail-command :prefix
                     #'prefix
                     "Read ALL top-level expressions in the usual prefix notation.")
  (add-quail-command   :pop       #'pop-quail
                       "Pop to the next pending top-level loop.")
  (add-quail-command   :exit      #'exit-quail
                       "Exit completely from Quail.")
  (add-quail-command   :quit      #'exit-quail
                       "Exit completely from Quail.")
  (add-quail-command   :bye       #'exit-quail
                       "Exit completely from Quail.")
  (add-quail-command   :quit!     #'quit-lisp
                       "Quit from LISP.")
  (add-quail-command   :help      #'(lambda ()
                                      (show-quail-commands)
                                      (help))
                       "Invokes the general help facility.")
  (add-quail-command   :topics  #'(lambda ()
                                         ;;(ensure-loaded-topics :package :quail)
                                         (help 'Quail :topic)
                                         ;;(delete-quail-command :load-topics)
                                         )
                       "General information on quail organized by topic.")
  (add-quail-command   'help
                       #'(lambda ()
                           (format *quail-terminal-io*
                                   "With the exception of ? all quail toplevel commands
                                    are keyword symbols.  ~&
                                    That is, the first character is a colon.~&
                                    If you would like general help information type :help.  ~&
                                    If you would only like to see the set of available quail toplevel commands
                                    type ?.  ~%
                                    N.B. This message will appear only once.  ~&
                                    From now on you must use :help or ?.")
                           (delete-quail-command 'help)
                           (show-quail-commands))
                       "A one shot look at the available quail-commands.")
  )

(eval-when (eval load) (setup-quail-top-level-commands))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Evaluating and printing in the quail loop
;;;
;;;


#|  ;;The old one.
(defun eval-print (&optional (stream *quail-terminal-io*)
                             &aux result)
  "Evaluates and prints the form supplied.  Output to stream, ~
   if supplied, otherwise to *quail-terminal-io*."
  (declare
   (special *quail-terminal-io*
            + ++ +++ * ** *** / // /// -))
  (setf result (multiple-value-list (eval -)))
  (setf +++ ++)
  (setf ++ +)
  (setf + -)
  (setf *** **)
  (setf ** *)
  (setf /// //)
  (setf // /)
  (setf / (list (setf * (print (car result) stream))))
  (if (> (length result) 1)
    (setf / (append /
                    (loop for v in (cdr result) collect (print v stream)))))
  *
  )
|#

(defun eval-print (&optional (stream *quail-terminal-io*)
                             &aux result)
  "Evaluates and prints the form supplied.  Output to stream,
   if supplied, otherwise to *quail-terminal-io*."
  (declare
   (special *quail-terminal-io*
            + ++ +++ * ** *** / // /// -))
  (setf result (multiple-value-list
                (eval
                 (if (infix-p)
                   (setf -
                         (infix2prefix -))
                   -)))
        )
  (setf +++ ++)
  (setf ++ +)
  (setf + -)
  (setf *** **)
  (setf ** *)
  (setf /// //)
  (setf // /)
  (setf / (list (setf * (format stream "~&~a" (car result)))))
  (if (> (length result) 1)
    (setf / (append /
                    (loop for v in (cdr result)
                          collect 
                          (format stream "~&~a" (car result))))))
  *
  )


(defun quail-top-level-eval-current-form ()
  "Evaluates and prints the value of the current form (the value of the
   - symbol).  Accepts quail commands as well."
  (declare (special -))
  (if (quail-command-p -)
    (do-quail-command -)
    (eval-print)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;     Quail top-level loop
;;;
(defun quail-read ()
  "Read the next Quail expression."
  (quail:get-next-form))

(defun quail-loop (&aux prompt-string)
  "The Quail top-level loop function."
  (declare (special - *quail-levels*))
  (fresh-line)
  (setf prompt-string
        (concatenate 'string
                     "Quail"
                     (extra-prompt-info)
                     (if (> *quail-levels* 1)
                        (format nil "(~s)" (- *quail-levels* 1))
                        (values))
                     "> "))
  (princ prompt-string)
  (setf - (quail-read))
  (quail-top-level-eval-current-form)
  )

(defvar *extra-prompt-info* NIL)

(defun extra-prompt-info ()
  (declare (special *extra-prompt-info*))
  (if *extra-prompt-info*
    (apply #'concatenate 'string "{" (append *extra-prompt-info* (list "}")))
    (values)))

(defun quail::setup-legal-quail-loop ()
  (install-legal-top-level-loop #'quail-loop))

(eval-when (eval load)
  (setup-legal-quail-loop))

(defun quail ()
  "Starts the Quail top-level loop."
  (declare (special *package* *quail-terminal-io* *quail-levels*))
  (if (zerop *quail-levels*)
    (format *quail-terminal-io*
            "~%Welcome to ~a! ~%
             Several keyword commands are available at Quail's top-level loop.  ~&
             To see these, type ? at the Quail prompt.  ~&
             If you exit Quail, you can always reinvoke it by typing (quail). ~%"
            (qk::quail-release)))
  (increment-quail-level)
  (push-level (current-top-level))
  (push-package *package*)
  (set-listener-package :quail-user)
  (install-top-level #'quail-loop)
  )

