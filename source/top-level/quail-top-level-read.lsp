
(in-package :quail)


(defvar *forms-awaiting-evaluation* nil
  "Remainder of last line read.")

(defun quail-toplevel-reader-function (&optional (stream *standard-input*))
  "Reads a line of input from stream and returns the first ~
   form to be evaluated.   Stores any remaining forms in the ~
   special variable *forms-awaiting-evaluation*.  ~
   Created for use in the Quail toplevel loop."
  (declare (special *forms-awaiting-evaluation*))
  (flet ((num-unmatched-left-parens (string)
           (let ((count 0))
             (dotimes (i (length string) count)
               (let ((string-elt-i (elt string i)))
                 (cond
                  ((string-equal  string-elt-i  "(")
                   (incf count))
                  ((string-equal string-elt-i ")")
                   (decf count))
                  (t nil)))))))
    (let ((input-line
           (do* ((next-line (read-line stream nil nil)
                            (read-line stream nil nil))
                 (input-line next-line
                             (concatenate 'string
                                          input-line
                                          next-line))
                 (count (num-unmatched-left-parens next-line)
                        (incf count (num-unmatched-left-parens next-line))))
                ((= count 0) input-line))))
      (with-input-from-string (str input-line)
        (let ((first-form (read str nil nil)))
          (if (listen str)
            (do ((next-form (read str nil nil)
                            (read str nil nil))
                 (rest-of-forms nil))
                ((null next-form) 
                 (if rest-of-forms
                   (setq *forms-awaiting-evaluation*
                         (append *forms-awaiting-evaluation*
                                 (nreverse rest-of-forms)))))
              (push next-form rest-of-forms)))
          first-form)))))

#|
(defun quail-loop (&aux prompt-string)
  "The Quail top-level loop function."
  (declare (special -))
  (fresh-line)
  (setf prompt-string
        (concatenate 'string
                     "Quail"
                     "> "))
  (princ prompt-string)
  (setf - (quail-read))
  (quail-top-level-eval-current-form)
  )

(defun quail-read ()
  "Read the next Quail expression."
  (get-next-form))

(defun get-next-form ()
  "Retrieves the next form to be evaluated."
  (declare (special *forms-awaiting-evaluation*))
  (or (ccl:get-next-queued-form) 
      (pop *forms-awaiting-evaluation*)
      (quail-toplevel-reader-function)))

(defun quail-top-level-eval-current-form ()
  "Evaluates and prints the value of the current form (the value of the ~
   - symbol)."
  (declare (special -))
  (eval-print))

(defun eval-print (&optional (stream *terminal-io*)
                             &aux result)
  "Evaluates and prints the form supplied.  Output to stream, ~
   if supplied, otherwise to *terminal-io*."
  (declare
   (special *terminal-io*
            + ++ +++ * ** *** / // /// -))
  (setf result (multiple-value-list
                (eval -))
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
                          (format stream "~&~a" v)))))
  *
  )

(defun quail ()
  (ccl::%set-toplevel #'quail-loop)
  (ccl:toplevel))

(defun exit ()
  (ccl::%set-toplevel #'ccl:toplevel-loop)
  (ccl:toplevel))
|#
