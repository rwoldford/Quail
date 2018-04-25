;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; new-code-fragment.lsp
;;;
;;; derived from code-fragment.lsp
;;; and some attempts to simplify
;;; the building, creating, processing of dialogs
;;; notable through the separation of values
;;; which structure elements from their building
;;;
;;; Started 18 JAN 2005 GWB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; start with some bits from code-fragment
(defun how-many (char string)
   "Counts the number of occurrences of char in string"
    (- (length string)
      (length (remove-if #'(lambda (x)
                             (eq x char)) string))))

(defun list-of-lengths (string)
   ;; Assume string does *not* start with ~%
   ;; Gets a list of the stream-string-width of the
   ;; text bits of string
   (let ((result '())
         (fmat (list #\~ #\% #\Newline))
         (alphabet (list #\a #\b #\c #\d #\e #\f #\g #\h
                 #\i #\j #\k #\l #\m #\n #\o #\p #\q
                 #\r #\s #\t #\u #\v #\w #\x #\y #\z
                 #\A #\B #\C #\D #\E #\F #\G #\H
                 #\I #\J #\K #\L #\M #\N #\O #\P #\Q
                 #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
                 #\Space  #\. #\, #\; #\: #\( #\)))
         )
      (push 
       (let ((scrn (cg::screen cg::*system*)))
    (cg::with-device-context (hdc scrn)
       (cg::stream-string-width (cg::screen cg::*system*) string))) result)
      (do ((j 0 (+ j 1))
           (n (how-many #\Newline string)))
           ((= j n))
           (setf string (string-left-trim fmat 
                        (string-left-trim alphabet string)))
           (push 
         (let ((scrn (cg::screen cg::*system*)))
    (cg::with-device-context (hdc scrn)
         (cg::stream-string-width (cg::screen cg::*system*) string))) result)
      )
     result))

(defun max-text-segment (lengths) 
   " Gets the length of the maximum text segment~
   from a collection of cumulative lengths of a piece~
   of text assumed to have come from list-of-lengths."
   (let ((diffs '()))
      (do ((j 0 (+ j 1))
           (n (length lengths)))
          ((= j (- n 1)))
         (push (- (elt lengths j) (elt lengths (+ j 1))) diffs))
      (cond ((eq 1 (length lengths))
             (first lengths))
            (T (abs (first (sort diffs #'<))))
            )
     ))

;;; And now a form to set some of those structural pieces
(defun set-dialog-forms ()
  (let* ((displayed 5)
         (scroll-bar-width 25)
         (seq-height 200)
         (seq-width 200)
         (dialog-outer-left 10)
         (min-separator 15)
         (dialog-outer-top 10)
         (temp-font-info 
          (let ((scrn (cg::screen cg::*system*)))
            (cg::with-device-context (hdc scrn)
              (cg::fontmetrics (cg::screen cg::*system*)))))
         (text-height 
          (+ (cg::font-ascent temp-font-info) (cg::font-descent temp-font-info) 
             (cg::font-leading temp-font-info)))
         (button-height (+ 2 text-height))
         (prompt-box-height (+ 10 (* 1 text-height)))
         (text-width #'(lambda (string) 
                         (let ((scrn (cg::screen cg::*system*)))
                           (cg::with-device-context (hdc scrn)
                             (cg::stream-string-width (cg::screen cg::*system*) string)))))
         (button-width 
            (+ 5 
               (max (text-width select-text) 
                    (text-width cancel-text))))
         )
    (list text-width displayed scroll-bar-width seq-height seq-width dialog-outer-left
          dialog-outer-top min-separator temp-font-info text-height button-height
          prompt-box-height button-width)
    ))
;;; I *think* these are all the universal things I can deal with here.
;;; In this syntax I shall have to (funcall (car (set-dialog-forms) string-needing-width))
;;; If I write (twtmp (defun text-width (string)  ktl)
;;; then I can call text-width normally.

;;; Now I can rewrite build-multi-dialog-items