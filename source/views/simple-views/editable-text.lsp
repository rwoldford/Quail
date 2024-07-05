;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               editable-text.lsp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(editable-text-view)))


(defclass editable-text-view (text-view key-input-view)
  ((between-words? :initform NIL :accessor break-between-words
                :documentation
                "A flag for draw-view to indicate how overflow lines should ~
                 break when displayed.  If non-NIL then the display will break ~
                 the line just before the overflowing word, otherwise just ~
                 before the overflowing character.  Default is NIL.")
   (pos-&-chars :initform NIL :accessor character-positions
                :documentation
                "An association list indexed by viewport.  Each value is ~
                 a list of rows ~
                 where each row is itself a list the first element ~
                 of which is the y-position ~
                 and whose remaining elements are the ~
                 (position string-index character) triples for each character in ~
                 the text string.  The list is in the reverse order to the ~
                 order of the characters in the display.~%~
                 It is constructed whenever the view is drawn.")
   (input-color :initform wb:*black-color*
                     :accessor input-color
                     :documentation
                     "The colour to be used as the drawing colour for the ~
                      editable text when input is active.")
   (input-back-color :initform *default-highlight-color*
                     :accessor input-back-color
                     :documentation
                     "The colour to be used as the background colour for the ~
                      editable text when input is active.")
   )
  (:documentation " A text-view whose text can be altered by keyboard input.")
  )

;;;
;;;  The following is a utility function used by draw-view and by handle-key-event
;;;

(defun draw-&-return-pos-info (editable-text-view viewport &key string (counter 0))
  (let* ((self editable-text-view)
         (col (if (input-mode-p self)
                (input-color self)
                (draw-style self :color)))
         (font (draw-style self :font))
         (pos-in-string (- counter 1))
         (start 0)
         (break-between-words? (break-between-words self))
         (results-by-vp NIL)
         )
    (unless string (setf string (get-text self)))
    (flet
      ((read-word (string)
         (If (> start (length string))
           nil
           (let* ((pos-space (position #\Space string :test #'char= :start start))
                  (pos-line (position #\Newline string :test #'char= :start start))
                  result
                  to-newline?)
             (cond
              ((and pos-line pos-space)
               (cond
                ((< pos-line pos-space)
                 (setq result (subseq string start pos-line))
                 (setq start (+  1 pos-line))
                 (setq to-newline? T))
                (T (setq result (subseq string start (+ 1 pos-space)))
                   (setq start (+  1 pos-space)))))
              (pos-space
               (setq result (subseq string start (+ 1 pos-space)))
               (setq start (+  1 pos-space)))
              (pos-line
               (setq result (subseq string start pos-line))
               (setq start (+  1 pos-line))
               (setq to-newline? T))
              (T  (setq result (subseq string start))
                  (setq start (+ 1 (length string)))))
             (cons result to-newline?)))))
      
      (with-exposed-viewports self viewport vp
        (let* ((w (window-of vp))
               (pos-by-y NIL)
               (x-pos-&-chars NIL)
               (leading (wb:canvas-font-leading w :font font))
               (descent (wb:canvas-font-descent w :font font))
               (ascent (wb:canvas-font-ascent w :font font))
               (height (+ leading ascent descent))
               (input-pos (cdr (assoc vp (input-positions self))))
               )
          (wb:with-pen-color w col
            (wb:with-canvas-font w font
              (multiple-value-bind (l r b tp ) (bounds-of vp)
                (cond
                 (input-pos
                  (wb:canvas-move-to w
                                     (wb:position-x input-pos)
                                     (wb:position-y input-pos)))
                 (T
                  (setf input-pos (wb:make-position l (- tp ascent)))
                  (wb:canvas-move-to w l (- tp ascent))
                  ))
                (loop for (word . newline) = (read-word string)
                      until (or (and (null word)
                                     (null newline)   ;; <------------------here
                                     )
                                (< (- (wb:canvas-y w) descent)  b))
                      do 
                      (cond
                       (break-between-words?
                        (when (> (wb:canvas-string-width w word :font font)
                                 (- r (wb:canvas-x w)))
                          (push (cons (wb:canvas-y w) x-pos-&-chars) pos-by-y)
                          (setf x-pos-&-chars NIL)
                          (wb:canvas-move-to w l (- (wb:canvas-y w) height)))
                        (do ((char)
                             (i 0 (1+ i)))
                            ((= i (length word)))
                          (setf char (elt word i))
                          (push (list (wb:canvas-x w) (incf pos-in-string) char)
                                x-pos-&-chars)
                          (wb:canvas-draw-character w char)))
                       (T 
                        (do ((char)
                             (i 0 (1+ i)))
                            ((= i (length word)))
                          (setf char (elt word i))
                          (when (> (wb:canvas-string-width w (string char) :font font)
                                   (- r (wb:canvas-x w)))
                            (push (cons (wb:canvas-y w) x-pos-&-chars) pos-by-y)
                            (setf x-pos-&-chars NIL)
                            (wb:canvas-move-to w l (- (wb:canvas-y w) height))
                            )
                          (push (list (wb:canvas-x w) (incf pos-in-string) char)
                                x-pos-&-chars)
                          (wb:canvas-draw-character w char)
                          )))
                      (when newline
                        ;; stick the newline character on the end of the  present line
                        (push (list (wb:canvas-x w) (incf pos-in-string) #\NewLine)
                              x-pos-&-chars)
                        ;; end the current line
                        (push (cons (wb:canvas-y w) x-pos-&-chars) pos-by-y)
                        ;; move to beginning of new line
                        (wb:canvas-move-to w l (- (wb:canvas-y w) height))
                        ;; start newline
                        (setf x-pos-&-chars NIL)
                        )
                      finally
                      (push (cons (wb:canvas-y w) x-pos-&-chars) pos-by-y)
                      ))))
          
          (setf input-pos (wb:make-position (wb:canvas-x w) (wb:canvas-y w)))
          (push (list vp
                      (cons :input-position input-pos)
                      (cons :positions pos-by-y))
                results-by-vp)
          ))
      )
    results-by-vp))

(defmethod draw-view ((self editable-text-view) &key viewport)
  ;; get rid of the input-positions so that the drawing will start from
  ;; the natural origin.
  (when (viewports-of self)
    (setf (input-positions self) NIL))
  (let ((results NIL))
    (setf results (draw-&-return-pos-info self viewport))
    (when results
      ;; Now tuck the results away on self
      (loop
        for result in results do
        (let* ((vp (car result))
               (info (rest result))
               (input-pos (cdr (assoc :input-position info)))
               (positions (cdr (assoc :positions info))))
          (if (assoc vp (character-positions self))
            (setf (cdr (assoc vp (character-positions self))) positions)
            (push (cons vp positions) (character-positions self)))
          (if (assoc vp (input-positions self))
            (setf (cdr (assoc vp (input-positions self))) input-pos)
            (push (cons vp input-pos) (input-positions self)))
          )))))


(defun erase-input-mark (self viewport)
  (let ((back-color (input-back-color self)))
    (with-exposed-viewports self viewport vp
      (let* ((w (window-of vp))
             (font (draw-style self :font))
             (descent (wb:canvas-font-descent w :font font))
             (ascent (wb:canvas-font-ascent w :font font))
             (input-pos (cdr (assoc vp (input-positions self))))
             )
        (unless (input-mode-p self)
          (setf back-color (wb:canvas-background-color w)))
        (multiple-value-bind (l r b tp ) (bounds-of vp)
          (declare (ignore b r))
          (when (null input-pos)
            ;; Should never happen.
            (setf input-pos (wb:make-position l (- tp ascent))))
          (wb::canvas-draw-line w
                                (wb:position-x input-pos)
                                (- (wb:position-y input-pos) descent)
                                (wb:position-x input-pos)
                                (+ (wb:position-y input-pos) ascent -1)
                                :color back-color)
          )))))

(defun draw-input-mark (self viewport)
  (let ((input-color (input-color self)))
    (with-exposed-viewports self viewport vp
      (let* ((w (window-of vp))
             (font (draw-style self :font))
             (descent (wb:canvas-font-descent w :font font))
             (ascent (wb:canvas-font-ascent w :font font))
             (input-pos (cdr (assoc vp (input-positions self))))
             )
        (unless (input-mode-p self)
          (setf input-color (draw-style self :color)))
        (multiple-value-bind (l r b tp ) (bounds-of vp)
          (declare (ignore b r))
          (when (null input-pos)
            ;; Should never happen.
            (setf input-pos (wb:make-position l (- tp ascent))))
          (wb::canvas-draw-line w
                                (wb:position-x input-pos)
                                (- (wb:position-y input-pos) descent)
                                (wb:position-x input-pos)
                                (+ (wb:position-y input-pos) ascent -1)
                                :color input-color)
          )))))

(defmethod stop-input ((self editable-text-view) viewport)
  (erase-input-mark self viewport)
  )

(defmethod stop-input :after ((self editable-text-view) viewport)
  (clear-view-viewport self :viewport viewport)
  (draw-view self :viewport viewport))


(defmethod start-input ((self editable-text-view) viewport)
  (draw-input-mark self viewport))

;;; the next method replaces the 3 block-commented out below it 29SEP2023  gwb
(defmethod end-event-input-p ((view editable-text-view) (event T))
  (declare (ignorable view event))
  (cond ((eql event wb:+enter-key-event+) T)
        ((eql event wb:+escape-key-event+) T)
        (T nil)))
  
#|
(defmethod end-event-input-p ((view editable-text-view) (event T)) ;T)) 18SEP2023
  (declare (ignorable view event)) ;(declare (ignore view event)) ;29JUL2023
  NIL)

(defmethod end-event-input-p
           ((view editable-text-view) (event (eql wb:+enter-key-event+))) ;wb:*enter-key-event*))) 23FEB2022 gwb
  (declare (ignorable view event)) ;(declare (ignore view event)) ;29JUL2023
  T)

(defmethod end-event-input-p
           ((view editable-text-view) (event (eql wb:+escape-key-event+))) ;wb:*escape-key-event*))) 23FEB2022 gwb
  (declare (ignorable view event)) ;(declare (ignore view event)) ; 28JUL2023
  T)
|#


(defmethod refocus-input ((self editable-text-view) viewport
                          &optional position)
  "Moves the focus of the input to a new location determined from the ~
   given position."
  (with-exposed-viewports self viewport vp
    (let* ((w (window-of vp))
           (font (draw-style self :font))
           (descent (wb:canvas-font-descent w :font font))
           (ascent (wb:canvas-font-ascent w :font font))
           (input-pos (cdr (assoc vp (input-positions self))))
           (character-positions (cdr (assoc vp (character-positions self))))
           (last-y (first (first character-positions)))
           (last-x (first (second (first character-positions))))
           rows-before
           input-row
           chars-before
           input-pos-x
           input-pos-y
           pos-x
           pos-y
           )
      (multiple-value-bind (l r b tp ) (bounds-of vp)
        (declare (ignore r b))
        (unless last-x (setf last-x l))
        (unless last-y (setf last-y (- tp ascent)))
        (cond
         ((or (null input-pos) (null character-positions))
          ;; no characters, starting input
          (setf input-pos-x l
                input-pos-y (- tp ascent))
          )
         (T
          ;; erase the current input mark, if there is one.
          (erase-input-mark self vp)
          ;; local-vars
          (setf input-pos-x (wb:position-x input-pos)
                input-pos-y (wb:position-y input-pos))
          (setf pos-x (wb:position-x position)
                pos-y (wb:position-y position))
          
          ;; Need to determine which character is immediately before
          (cond
           ((< pos-y (- last-y descent))
            ;; then it's past the last line, so place the input position after the
            ;; last character.
            (setf input-pos-y last-y)
            (setf input-pos-x
                  ;; sum last-x and the character's width
                  (+ last-x
                     (if (third (second (first character-positions)))
                       (wb:canvas-character-width
                             w
                             ;; last-char
                             (third (second (first character-positions)))
                             :font font)
                       0)))
            )
           (T
            ;; Determine input row
            (setf rows-before
                  (member-if #'(lambda (row) (< pos-y (+ (car row) ascent)))
                             character-positions))
            (setf input-row (car rows-before))
            
            ;; Here are the true rows before
            (setf rows-before (cdr rows-before))
            ;;
            ;; get the characters before the input in the same row.
            ;;
            (setf chars-before
                  (member-if #'(lambda (x)
                                 (<
                                  (+ (car x)
                                     (wb:canvas-character-width w (third x) :font font))
                                  pos-x))
                             (cdr input-row)))
            ;;
            ;; the location of the character before the input-position 
            ;; is either in this row; or if the input is before the first
            ;; character then we need to go to the end of the previous row.
            ;;
            (cond
             (chars-before
              ;; The y location
              (setf input-pos-y (first input-row))
              
              ;; The X location
              (setf input-pos-x 
                    (+ 
                     ;; sum previous character's x position
                     (first (first chars-before))
                     ;; and it's width
                     (wb:canvas-character-width
                      w
                      ;; the char
                      (third (first chars-before))
                      :font font)
                     ))
              )
             (rows-before
              ;; No characters precede the position in this row
              ;; Move to after the last character in the row before
              (setf input-pos-y (first (first rows-before)))
              (setf input-pos-x 
                    (+
                     (first (second (first rows-before)))
                     (wb:canvas-character-width
                      w
                      ;; the char
                      (third (second (first rows-before)))
                      :font font)))
              )
             (T
              ;; Must be at the beginning
              (setf input-pos-x l
                    input-pos-y (- tp ascent)))
             )
            )
           )
          )
         )
        )
      
      ;; Got the new input position
      (setf input-pos (wb:make-position input-pos-x input-pos-y))
      
      ;; Store it away
      (if (assoc vp (input-positions self))
        (setf (cdr (assoc vp (input-positions self)))
              input-pos)
        (push (cons vp input-pos) (input-positions self)))
      
      ;; Indicate where the input position is in the display
      (draw-input-mark self vp)
      )
    )
  )

(defmethod handle-key-event ((self editable-text-view) (char character))
  "Inserts the characters at the current position."
  (let ((col (draw-style self :color))
        (font (draw-style self :font)))
    (with-exposed-viewports self NIL vp
      (let* ((w (window-of vp))
             (leading (wb:canvas-font-leading w :font font))
             (descent (wb:canvas-font-descent w :font font))
             (ascent (wb:canvas-font-ascent w :font font))
             (height (+ leading ascent descent))
             (input-pos (cdr (assoc vp (input-positions self))))
             (character-positions (cdr (assoc vp (character-positions self))))
             (last-index 
              (second (second 
                               (find-if #'(lambda (row)
                                            (not (null (cdr row))))
                                        character-positions))))
             (last-y (first (first character-positions)))
             (last-x (first (second (first character-positions))))
             (rows-before NIL)
             (input-row NIL)
             (chars-before NIL)
             (input-index NIL)
             (input-pos-x NIL)
             (input-pos-y NIL)
             back-color)
        (cond
         ((input-mode-p self)
          (setf back-color (input-back-color self))
          (setf col (input-color self))
          )
         (T
          (setf back-color (wb:canvas-background-color w))))
        (wb:with-pen-color w col
          (wb:with-canvas-font w font
            (multiple-value-bind (l r b tp ) (bounds-of vp)
              (unless (wb:position-p input-pos)
                (setf input-pos (wb:make-position l (- tp ascent))))
              (unless last-x (setf last-x l))
              (unless last-y (setf last-y (- tp ascent)))
              (setf input-pos-x (wb:position-x input-pos)
                    input-pos-y (wb:position-y input-pos))
              (wb:canvas-move-to w input-pos-x input-pos-y)
              ;;
              ;; first get the rows that are OK.
              ;;
              (when character-positions
                (setf rows-before
                      (member-if #'(lambda (row) (= (car row) input-pos-y))
                                 character-positions)))
              (setf input-row (car rows-before))
              (setf rows-before (cdr rows-before))
              ;;
              ;; get the characters before the input in the same row.
              ;;
              (when input-row
                (setf chars-before
                      (member-if #'(lambda (x) (< (car x) input-pos-x))
                                 (cdr input-row))))
              ;;
              ;; get the index of the character before the input-position 
              ;; in the string 
              ;;
              (cond
               (chars-before (setf input-index (second (first chars-before))))
               (rows-before
                ;; We pop to the previous line
                (setf input-index (second (second (first rows-before)))))
               (T (setf input-index NIL))
               )
              ;;
              ;; Deletion of previous character must be treated differently from
              ;; simply adding characters.
              ;;
              (if (member char (list wb:+rubout-event+ wb:+delete-event+)) ;wb:*rubout-event* wb:*delete-event*)) 23FEB2022 gwb
                (cond
                 ((null last-index)
                  ;; Nothing is here, delete is bogus.
                  ;; Set input-pos and character-positions properly
                  ;;
                  (setf input-pos-x l
                        input-pos-y (- tp ascent))
                  ;; tuck it away
                  (setf (cdr (assoc vp (input-positions self)))
                        (wb:make-position input-pos-x input-pos-y))
                  ;; blow away character positions
                  (when (assoc vp (character-positions self))
                    (setf (cdr (assoc vp (character-positions self))) NIL))
                  )
                 ((null input-index)
                  ;; Nothing is before, delete is bogus.  just set input-pos right.
                  (setf input-pos-x l
                        input-pos-y (- tp ascent))
                  ;; tuck it away
                  (setf (cdr (assoc vp (input-positions self)))
                        (wb:make-position input-pos-x input-pos-y))
                  )
                 ((zerop input-index)
                  ;; delete first character, just erase all, delete, redraw
                  (erase-view self :viewport vp)
                  (setf (text-of self) (subseq (get-text self) 1))
                  (draw-view self :viewport vp)
                  (setf input-pos-x
                        ;; x value of
                        (first 
                         ;;first character
                         (first (last   ; was second?! 
                                 ;; in first row
                                 (first (last character-positions))))
                         )
                        input-pos-y
                        ;; y value of
                        (first 
                         ;; first row
                         (first (last character-positions))))
                  ;; tuck it away
                  (setf (cdr (assoc vp (input-positions self)))
                        (wb:make-position input-pos-x input-pos-y))
                  )
                 ((= input-index last-index)
                  (erase-input-mark self vp)
                  ;; delete last character
                  (setf (text-of self) (subseq (get-text self) 0 last-index))
                  (wb:canvas-draw-filled-rectangle
                   w
                   last-x r
                   (- last-y descent)
                   (+ last-y ascent)
                   :color back-color)
                  ;; 
                  ;; Move back to previous character's position
                  (setf input-pos-x last-x)
                  (setf input-pos-y last-y)
                  ;;
                  ;; Update character-positions
                  ;; 
                  (setf (first character-positions)
                        (cons
                         ;; y of last row
                         (first (first character-positions))
                         ;; strip off last character of the rest of that row
                         (rest (rest (first character-positions)))))
                  ;;
                  ;; Move back to previous line if we deleted the current line
                  ;; 
                  (when
                    (and rows-before
                         ;; there was only one character in the present line
                         ;; (which has now been deleted)
                         (= 1 (length chars-before))
                         ;; And the last character of the previous row
                         ;; didn't force a new line.
                         (not (char-equal  #\Newline
                                           (third (second (first rows-before)))))
                         )
                    (setf input-pos-x
                          (+ (first (second (first rows-before)))
                             (wb:canvas-character-width
                              w
                              (third (second (first rows-before)))
                              :font font)))
                    (setf input-pos-y
                          (first (first rows-before)))
                    ;; drop the last row.
                    (setf character-positions
                          (rest character-positions))
                    )
                  ;;
                  ;; tuck updated info away
                  ;;
                  (setf (cdr (assoc vp (input-positions self)))
                        (wb:make-position input-pos-x input-pos-y))
                  (if (assoc vp (character-positions self))
                    (setf (cdr (assoc vp (character-positions self)))
                          character-positions)
                    (push (cons vp character-positions)
                          (character-positions self)))
                  )
                 (T
                  ;; Delete in the middle somewhere.
                  ;;
                  ;; First clear things
                  (let ((line-bottom (- input-pos-y descent 1))
                        left)
                    (when chars-before
                      ;; Erase rest of the line
                      (setf left (first (first chars-before)))
                      (wb:canvas-draw-filled-rectangle w left r line-bottom
                                                       (+ line-bottom height)
                                                       :color back-color)
                      (setf input-pos-x left)
                      )
                    ;; Now blow out the rest of the viewport
                    (wb:canvas-draw-filled-rectangle w l r b line-bottom
                                                     :color back-color)
                    )
                  ;;
                  ;; Update the input-position.
                  ;;
                  (setf chars-before (rest chars-before))
                  (when (and rows-before
                             (null chars-before)
                         ;; And the last character of the previous row
                         ;; didn't force a new line.
                         (not (char-equal  #\Newline
                                           (third (second (first rows-before)))))
                             )
                    (setf input-pos-x
                          (+ (first (second (first rows-before)))
                             (wb:canvas-character-width
                              w
                              (third (second (first rows-before)))
                              :font font)))
                    (setf input-pos-y
                          (first (first rows-before))))
                  ;;
                  ;; tuck the input-position away
                  ;;
                  (setf (cdr (assoc vp (input-positions self)))
                        (wb:make-position input-pos-x input-pos-y))
                  ;;
                  ;; Update text string
                  ;;

                  (setf (text-of self)
                        (concatenate 'string
                                     (subseq (get-text self) 0 input-index)
                                     (subseq (get-text self) (1+ input-index))))
                  ;;
                  ;; Draw the rest of the string and update things
                  ;;
                  (let ((results NIL))
                    (setf
                     results
                     (first
                      (draw-&-return-pos-info
                       self vp
                       :string (subseq (get-text self) input-index) 
                       :counter  input-index ) ) ) ;;(1+ input-index))))
                    (when results
                      ;; Now tuck the results away on self
                      (let* ((info (rest results))
                             (input-pos (cdr (assoc :input-position info)))
                             (positions (cdr (assoc :positions info))))
                        (if (assoc vp (input-positions self))
                          (setf (cdr (assoc vp (input-positions self))) input-pos)
                          (push (cons vp input-pos) (input-positions self)))
                        ;;
                        ;; Now merge these results with unchanged piece
                        ;;
                        ;; fix the input row
                        ;;
                        (nconc (rest (first (last positions))) chars-before)
                        ;;
                        ;; put all the rows together
                        ;;
                        (nconc positions rows-before)
                        ;;
                        ;; Now tuck the positions away on self
                        ;;
                        (if (assoc vp (character-positions self))
                          (setf (cdr (assoc vp (character-positions self))) positions)
                          (push (cons vp positions) (character-positions self)))
                        ;;
                        ;; Reset the input position
                        (setf (cdr (assoc vp (input-positions self)))
                              (wb:make-position input-pos-x input-pos-y))
                        
                        ))))
                 )
                ;; Just update, erase, and draw
                (let ((results NIL))
                  (cond
                   ((null input-index)
                    ;; At the beginning, just do it.
                    (setf (text-of self) (string char))
                    (setf results (first (draw-&-return-pos-info self vp)))
                    )
                   (T
                    ;; Edit in the middle somewhere.
                    ;;
                    ;; Clear things
                    (let ((line-bottom (- input-pos-y descent 1))
                          left)
                      (when chars-before
                        ;; Erase rest of the line
                        (setf left (+ (first (first chars-before))
                                      (wb:canvas-character-width
                                       w
                                       (third (first chars-before))
                                       :font font)))
                        (wb:canvas-draw-filled-rectangle
                         w left r line-bottom (+ line-bottom ascent descent)
                         :color back-color)
                        )
                      ;; Now blow out the rest of the viewport
                      (wb:canvas-draw-filled-rectangle
                       w l r b line-bottom
                       :color back-color)
                      )
                    ;;
                    ;; Update the input-position.
                    ;;
                    (setf (cdr (assoc vp (input-positions self)))
                          (wb:make-position input-pos-x input-pos-y))
                    ;;
                    (setf (text-of self)
                          (concatenate 'string
                                       (subseq (get-text self) 0 (1+ input-index))
                                       (string char)
                                       (subseq (get-text self) (1+ input-index))))
                    ;;
                    ;; Draw the rest of the string and update things
                    ;;
                    (setf
                     results
                     (first
                      (draw-&-return-pos-info
                       self vp
                       :string (subseq (get-text self) (1+ input-index))
                       :counter  (1+ input-index)))))
                   )
                  (when results
                    ;; Now tuck the results away on self
                    (let* ((info (rest results))
                           (positions (cdr (assoc :positions info)))
                           (first-row (first (last positions)))
                           input-char
                           (char-widowed-on-newline? NIL))
                      (when (null (rest first-row))
                        ;; our input character went to a new line
                        (setf char-widowed-on-newline? T)
                        (setf positions (butlast positions))
                        (setf first-row (first (last positions))))
                           ;; Need to determine the input-position
                      (setf input-char
                            ;; gets the character triple
                            (first
                             ;; gets the last (i.e. first written) character
                             (last
                              ;; without the y value
                              (rest first-row))))
                      ;;
                      ;; determine the input position
                      ;;
                      (setf input-pos-y (first first-row))
                      (setf input-pos-x
                            (+ (first input-char)
                               (wb:canvas-character-width
                                w (third input-char) :font font)))
                      (setf input-pos (wb:make-position input-pos-x input-pos-y))
                      ;;
                      ;; Reset the input position
                      ;;
                      (if (assoc vp (input-positions self))
                        (setf (cdr (assoc vp (input-positions self))) input-pos)
                        (push (cons vp input-pos) (input-positions self)))
                      ;;
                      ;; Now merge these results with unchanged piece
                      ;;
                      (cond
                       (char-widowed-on-newline?
                        ;;
                        ;; put all the rows together
                        ;;
                        (if input-row (push input-row rows-before))
                        (nconc positions rows-before))
                       (T
                        ;; fix the input row
                        ;;
                        (nconc (rest first-row) chars-before)
                        ;;
                        ;; put all the rows together
                        ;;
                        (nconc positions rows-before)
                        )
                       )
                      ;;
                      ;; Now tuck the positions away on self
                      ;;
                      (if (assoc vp (character-positions self))
                        (setf (cdr (assoc vp (character-positions self))) positions)
                        (push (cons vp positions) (character-positions self)))
                      
                      )))))
            )
          )
        )
      ;;
      ;;  Redraw the input-mark
      ;;
      (draw-input-mark self vp)
      )
    )
  )


#|
(setf test
      (make-instance 'editable-text-view 
        :text "Now is the time for all good men to come to the aid of their country."))
(set-text test "Now is the time for all good men to come to the aid of their country.")
(setf self test)
(setf viewport (make-viewport))
(setf vp (first (viewports-of self)))
(setf char wb::*delete-event*)
(draw-view test)
(inspect test)
|#
