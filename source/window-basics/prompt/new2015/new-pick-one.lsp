;;; This is new-pick-one.lsp
;;; Started 23Feb22015 gwb

;; From examples/cg/menus-and-help.cl
(in-package :cg-user)

(defclass my-main-window (frame-window)())

(defclass my-single-item-list (single-item-list)())

(defun my-on-change (widget new-value old-value)
  "Called when the value of WIDGET has changed."
  (window-message (parent widget) "Widget ~s goes from ~s to ~s."
    (name widget) old-value new-value)
  t)

;; setup quail-dialog-font
(defvar quail-dialog-font NIL)

(defun set-quail-dialog-font (&optional  font ) 
  ;"Sets quail-dialog-font with default (make-font-ex nil "MS Sans Serif" 13 nil)~
  ;  intput must be a cg::font, result is one too."
  (setf quail-dialog-font (or font (make-font-ex nil "MS Sans Serif" 13 nil)))
  quail-dialog-font)

(defvar quail-dialog-window-left NIL)

(defun set-quail-dialog-window-left (&optional (left 10))
  "Sets the value of quail-dialog-window-left~
    Defaults to 10"
  (setf quail-dialog-window-left left)
  quail-dialog-window-left)

(defvar quail-dialog-window-top NIL)

(defun set-quail-dialog-window-top (&optional (top 10))
  "Sets the value of quail-dialog-window-top~
    Defaults to 10"
  (setf quail-dialog-window-top top)
  quail-dialog-window-top)

(defvar quail-dialog-box-left NIL)

(defun set-quail-dialog-box-left (&optional (left 10))
  "Sets the value of quail-dialog-box-left~
    Defaults to 10"
  (setf quail-dialog-box-left left)
  quail-dialog-box-left)

(defvar quail-dialog-box-top NIL)

(defun set-quail-dialog-box-top (&optional (top 10))
  "Sets the value of quail-dialog-box-top~
    Defaults to 10"
  (setf quail-dialog-box-top top)
  quail-dialog-box-top)

(defvar quail-dialog-separator NIL)

(defun set-quail-dialog-separator (&optional (separator 15))
  "Sets the value of quail-dialog-separator~
   the vertical or horizontal amount between elements~
   Deafults to 15"
  (setf quail-dialog-separator separator)
  quail-dialog-separator)

(defvar quail-dialog-displayed NIL)

(defun set-quail-dialog-displayed (&optional (displayed 5))
  "Sets the quail-dialog-displayed, the number of items~
   shown in a dialog list.
   Defaults to 5"
  (setf quail-dialog-displayed displayed)
  displayed)

(defvar quail-dialog-scrollbar-width NIL)

(defun set-quail-dialog-scrollbar-width (&optional (width 30))
  "Sets the width allowance for a scrollbar in quail dialogs~
   Defaults to 30."
  (setf quail-dialog-scrollbar-width width)
  quail-dialog-scrollbar-width)
  
  

;; I need something to deal with the stringish sizes
(defun quail-dialog-string-sizes (items)
  "Collects the maximum width of the strings in items~
                         and the width of the Cancel and Select strings."
  (let*((zot (open-stream 'bitmap-stream nil nil))
       (font-zot (font zot) quail-dialog-font)
        (its-metrics (fontmetrics zot))
        (max-item-width 
                           (first (sort (mapcar #'(lambda (x) ;; to allow for scrollbar width
                                                  (if (stringp x)
                                                      (cg::stream-string-width zot x)
                                                    (cg::stream-string-width zot
                                                                             (format NIL "~s" x))))
                                          items) #'>)))
        (item-height (font-height its-metrics))
        (zcb (make-instance 'cancel-button
               :font
                 (make-font-ex nil "MS Sans Serif" 13 nil)
               :name :cancel-button-1
               :title "Cancel"))
        (cancel-width (width zcb))
        (cancel-height (height zcb))
    )
  (list :max-width max-width :item-height item-height :cancel-width cancel-width :cancel-height  cancel-height)
    ))

(defun make-quail-dialog-box (items )
  "Sets the box which holds the items in a quail-dialog"
  (let* ((left (set-quail-dialog-box-left))
         (top (set-quail-dialog-box-top))
         (width (+ (getf (quail-dialog-string-sizes items) :max-width) (set-quail-dialog-scrollbar-width)))
         (height (* (set-quail-dialog-displayed) (getf (quail-dialog-string-sizes items) :item-height)))
         )
  (make-box left top (+ left width) (+ top height))
  ))


;; That's all very well, but what I really want is to set the font and then get stringish things
;; with respect to that font. This is complicated because I have to associate the font with
;; a stream since that's what stream-string-width works on.
;; Try the following let*. If OK then work it into other things.
(let* ((zot (open-stream 'bitmap-stream nil nil))
       (font-zot (font zot) quail-dialog-font)
       (its-metrics (fontmetrics zot))
       )
  (format t "~%Font height is ~d " (font-height its-metrics))
  (format t "~%Stream-string-width is ~d " (stream-string-width zot "Stream-String-width"))
  )

;; It did function as expected. Note to self a canel-button's width comes from just (width ..

(defun the-pick-one (items)
  (setf pick-one-window (make-window :my-main-window
                          :class 'my-main-window
                          :owner (screen *system*)
                          :title "Menus and Help Options"
                          :exterior (make-box (quail-dialog-window-left) (quail-dialog-window-top)
                                              (+ (quail-dialog-window-left)
                          ;:exterior (make-box 400 200 900 500)
                          :scrollbars nil
                          :dialog-items
                          (list (make-instance 'my-single-item-list
                                  :font
                                  quail-dialog-font
                                  :name :list
                                  :range items ;'(one two three)
                                  :value '(first items);'one
                                  ;:tooltip "The first three."
                                  :help-string "Pick a number.  Any number."
                                  :on-mouse-in 'show-the-help-string
                                  :on-mouse-out 'clear-the-status-bar
                                  :on-change 'my-on-change
                                  :left 20 :top 60
                                  :width (first (string-sizes items)) 
                                  :height 80)
                                (make-instance 'cancel-button
                                  :font
                                  (make-font-ex nil "MS Sans Serif" 13 nil)
                                  :left 300
                                  :name :cancel-button-1
                                  :title "Cancel"
                                  :top 60)
                                (make-instance 'cancel-button
                                  :font 
                                  (make-font-ex nil "MS Sans Serif" 13 nil)
                                  :left 300
                                  :name :select-button-1
                                  :title "Select"
                                  :top 110)
                                )
                          )
   ))


;; From V/S-V/numerical-label.lsp
(setf format-types '("Lisp format directive"
                           "Fixed-format floating point"
                           "Exponential floating point"
                           "Dollars floating point"
                           "General floating point"
                           "Decimal"
                           "Binary"
                           "Octal"
                           "Hexadecimal"
                           "Radix"
                           ))

;; From permuted index on cancel button class
(make-instance 'cancel-button
               :font
                 (make-font-ex nil "MS Sans Serif" 13 nil)
               :left 189
               :name :cancel-button-1
               :title "Cancel"
               :top 146)

