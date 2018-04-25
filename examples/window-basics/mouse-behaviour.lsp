;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          Mouse input behaviour                        
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1995 Statistical Computing Laboratory, University Of Waterloo
;;;
;;; 
;;;  Authors:
;;;     R.W. Oldford 1995.
;;;
;;;
;;;
;;; Besides keyboard input, Quail accepts a variety of input from the
;;; mouse.
;;;
;;; Quail assumes a three button mouse; if the mouse has only one or
;;; two buttons, other keys are used in combination with the mouse buttons
;;; to mimic a three-button mouse.  The three mouse buttons will be referred
;;; to as LEFT, MIDDLE, and RIGHT.
;;;
;;; KEY    |   Mac implementation
;;;________|_________________________________________________________________
;;;        |
;;; LEFT   |    Mouse button
;;;        |
;;; MIDDLE |    Mouse button
;;;        |    + Option key
;;;        |
;;; RIGHT  |    Mouse button
;;;        |    + Command key
;;;        |     (i.e. Apple or Clover key)
;;;        |
;;;        |
;;;________|_________________________________________________________________
;;;
;;; Two modifier keys, SHIFT and CTRL, can be used in conjuction with any
;;; mouse buttons. For convenience, we will sometimes refer to the case
;;; when no modifier key is used, as using the NONE modifier.
;;;
;;; Thus there are nine possibilities for mouse input
;;;   (3 buttons) x (3 modifiers)
;;;
;;;           |                   Mouse button                              |
;;; Modifier  |_____________________________________________________________|
;;;           |               |                    |                        |
;;;           |    LEFT       |      MIDDLE        |        RIGHT           |
;;;___________|_____________________________________________________________|
;;;           |               |                    |                        |
;;;           |               |                    |                        |
;;; NONE      |  Select view  | Access display     | Editing views -- cut   |
;;;           |               | characteristics of | paste, move, copy,     |
;;;           |               | the selected view  | clone, link, unlink... |
;;;           |               |                    |                        |
;;;          _|_             _|_                  _|_                       |
;;;           |               |                    |                        |
;;;           |               |                    |                        |
;;; SHIFT     |  Multiple     | Not prescribed     | Not prescribed         |
;;;           |  selection    |                    |                        |
;;;           |  of views     |                    |                        |
;;;           |               |                    |                        |
;;;          _|_             _|_                  _|_                       |
;;;           |               |                    |                        |
;;;           |               |                    |                        |
;;; CTRL      | Access the    | Not prescribed     | Not prescribed         |
;;;           | viewed-object | but reserved for   | but reserved for       |
;;;           |               | viewed-object      | viewed-object          |
;;;           |               | interaction        | interaction            |
;;;___________|_______________|____________________|________________________|
;;;
;;;
;;;
;;; 
;;;
;;;  Some mouse related functions from the window-basics package
;;;
;;; Is a mouse button pressed?

(wb:mouse-down-p)

;;; Which mouse button is pressed? ... one of :left :middle :right or :none

(wb:mouse-state)

(loop for i from 1
  ;; starts an infinite loop
  until (wb:mouse-down-p)
  ;; which will end when you press a mouse button
  finally (return (wb:mouse-state)))
      
;;; Modifier key info

(wb:control-key-p)
(wb:shift-key-p)

(loop for i from 1
  ;; starts an infinite loop
  until (and (wb:mouse-down-p)
             (or (wb:control-key-p)
                 (wb:shift-key-p)))
  ;; which will end when you press a mouse button
  finally (return (if (wb:control-key-p)
                    :CTRL
                    :SHIFT)))


;;;
;;; Where is the mouse on the screen (in screen coordinates)?

(wb:screen-mouse-position)
(wb:position-x (wb:screen-mouse-position))
(wb:screen-mouse-x)
(wb:position-y (wb:screen-mouse-position))
(wb:screen-mouse-y)

;;;
;;; Where is the mouse on a window (canvas) in its own coordinates?

(<- test-canvas (wb:make-canvas))

(wb:mouse-position test-canvas)
(wb:position-x (wb:mouse-position test-canvas))
(wb:mouse-x test-canvas)
(wb:position-y (wb:mouse-position test-canvas))
