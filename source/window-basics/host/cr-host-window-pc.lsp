
On (d m y) (21 MAR 2002) 
At (h:m) (16 ":" 31) 
From directory "\\Program Files\\SOFTWARE\\QUAIL\\CR-QUAIL\\Source\\Window-Basics\\Host\\"
Analysing "host-window-pc.lsp" 
Results to "cr-host-window-pc.lsp" 
**********************************************************************

(IN-PACKAGE :WB)
----------------------------------------------------------------------
**********************************************************************

(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (EXPORT '(HOST-WINDOW HOST-PANE)))
----------------------------------------------------------------------
**********************************************************************

(DEFCLASS HOST-WINDOW (CG:BITMAP-WINDOW) NIL
          (:DOCUMENTATION
           "Mixin to canvas that captures properties of the host window system."))
----------------------------------------------------------------------
**********************************************************************

(DEFCLASS HOST-PANE (CG:BITMAP-PANE) NIL)
----------------------------------------------------------------------
**********************************************************************

(DEFMETHOD CG:DEFAULT-PANE-CLASS ((C HOST-WINDOW)) 'HOST-PANE)
----------------------------------------------------------------------
**********************************************************************

(DEFMETHOD CG:SELECT-WINDOW :AFTER
    ((SELF HOST-PANE) &OPTIONAL RECURSIVE-P)
  (DECLARE (SPECIAL *CURRENT-CANVAS*) (IGNORE RECURSIVE-P))
  (SETF *CURRENT-CANVAS* (CG:PARENT SELF)))
----------------------------------------------------------------------
**********************************************************************

(DEFMETHOD CG:BRING-WINDOW-TO-FRONT :AFTER ((SELF HOST-PANE))
  (DECLARE (SPECIAL *CURRENT-CANVAS*))
  (SETF *CURRENT-CANVAS* (CG:PARENT SELF)))
----------------------------------------------------------------------
**********************************************************************

(DEFMETHOD CG:REDISPLAY-WINDOW :AFTER ((C HOST-PANE) &OPTIONAL BOX)
  (DECLARE (SPECIAL *CURRENT-CANVAS*) (IGNORE BOX))
  (LET ((PW (CG:PARENT C)))
    (WHEN (EQ PW (FIRST (WB::CANVASES))) (SETF *CURRENT-CANVAS* PW))))
----------------------------------------------------------------------
USE-EQL: Unless something special is going on, use EQL, not EQ.
----------------------------------------------------------------------
