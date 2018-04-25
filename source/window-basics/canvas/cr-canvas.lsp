;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cr-canvas.lsp
;;; 12MAR02
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;FROM ..
(defun set-up-default-canvas-region
       (&optional (left 10) (bottom 10)
                  (width 300) (height 300))
       "Sets the default region where a new canvas will appear on the ~
        screen."
       
  (setf *default-canvas-region*  (make-region left bottom width height)))
----------------------------------------------------------------------
SETS-GLOBALS: GLOBALS!! Don't use global variables, i.e.,
*DEFAULT-CANVAS-REGION*
----------------------------------------------------------------------
OPTIONALS: Multiple optional arguments get confusing. Use &KEY for
SET-UP-DEFAULT-CANVAS-REGION.
----------------------------------------------------------------------
NEEDLESS-SETF: Why do you think you need that SETF on
*DEFAULT-CANVAS-REGION*?
----------------------------------------------------------------------
