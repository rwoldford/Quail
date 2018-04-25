;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cr-color-canvas-pc.lsp
;;; 12MAR02
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;FROM ..
(defun make-color-canvas (&rest
                          canvas-keywords
                          &key
                          left bottom width height
                          ;;           (type (device-type))
                          (canvas-class 'canvas)
                          (title "Color Canvas")
                          (background-color NIL)
                          (pen-color NIL)
                          (pen-width nil)
                          (pen-operation nil)
                          (font *normal-graphics-font*)
                          &allow-other-keys)
  "Creates and returns a black and white canvas."
  (declare (special *normal-graphics-font*
                    *default-canvas-background-color*
                    *default-canvas-pen-color*
                    *white-color*
                    *black-color*))    
  (unless left
    (setq left (round (/ (- (screen-width) width) 2))))
  (unless bottom
    (setq bottom (round (/ (- (screen-height) height) 2))))
     (when font 
          (setf canvas-keywords (remove :font canvas-keywords))
          (setf canvas-keywords (remove font canvas-keywords)))
  (let* ((top (+ bottom height))
 (c (apply #'cg::make-window  (gensym "canvas") :device canvas-class 
        :parent (cg::screen  cg::*system*) :scrollbars T
        :exterior (cg::make-box left (- (screen-height) top 25)
                     (+ left width) (- (screen-height) bottom 25))
       #| Lines below replaced with the defn of :exterior above
       :window-exterior-top-left (cg::make-position left 
                (- (screen-height) top 25)                                  
                                  )
       :visible-box (cg::make-box left (- bottom  height )
                                             (+ left width) 
                        bottom  )
       |#
             :title title
             :color? T
             :allow-other-keywords t
             canvas-keywords))
    )
    ;;
    ;; Now set the colors 
    ;;
    (canvas-set-background-color c (or background-color
                                       *default-canvas-background-color*))
    (canvas-set-pen c
                    :color (or pen-color
                               *default-canvas-pen-color*
                               (canvas-default-draw-color c))
                    :width pen-width 
                    :operation pen-operation)
    ;;
    ;; Now set the fonts
    ;;
     (setf (canvas-font c) font) 
    ;; 
    ;;
    ;; Finally get the pen back to the correct origin
    ;;
    
    (canvas-move-to c 0 0)
    c))
----------------------------------------------------------------------
FUNCTION-TOO-LONG: Definition way too long!
----------------------------------------------------------------------
SETS-PARAMETERS: It's bad style to reassign input parameters like
CANVAS-KEYWORDS -- and usually useless.
----------------------------------------------------------------------
SETS-PARAMETERS: It's bad style to reassign input parameters like
BOTTOM -- and usually useless.
----------------------------------------------------------------------
SETS-PARAMETERS: It's bad style to reassign input parameters like
LEFT -- and usually useless.
----------------------------------------------------------------------
