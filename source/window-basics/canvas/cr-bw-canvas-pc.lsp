;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cr-bw-canvas-pc.lsp
;;; 12MAR02
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;FROM ..
(defun make-b&w-canvas (&rest
                          canvas-keywords
                          &key
                          left bottom width height
                          ;;           (type (device-type))
                          (canvas-class 'canvas)
                          (title "Canvas")
                          (pen-color *black-shade*)
                          (pen-width nil)
                          (pen-operation nil)
                          (font *normal-graphics-font*)
                          &allow-other-keys)
   "Creates and returns a black and white canvas."
   (declare (special *normal-graphics-font*
               *black-shade* c))
   (unless left
      (setq left (round (/ (- (screen-width) width) 2))))
   (unless bottom
      (setq bottom (round (/ (- (screen-height) height) 2))))
     (when font
          (setf canvas-keywords (remove :font canvas-keywords))
          (setf canvas-keywords (remove font canvas-keywords)))
   (let* ((top (+ bottom height))
          (c (apply #'cg::make-window (gensym "canvas") :device 'canvas-class 
              :parent (cg::screen cg::*system*) :scrollbars T
                 :window-exterior-top-left (cg::make-position left
                                                            (- (screen-height) top 25))
              :title title 
                :visible-box (cg::make-box left (- bottom height)
                                             (+ left width) bottom)
                              :color? nil
              :allow-other-keywords t
              canvas-keywords))
             ))
      
      ;;
      ;; Now set the pen
      ;;
      ;;    (canvas-set-pen c :color pen-color :width pen-width :operation pen-operation)
      ;;
      ;; Finally get the pen back to the correct origin
      ;;
      
      ;;    (canvas-move-to c 0 0)
  c)
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
