;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cr-make-canvas.lsp
;;; 12MAR02
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;FROM ..
(defun set-up-default-canvas-region
       (&optional (left *screen-left-margin*) (bottom *screen-bottom-margin*)
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
;;FROM ..
(defun make-canvas (&rest canvas-keywords
                          &key
                          left bottom width height
                          region
                          (color? (color-device-p))
                          (type (device-type))
                          (canvas-class 'canvas)
                          (font *normal-graphics-font*)
                          (title "Canvas")
                          background-color
                          pen-color
                          &allow-other-keys)
  "Creates and returns a canvas.  Canvas size is calculated by: ~
   first, using left/bottom/width/height if they are given; ~
   second, using the bounding box of region if it is a region; ~
   third, using *default-canvas-region* if region is NIL; ~
   fourth, prompting user to use pointing device to give size."

  (declare (special *default-canvas-region* *normal-graphics-font*))
  (declare (ignore pen-color background-color title font canvas-class))

  ;; If left/bottom/width/height not given, try to extract from region.
  ;; Where all these things are NIL, we quietly use the default. -- jrm
  
  (unless (and left bottom width height)
    (when (null region) (setf region *default-canvas-region*))
    (when (region-p region)
      (setf left (region-left region)
            bottom (region-bottom region)
            width  (region-width region)
            height (region-height region))))
  
  (if (and left bottom width height)
    
    (apply
     (if (and color? (color-device-p))
       #'make-color-canvas
       #'make-b&w-canvas)
     :left (max left (window-min-left))
     :bottom (max bottom (window-min-bottom))
     :width (min width (window-max-width)) 
     :height (min height (window-max-height))
     :type type
     :allow-other-keys t canvas-keywords)
    
    (apply #'prompt-for-canvas canvas-keywords))
  
  )
----------------------------------------------------------------------
FUNCTION-TOO-LONG: Definition way too long!
----------------------------------------------------------------------
SETS-PARAMETERS: It's bad style to reassign input parameters like
REGION -- and usually useless.
----------------------------------------------------------------------
