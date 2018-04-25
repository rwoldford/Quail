;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               window-basics.asd                             
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1991 - 1992.
;;;     G.W. Bennett 2017.
;;;
;;;--------------------------------------------------------------------------------

;(in-package :make)

;(eval-when (compile load eval)
;  (load-pathnames "window-basics"))

;;;  The following hack lets us support different window systems for
;;;  different CLs
;;;  ... rwo

;(defun wb-add-system-extension (file-name-string)
;  #+:ccl-2 (add-system-extension file-name-string)
;  #+:clx (concatenate 'string file-name-string "-clx")
;  #+:aclpc (concatenate 'string file-name-string "-pc"))


(asdf:defsystem "window-basics"
    :default-component-class cl-source-file.lsp
  :components
  ((:file "window-basics/window-basics-package")
   (:module "window-basics/startup"
            :components
            ((:file "restore")
             ;;(:file (wb-add-system-extension "restore"))
             ;;<-- use quail's restore fns instead
             (:file "debug"))
            )
   (:module "window-basics/macros"
            :components
            ((:file "macros-sblx")
             (:file "operations-sblx")
             (:file "operations")
             (:file "positions")
             (:file "display-mode"))
            )
   ))
#|
   (:module "window-basics/host"
            :components
            ((:file "host-draw-package")
             (:file "host-system-sblx")
             (:file "host-draw-sblx")
             (:file "scrolling-window-sblx")
             (:file "host-menu-canvas-sblx")
             (:file "host-window-sblx")
             (:file "host-fonts-sblx"))
             )
   (:module "window-basics/region"
            :components
            ((:file "region")))
   (:module "window-basics/transforms"
            :components
            ((:file "list-transforms"))
             ;;(:file integer-affine-transforms)
             )
   (:module "window-basics/color"
            :components
            ((:file "color-sblx")
             (:file "color")
             (:file "color-table")
             (:file "color-mixin-sblx")
             )
            :depends-on ("window-basics/macros"))
   (:module "window-basics/fonts"
            :components
            ((:file "font")
             (:file "default-fonts-sblx")
             (:file "font-mixin-sblx")
             )
            :depends-on ("window-basics/macros"))
   (:module "window-basics/pen"
            :components
            ((:file "pen")
             ;; Following contains only a list of legal pen-ops ... never used.
             ;; (:file (wb-add-system-extension "pen"))
             (:file "pen-mixin"))
            )
   (:module "window-basics/bitmap"
            :components
            ((:file "bitmap-sblx")
             (:file "bitmap")
             (:file "cursor-sblx")
             (:file "shades")
             (:file "shades-sblx")
             (:file "cursor")
             (:file "patterns")
             )
            :depends-on ("window-basics/pen" "window-basics/macros"))
   (:module "window-basics/monitor"
            :components
            ((:file "screen-sblx")
             (:file "screen")
             (:file "device-sblx")
             (:file "device"))
             )
   (:module "window-basics/mouse"
            :components
            ((:file "mouse-sblx")
             (:file "button-default")
             (:file "canvas-button")
             (:file "mouse")
             (:file "canvas-button-sblx"))
             )
   (:module "window-basics/menus"
            :components
            ((:file "menu-canvas")
             (:file "menu-canvas-sblx")
             (:file "menu")
             (:file "menu-sblx"))
             )
   (:module "window-basics/prompt"
            :components
            ((:file "dialog-items-sblx")
             (:file "dialog-sblx")
             (:file "prompt-sblx")
             (:file "pick-one-sblx")
             (:file "check-items-sblx")
             (:file "collect-input-sblx"))
             )
   (:module "window-basics/redisplay"
          :components
            ((:file "canvas-ops-sblx")
             (:file "canvas-redisplay")
             (:file "canvas-redisplay-sblx"))
             )
   (:module "window-basics/postscript"
            :components
            ((:file "postscript-canvas")
             (:file "ps-font")
             (:file "ps-file")
             (:file "ps-draw")
             (:file "ps-strings")
             (:file "ps-prompt")
             (:file "canvas-to-ps"))
            :depends-on ("macros")
            )
   (:module "window-basics/canvas"
            :components
            ((:file "canvas")
             (:file "bw-canvas-sblx")
             (:file "color-canvas-sblx")
             (:file "make-canvas")
             (:file "key-event")
             (:file "key-event-sblx"))
           )
   (:module "window-basics/draw"
            :components
            ((:file "draw")
             (:file "draw-sblx")
             (:file "strings-sblx")
             (:file "strings")
             (:file "erase"))
            :depends-on ("window-basics/macros" "window-basics/pen" "window-basics/fonts" "window-basics/host"))
   (:module "window-basics/canvas-regions"
            :components
            (;;(:file (wb-add-system-extension "canvas-regions"))
             (:file "canvas-regions")
             (:file "clip-sblx")
             (:file "clip")
             (:file "drag-sblx")
             )
            :depends-on ("window-basics/macros" "window-basics/pen"))
   (:module "window-basics/hardcopy"
            :components
            ((:file "canvas-export-sblx")
             (:file "hardcopy-sblx"))
             )
   (:module "window-basics/fast-graphics"
            :components
            ((:file "point-defs-sblx")
             (:file "points-sblx"
                    :depends-on (#+:ccl "point-defs-mcl"))
             (:file "symbols-sblx" 
                    :depends-on (#+:ccl "point-defs-mcl"))
             (:file "/lines-sblx"
                    :depends-on (#+:ccl "point-defs-mcl"))
             (:file "rotate"))
            :depends-on ("window-basics/macros" "window-basics/pen"))
   (:module "window-basics/surface"
            :components
            ((:file "surface-rotate")
             (:file "surface-update")
             (:file "show-lines")
             (:file "hide-lines")
             (:file "surface-fill"))
             )
   (:module "window-basics/start-windows"
            :components
            ((:file "start-windows-sblx")
             ))
   ))
#|
(defun compile-window-basics (&rest op-on-sys-keyword-pairs
                              &key (verbose T)
                              &allow-other-keys)
  "Compiles the window-basics system using make's~
   operate-on-system's keywords."
  (apply #'operate-on-system 'window-basics 'compile
         :verbose verbose
         :allow-other-keys T op-on-sys-keyword-pairs))

(defun load-window-basics (&rest op-on-sys-keyword-pairs
                           &key (verbose T)
                           &allow-other-keys)
  "Loads the window-basics system using make's~
   operate-on-system's keywords."
  (apply #'operate-on-system 'window-basics 'load
         :verbose verbose
         :allow-other-keys T op-on-sys-keyword-pairs))
|#
|#