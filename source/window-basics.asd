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
;;; THIS IS THE SBCL FILE

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
            (#+:sbcl-linux(:file "macros-sblx")
             #+:aclpc-linux(:file "macros-pc")
             #+:sbcl-linux(:file "operations-sblx")
             #+:aclpc-linux(:file "operations-pc")
             (:file "operations")
             (:file "positions")
             (:file "display-mode"))
           )

   (:module "window-basics/host"
            :components
            ((:file "host-draw-package")
            #+:sbcl-linux(:file "host-system-sblx")
             #+:aclpc-linux(:file "host-system-pc")
             #+:sbcl-linux(:file "host-draw-sblx")
             #+:aclpc-linux(:file "host-draw-pc")
             #+:sbcl-linux(:file "scrolling-window-sblx")
             #+:aclpc-linux(:file "scrolling-window-pc")
             #+:sbcl-linux(:file "host-menu-canvas-sblx")
            ;#+:sbcl-linux(:file"test-color-menu") ;; NEW 15OCT2024 to test replacement
             #+:aclpc-linux(:file "host-menu-canvas-pc")
             #+:sbcl-linux(:file "host-window-sblx")
             #+:aclpc-linux(:file "host-window-pc")
             #+:sbcl-linux(:file "host-fonts-sblx")
             #+:aclpc-linux(:file "host-fonts-pc")
          )
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
            (#+:sbcl-linux(:file "color-sblx")
             #+:aclpc-linux(:file "color-pc")   
             (:file "color")
             (:file "color-table")
             #+:sbcl-linux(:file "color-mixin-sblx")
             #+:aclpc-linux(:file "color-mixin-pc")
             )
            :depends-on ("window-basics/macros")
            )

   (:module "window-basics/fonts"
            :components
            ((:file "font")
             #+:sbcl-linux(:file "default-fonts-sblx")
             #+:aclpc-linux(:file "default-fonts-pc")
             #+:sbcl-linux(:file "font-mixin-sblx")
             #+:aclpc-linux(:file "font-mixin-pc")
             )
            :depends-on ("window-basics/macros")
            )

   (:module "window-basics/pen"
            :components
            ((:file "pen")
             ;; Following contains only a list of legal pen-ops ... never used.
             ;; (:file (wb-add-system-extension "pen"))
             (:file "pen-mixin"))
            )

   (:module "window-basics/bitmap"
            :components
            (        
            #+:sbcl-linux(:file "bitmap-sblx") ;; stubbed 11MAR2022  gwb
             #+:aclpc-linux(:file "bitmap-pc")
             (:file "bitmap") ;; stubbed 11MAR2022  gwb
   ;          ;#+:sbcl-linux(:file "cursor-sblx")
   ;          #+:aclpc-linux(:file "cursor-pc")
             (:file "shades")
             #+:sbcl-linux(:file "shades-sblx")
             #+:aclpc-linux(:file "shades-pc")
   ;          ;(:file "cursor")
             (:file "patterns")
             )
            :depends-on ("window-basics/pen" "window-basics/macros")
           )


   (:module "window-basics/monitor"
      :components
      (#+:sbcl-linux(:file "screen-sblx")
             #+:aclpc-linux(:file "screen-pc")
             (:file "screen")
             #+:sbcl-linux(:file "device-sblx")
             #+:aclpc-linux(:file "device-pc")
             (:file "device")
   )
      )

   (:module "window-basics/mouse"
            :components
            (#+:sbcl-linux(:file "mouse-sblx")
             #+:aclpc-linux(:file "mouse-pc")
             (:file "button-default")
             (:file "canvas-button")
             (:file "mouse")
             #+:sbcl-linux(:file "canvas-button-sblx")
             #+:aclpc-linux(:file "canvas-button-pc"))
             )
   
   (:module "window-basics/menus"
            :components
            ((:file "menu-canvas")     
             #+:sbcl-linux(:file "menu-canvas-sblx") ;does nothing
             #+:aclpc-linux(:file "menu-canvas-pc")
             (:file "menu")
             #+:sbcl-linux(:file "menu-sblx") ;stub version 07MAR2022
             #+:aclpc-linux(:file "menu-pc")
             ;#+:sbcl-linux(:file "quail-plots-canvas-menus-sblx") ;NEW 01SEP2024
             )
             )

   (:module "window-basics/prompt"
            :components
            (#+:sbcl-linux(:file "dialog-items-sblx")
             #+:aclpc-linux(:file "dialog-items-pc")
             #+:sbcl-linux(:file "dialog-sblx")
             #+:aclpc-linux(:file "dialog-pc")
             #+:sbcl-linux(:file "prompt-sblx")
             #+:aclpc-linux(:file "prompt-pc")
             #+:sbcl-linux(:file "pick-one-sblx")
             #+:aclpc-linux(:file "pick-one-pc")
             #+:sbcl-linux(:file "check-items-sblx")
             #+:aclpc-linux(:file "check-items-pc")
             #+:sbcl-linux(:file "collect-input-sblx")
             #+:aclpc-linux(:file "collect-input-pc"))
             )

   (:module "window-basics/redisplay"
          :components
            (#+:sbcl-linux(:file "canvas-ops-sblx")
             #+:aclpc-linux(:file "canvas-ops-pc")
             (:file "canvas-redisplay")
             #+:sbcl-linux(:file "canvas-redisplay-sblx") ; 27AUG2021
             #+:aclpc-linux(:file "canvas-redisplay-pc"))
             )
            
   (:module "window-basics/postscript"
            :components
            ((:file "postscript-canvas")
               (:file "ps-font-sblx") ;30AUG2021
             ;(:file "ps-font")
             (:file "ps-file")
             (:file "ps-draw")
             (:file "ps-strings")
             (:file "ps-prompt")
             (:file "canvas-to-ps"))
            :depends-on ("window-basics/macros")
            )

   
   (:module "window-basics/canvas"
            :components
            ((:file "canvas")
             #+:sbcl-linux(:file "bw-canvas-sblx")
             #+:aclpc-linux(:file "bw-canvas-pc")
             #+:sbcl-linux(:file "color-canvas-sblx")
             #+:sbcl-linux(:file "color-canvas-ops-sblx") ;;New 28AUG2021
             #+:aclpc-linux(:file "color-canvas-pc")
             (:file "make-canvas")
             (:file "key-event")
             #+:sbcl-linux(:file "key-event-sblx")
             #+:aclpc-linux(:file "key-event-pc"))
           )
           

   (:module "window-basics/draw"
            :components
            ((:file "draw")
             #+:sbcl-linux(:file "draw-sblx")
             #+:aclpc-linux(:file "draw-pc")
             #+:sbcl-linux(:file "strings-sblx") ;DONE! 22 November 2019
             #+:aclpc-linux(:file "strings-pc")
             (:file "strings")
             (:file "erase")
             )
            :depends-on ("window-basics/macros" "window-basics/pen" "window-basics/fonts" "window-basics/host"))
            

   (:module "window-basics/canvas-regions"
            :components
            (;;(:file (wb-add-system-extension "canvas-regions"))
             (:file "canvas-regions")
             #+:sbcl-linux(:file "clip-sblx")
             #+:aclpc-linux(:file "clip-pc")
             (:file "clip")
             #+:sbcl-linux(:file "drag-sblx") ;07MAR2022 test
             #+:aclpc-linux(:file "drag-pc")
             )
            :depends-on ("window-basics/macros" "window-basics/pen"))
           
   (:module "window-basics/hardcopy"
            :components
            (#+:sbcl-linux(:file "canvas-export-sblx")
             #+:aclpc-linux(:file "canvas-export-pc")
             #+:sbcl-linux(:file "hardcopy-sblx")
             #+:aclpc-linux(:file "hardcopy-pc"))
             )
        
   (:module "window-basics/fast-graphics"
            :components
            (#+:sbcl-linux(:file "point-defs-sblx")
             #+:aclpc-linux(:file "point-defs-pc")
             #+:sbcl-linux(:file "points-sblx")
             #+:aclpc-linux(:file "points-pc")
             #+:sbcl-linux(:file "symbols-sblx")
             #+:aclpc-linux(:file "symbols-pc" )
             #+:sbcl-linux(:file "lines-sblx")
             #+:aclpc-linux(:file "lines-pc")
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
            (#+:sbcl-linux(:file "start-windows-sblx")
             #+:aclpc-linux(:file "start-windows-pc")
             ))

))
