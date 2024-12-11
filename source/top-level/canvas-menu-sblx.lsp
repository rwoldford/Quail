;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;
;;;
;;;                          quail-canvas-menu-sblx.lsp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;
;;;
;;; 
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     G.W. Bennett 2024.
;;;
;;;--------------------------------------------------------------------------------

;;; McCLIM makes menus in a way which does not seem to fit easily with the
;;; Quail code for making menus under Allegro Common Lisp and 
;;; MacLisp- see source/window-basics/menus/menu.lsp
;;; 
;;; McCLIM uses command-tables to hold commands, which play the role of menu-items,
;;; and other command-tables, which play the role of sub-menus.
;;;
;;; In Quail's menus there may be several menu-items known as black-color.
;;; Here GWB has chosen to codify the particular position of the corresponding
;;; command using Quail's menu structure. For example, there are commands
;;; com-Black-QEHWBSB and com-Black-QEHWPCSB among other com-Blacks.
;;;
;;; Initially Quail expects to show menus for Quail and Plots on its menu-bar
;;; These correspond to the McCLIM command-tables q-command-table and p-command-table.
;;; When displays derived from Quail's canvas class, Quail expects to add a menu
;;; for Canvas along side the other two. There is, therefore, a c-command-table too.

;;; It is (apparently) not possible to add something like the c-command-table to
;;; a menu-bar which has been instantiated with q-command-table and p-command-table.
;;; However, it is possible to replace the command table of q- and p- command-tables
;;; with one holding q- ,p- , and c- command-tables.

;;; Accordingly there are several files in which command-tables are defined.
;;; All are in the source/top-level directory:
;;; quail-menu-sblx.lsp  makes only the Quail menu through the q-command-table
;;; quail-plot-menu-sblx.lsp  makes only the Plots menu through the p-command-table
;;; quail-canvas-menu-sblx.lsp  makes on the Canvas menu through the c-command-table
;;; quail-plot-menu-sblx.lsp  makes the table for Quail and Plots menus
;;; quail-plots-canvas-menus-sbls.lsp  makes the table for Quail, Plots, and Canvas
;;; change-menu-bar-sblx.lsp  contains code for replacing a command-table
;;; Here is mcclim-like code to make the (mcclim)command-tables for the Quail menus

(in-package :wb);(in-package :quail)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(c-command-table)))

   ;;; START C/DP SB Canvas/DisPlay Sub-menu command table   
;;; the commands
(define-qmbar-command com-title-CDPSB ()
  #'(lambda (c)
                         (canvas-set-title c
                               (prompt-user
                                :prompt-string "Enter the title: "
                                :read-type :string
                                :result-type 'string))))

(define-qmbar-command com-bkgcol-CDPSB ()
  #'(lambda (c)
                         (canvas-set-background-color c (prompt-user-for-color))))

(define-qmbar-command com-pencol-CDPSB ()
  #'(lambda (c)
                         (set-pen-color c (prompt-user-for-color))))

(define-qmbar-command com-penwid-CDPSB ()
  #'(lambda (c)
                         (set-pen-width
                          c
                          (prompt-user
                           :prompt-string "Enter the new width (in pixels): "
                           :result-type 'integer
                           :read-type :eval))))

;;; make the command table
(make-command-table 'cdpsb-command-table :errorp nil
  :menu '(("Title" :command com-title-CDPSB)
    ("Background color" :command com-bkgcol-CDPSB)
    ("Pen color" :command com-pencol-CDPSB)
    ("Pen width" :command com-penwid-CDPSB)))                                                                                                             
   ;;; END CDPSB command-table

   ;;; START C/FO SB Canvas Fonts Sub-menu command table
;;; the commands
(define-qmbar-command com-font-CFOSB ()
  #'(lambda (c)
                         (let ((f-copy (copy-canvas-font (canvas-font c))))
                           (set-canvas-font-name
                            f-copy
                            (first (prompt-for-items (canvas-font-names))))
                           (setf (canvas-font c) f-copy))))

(define-qmbar-command com-fostyle-CFOSB ()
  #'(lambda (c)
                         (let ((f-copy (copy-canvas-font (canvas-font c))))
                           (set-canvas-font-style
                            f-copy
                            (prompt-for-items
                             (canvas-font-styles) :selection-type :disjoint))
                           (setf (canvas-font c) f-copy))))

(define-qmbar-command com-fosize-CFOSB ()
  #'(lambda (c)
                         (let ((f-copy (copy-canvas-font (canvas-font c))))
                           (set-canvas-font-size
                            f-copy
                            (prompt-user
                             :prompt-string "Enter the new font size (1 to 127): "
                             :result-type 'integer
                             :read-type :eval))
                           (setf (canvas-font c) f-copy))))
;;; make the command table
(make-command-table 'cfosb-command-table :errorp nil
  :menu '(("Font" :command com-font-CFOSB)
    ("Font style" :command com-fostyle-CFOSB)
    ("Font size" :command com-fosize-CFOSB)))                                                                                 
   ;;; END C/FOSB command table

;;; START the C Canvas command table
;;; the commands
(define-qmbar-command com-redisplay-C ()
  #'(lambda (c) (redisplay c)))

(define-qmbar-command com-export-C ()
  #'canvas-export)

(define-qmbar-command com-print-C ()
  #'canvas-hardcopy)

(define-qmbar-command com-draw-C ()
  #'(lambda (c)
             (let (c?)
               (loop
                 (when (mouse-down-p)
                   (setf c? (which-canvas (screen-mouse-position)))
                   (if (not (eq c c?)) (return))
                   (let* ((mouse-pos (mouse-position c))
                          (old-x (position-x mouse-pos))
                          (old-y (position-y mouse-pos))
                          new-x new-y)
                     (do ((i 1 (+ i 1)))
                         ((not (mouse-down-p)))
                       (setf mouse-pos (mouse-position c))
                       (setf new-x (position-x mouse-pos))
                       (setf new-y (position-y mouse-pos))
                       (canvas-draw-line c old-x old-y new-x new-y)
                       (setf old-x new-x)
                       (setf old-y new-y))
                     (return)))))))

(define-qmbar-command com-clear-C ()
  #'canvas-clear)

(define-qmbar-command com-save-C ()
  #'save-value)

(define-qmbar-command com-insp-C ()
  #'inspect )                                 
;;; make the table
(make-command-table 'c-command-table :errorp nil
  :menu '(("Redisplay" :command com-redisplay-C)
    ("Display parameters" :menu cdpsb-command-table)
    ("Export canvas" :command com-export-C)
    ("Print canvas" :command com-print-C)
    ("Canvas fonts" :menu cfosb-command-table)
    ("Draw on canvas" :command com-draw-C)
    ("Clear canvas" :command com-clear-C)
    ("Save canvas" :command com-save-C)
    ("Inspect canvas" :command com-insp-C)))

;;; END  C command table