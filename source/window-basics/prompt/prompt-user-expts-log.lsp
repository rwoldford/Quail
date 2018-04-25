;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prompt-user-expts-log.lsp
;;; started 18oct2005
;;; holding a log of the experiments
;;; code for which is in prompt-user-expts.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;[1] package stuff OK
; cut+paste build-prompt-user and execute it - OK fn defined
; cut+paste create-prompt-user and execute it - OK fn defined
; cut+paste prompt-user and execute it - OK fn defined

; create *element4*
;#<MULTI-ITEM-LIST CG.ITEM-LIST:MULTI-ITEM-LIST @ #x20c6034a> - ok
; create *select4*
;#<STATIC-TEXT CG.STATIC-TEXT:STATIC-TEXT @ #x20ccc532> - ok
; use make-window to show this
;#<DIALOG #:G829 in Listener 1 @ #x20bd83b2> - ok
; highlight Second and hit Select
;* Error: #<Interpreted Function (unnamed) @ #x20c86f92> got 2 args, wanted 0 args.
;* [condition type: PROGRAM-ERROR]
; this has to do with on-click - redefined the lambda to have 2 args - why ?
; highlight Second and hit Select
; Results is ("Second") - which is good. Note that there is no difficulty
; in closing the dialog and then re-defining it.
; The dialog does not close automatically at the moment.

; auxilairy functions from prompt-pc.lsp
; HOW-MANY, LIST-OF-LENGTHS, MAX-TEXT-SEGMENT - ok

; execute build-prompt-user
; WB(27): (build-prompt-user)
;* (149 194 #<STATIC-TEXT :TEXT-D @ #x20ce80e2>
;*  #<EDITABLE-TEXT :RESP-D @ #x20ce913a> #<BUTTON :SELECT-D @ #x20ce9f4a>
;*  #<CANCEL-BUTTON :CANCEL-D @ #x20ce946a>)
; which should match
; (list overall-width overall-height text-dialog response-dialog select-button cancel-button)
; and seems to do so while first -> sixth returns the items

:execute create-prompt-user
;WB(41): (create-prompt-user)
;* Error: attempt to call `CANVAS-FONT-TO-HOST-FONT' which is an undefined
;*      function.
;* [condition type: UNDEFINED-FUNCTION]
; so we probably need to get all of Quail to try this out since there are just too many
; bits and pieces missing  - indeed: this fn is in fonts.lsp and it calls some :h-draw
; stuff.

; Try this
(cg::make-window (gensym)
  :class 'cg::dialog
  :owner (cg::development-main-window cg::*system*)
  :title "A Prompt Dialog 1"
  :visible-box (cg::make-box 0 0 150 200)
  :widgets (cddr (build-prompt-user)))
; it does produce a dialog 
; the buttons don't respond since there are no :on-click functions associated
; with the widgets

; put an on-click in cancel-button to abort if checked
; I now get a reaction but the dialog does not automatically close.

; Comment out create-prompt-user, redefine prompt-user itself
; without the :on-click stuff for now to see whether things are carried through.
; After lunch with DMJ, I think.




