
(in-package :wb)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;
;;hardcopy-mcl.lisp
;;
;;
;;copyright 1988-89 Apple Computer, Inc.
;;
;; defines a very basic printing routine for windows
;;
;; Code taken from Apple and Bill Kornfeld and played with a bit to get
;; something working.  Trying to change the wptr and
;; then doing a view-draw-contents fails --- LISP unexpextantly quits.
;; View-draw-contents without changing the window pointer
;; cases a print job to be sent to the printer but nothing comes out.  Using a
;; print-contents function that just makes the
;; appropriate quickdraw calls seems to wrork ok. The basic print-contents
;; functions for text, views and windows is defined
;; here. Some extra print-contents functions for other items is defined in
;; odin-printing.lisp -- DEH 6/20/91
 

#|
From: LAu@a.darpa.mil
Message-Id: <9108091602.AA15191@a.darpa.mil>
Received: by a.darpa.mil (5.61/5.61)
        id AA15191; Fri, 9 Aug 91 11:02:53 -0500
Date: Fri, 9 Aug 91 11:54:00 EST
To: info-macl@cambridge.apple.com
Cc: LAu@a.darpa.mil
Subject: Printing Interface Tools Objects
 
A number of MCL users have expressed an interest in high-level support
to print interface tools objects.  It would be very nice to just pulldown
the File menu and select print.
 
The code to do this is not difficult to write,  and it would greatly enhance
the rapid-prototyping abilities of MCL because hardcopy is a great way to get
people to agree on the look and feel of a window design.
 
I've been playing with some code passed on to me by David Hirsch at
Price Waterhouse,  and with some minor enhancements I think it would be a good
thing to stash in the /pub/MCL2/contrib directory.  It needs to be able to
print out more than one page,  and to handle the other dialog-item types such
as radio button,  sequence,  and array.  Perhaps we could put the code out
there,  and people could just add these enhancements and re-publish the code.
 
                                    Lawrence Au
 
Here's the code:
 
|#



(export '(canvas-hardcopy hardcopy-bitmap  print-contents))  ;; cbh


;;;*logical-directory-alist*
;;(load (probe-file "library;records.lisp"))
;; (load (probe-file "library;traps.lisp"))

(defconstant $PrintErr #x944)
(defconstant $prJob.bJDocLoop (+ 62 6))
(defconstant $iPrStatSize 26)
(defconstant $bSpoolLoop 1)
(defconstant $err-printer 94)
(defconstant $err-printer-load 95)
(defconstant $err-printer-start 97)
;;(load (probe-file "interfaces;types.lisp"))
;;(load (probe-file "interfaces;quickdraw.lisp"))
;;(load (probe-file "library;quickdraw.lisp"))




 
(defun prchk (&optional (errnum $err-printer)
              &aux (print-error (ccl:%get-signed-word (ccl:%int-to-ptr
              $PrintErr))))
   (unless (zerop print-error)
           (error (format NIL "errnum = ~s  ~&print-error = ~s" errnum print-error)))
)
 

(defmethod print-schedule ((w window))
  (unwind-protect
    (with-cursor *arrow-cursor*
      (_PrOpen)
      (prchk $err-printer-load)
      (let ((pRec (get-print-record)))
        (when (and (_PrStlDialog :ptr pRec :boolean)
                   (_PrJobDialog :ptr pRec :boolean))
          (let ((*hc-page-open-p* nil)
                (ccl::*inhibit-error* t)
                err)
            
            ;_PrOpenDoc puts up a dialog window which causes the event system
            ;to get confused.  So we do the whole thing without interrupts, and
            ;make sure to clean up before handling errors.
            
            (declare (special *hc-page-open-p* ccl::*inhibit-error*
                              host-draw:*hardcopy-ptr*))
            (setq err
                  (progn ;catch-error-quietly
                    (without-interrupts
                     ;;(let (
                     (setq host-draw:*hardcopy-ptr*
                           (_PrOpenDoc :ptr pRec
                                       :ptr (ccl:%null-ptr)
                                       :ptr (ccl:%null-ptr) :ptr))
                     ;;)
                     (with-port host-draw:*hardcopy-ptr*
                       (unwind-protect
                         (with-dereferenced-handles ((ppRec pRec))
                           pprec
                           (prchk $err-printer-start)
                           (unwind-protect
                             (progn
                               (_PrOpenPage :ptr host-draw:*hardcopy-ptr*
                                            :ptr (ccl:%null-ptr))
                               ;; Do it!
                               (with-port host-draw:*hardcopy-ptr* (print-contents w)))
                             (_PrClosPage :ptr host-draw:*hardcopy-ptr*)
                             ))
                         (progn
                           (_PrClosDoc :ptr host-draw:*hardcopy-ptr*)
                           (setq host-draw:*hardcopy-ptr* NIL))))
                     ;;) ;; end of let
                     
                     (when t
                       ;;(eq (ccl:%hget-byte pRec $prJob.bJDocLoop)
                       ;;this test always seems to fail???!!!!
                       ;;$bSpoolLoop)
                       ;;(format t "~ccl:%Docloop ~S" (ccl:%hget-byte pRec $prJob.bJDocLoop))
                       (prchk)
                       (ccl:%stack-block ((StRec $iPrStatSize))
                                         (_PrPicFile :ptr pRec
                                                     :ptr (ccl:%null-ptr)
                                                     :ptr (ccl:%null-ptr)
                                                     :ptr (ccl:%null-ptr)
                                                     :ptr StRec))
                       (prchk)))))
            ;;; end of setq err
            t))))
    ;;; end of with-cursor *arrow-cursor*
    (progn
      (_PrClose)
      (setq host-draw:*hardcopy-ptr* NIL))
    )
  )
 

;;;------------------  The basic print-contents functions----------------------

(defmethod print-contents ((v window))
  "a window draws a box around itself and
   then asks its subviews to print themselves"
  (let ((size (view-size v))
        (top 0)
        (left 0)
        bottom right)
       (setq bottom (+ top (h-draw:point-y size))
             right  (+ left (h-draw:point-x size)))
       ;;first frame it
       (h-draw::with-rectangle-arg (r left top right bottom) (_FrameRect :ptr r))
       (dovector (sv (view-subviews v))
          (print-contents sv)))
)
 
(defmethod print-contents ((v view))
  "a view just asks its subviews to print themselves"
    (dovector (sv (view-subviews v))
      (print-contents sv)))
 
(defmethod print-contents ((sv ccl::basic-editable-text-dialog-item))
  "editable text uses textbox -- take into account font and the justification"
  (let ((window (view-window sv))
        (container (view-container sv))
        (pos (view-position sv))
        (size (view-size sv))
        cpos
        top left bottom right
        font-face mode-size)
       (multiple-value-setq (font-face mode-size)
       (view-font-codes sv))
       (setq cpos (convert-coordinates pos container window))
       (setq top (h-draw:point-y cpos)
             left (h-draw:point-x cpos))
       (setq bottom (+ top (h-draw:point-y size))
             right (+ left (h-draw:point-x size)))
       (with-font-codes font-face mode-size
         (h-draw::with-rectangle-arg (r left top right bottom)
           (with-pstrs ((pstring (dialog-item-text sv)))
             (_TextBox :ptr (ccl:%inc-ptr pstring 1)
                       :long (length (dialog-item-text sv))
                       :ptr r
                       :word (slot-value sv 'ccl::text-justification)))))))
 
(defmethod print-contents ((sv static-text-dialog-item))
  "static text uses textbox -- take into account font and the justification"
  (let ((window (view-window sv))
        (container (view-container sv))
        (pos (view-position sv))
        (size (view-size sv))
        cpos
        top left
        bottom right
        font-face mode-size)
       (multiple-value-setq (font-face mode-size)
       (view-font-codes sv))
       (setq cpos (convert-coordinates pos container window))
       (setq top (h-draw:point-y cpos)
             left (h-draw:point-x cpos))
       (setq bottom (+ top (h-draw:point-y size))
             right (+ left (h-draw:point-x size)))
       (with-font-codes font-face mode-size
          (h-draw::with-rectangle-arg (r left top right bottom)
             (with-pstrs ((pstring (dialog-item-text sv)))
                (_TextBox :ptr (ccl:%inc-ptr pstring 1)
                          :long (length (dialog-item-text sv))
                          :ptr r
                          :word (slot-value sv 'ccl::text-justification)))))))
 
(defmethod print-contents ((sv button-dialog-item))
  (let ((window (view-window sv))
        (container (view-container sv))
        (pos (view-position sv))
        (size (view-size sv))
        cpos
        top left bottom right
        font-face mode-size)
       (multiple-value-setq (font-face mode-size)
         (view-font-codes sv))
 
       (setq cpos (convert-coordinates pos container window))
       (setq top (h-draw:point-y cpos)
             left (h-draw:point-x cpos))
 
       (setq bottom (+ top (h-draw:point-y size))
             right (+ left (h-draw:point-x size)))
       (h-draw::with-rectangle-arg (r left top right bottom)
          (with-font-codes font-face mode-size
             (with-pstrs ((pstring (dialog-item-text sv)))
                 (_TextBox :ptr (ccl:%inc-ptr pstring 1)
                           :long (length (dialog-item-text sv))
                           :ptr r :word 1)))
          ;;; end of with-font-codes
          (rlet ((old-pen-state :penstate) (new-pen-state :penstate))
                (_GetPenState :ptr old-pen-state)
                (rset new-pen-state :penstate.pnLoc
                     (rref old-pen-state :penState.pnLoc))
                (rset new-pen-state :penstate.pnSize #@(1 1))
                (rset new-pen-state :penstate.pnMode :PATOR)
                (rset new-pen-state :penstate.pnPat *black-pattern*)
                (_SetPenState :ptr new-pen-state)
           ;;(decf (rref r :rect.left)
           ;;      (floor (dialog-item-width-correction sv) 2))
           ;;(incf (rref r :rect.right)
           ;;      (floor (dialog-item-width-correction sv) 2))
           (_FrameRoundRect :ptr r :word 10 :word 6)
           (_SetPenState :ptr old-pen-state)))))
 
(defmethod print-contents ((sv simple-view))
  "Default: if all else fails do nothing"
  t
)
 
#|
 
(setq w (make-instance 'window
            :window-title "HI there"
            :view-size #@(300 300)
            :view-subviews
               (list (make-instance 'view
                   :view-position #@(20 20)
                   :view-size #@(150 130)
                   :view-subviews
                       (List (make-instance 'static-text-dialog-item
                                 :view-position #@(10 10)
                                 :view-size #@(130 40)
                                 :view-font '("Helvetica" :srcor :bold 12)
                                 :dialog-item-text
                                    "how now said the big brown cow")
                             (make-instance 'static-text-dialog-item
                                 :view-position #@(10 70)
                                 :view-size #@(130 60)
                                 :view-font '("Geneva" :srcor :underline 14)
                                 :dialog-item-text
                   "there is a bunch of green cheese here on the moon"))))))
 
(print-schedule w)
 
(setf test-window
   (MAKE-INSTANCE 'COLOR-DIALOG
               :WINDOW-TYPE :DOCUMENT-WITH-ZOOM :VIEW-POSITION ':CENTERED
               :VIEW-SIZE #@(918 708)
               :VIEW-FONT '("Chicago" 12 :SRCOR :PLAIN)
               :VIEW-SUBVIEWS
               (LIST (MAKE-DIALOG-ITEM 'STATIC-TEXT-DIALOG-ITEM
                                       #@(13 9)
                                       #@(56 16)
                                       "Untitled"
                                       NIL)
                     (MAKE-DIALOG-ITEM 'EDITABLE-TEXT-DIALOG-ITEM
                                       #@(15 25)
                                       #@(84 16)
                                       "Untitled"
                                       NIL
                                       :ALLOW-RETURNS NIL)
                     (MAKE-DIALOG-ITEM 'BUTTON-DIALOG-ITEM
                                       #@(15 47)
                                       #@(62 16)
                                       "Untitled"
                                       NIL
                                       :DEFAULT-BUTTON NIL)
                     (MAKE-DIALOG-ITEM 'EDITABLE-TEXT-DIALOG-ITEM
                                       #@(381 683)
                                       #@(114 16)
                                       "bottom center"
                                       NIL
                                       :ALLOW-RETURNS NIL)
                     (MAKE-DIALOG-ITEM 'EDITABLE-TEXT-DIALOG-ITEM
                                       #@(11 688)
                                       #@(84 16)
                                       "bottom left"
                                       NIL
                                       :ALLOW-RETURNS NIL)
                     (MAKE-DIALOG-ITEM 'EDITABLE-TEXT-DIALOG-ITEM
                                       #@(375 20)
                                       #@(84 16)
                                       "top center"
                                       NIL 
                                       :ALLOW-RETURNS NIL)
                     (MAKE-DIALOG-ITEM 'EDITABLE-TEXT-DIALOG-ITEM
                                       #@(799 676)
                                       #@(84 16)
                                       "bottom right"
                                       NIL
                                       :VIEW-FONT
                                       '("New Century Schlbk"
                                         12
                                         :SRCOR
                                         :PLAIN)
                                       :ALLOW-RETURNS NIL)
                     (MAKE-DIALOG-ITEM 'EDITABLE-TEXT-DIALOG-ITEM
                                       #@(818 20)
                                       #@(84 16)
                                       "top right"
                                       NIL
                                       :VIEW-FONT
                                       '("New Century Schlbk"
                                         12
                                         :SRCOR
                                         :PLAIN)
                                       :ALLOW-RETURNS NIL)))
)
(print-schedule test-window)
 
 
|#

;; added by CBH , rwo

(defgeneric hardcopy-bitmap (canvas &key left top width height)
  (:documentation "Produces a hardcopy of the canvas"))

(defgeneric canvas-hardcopy (canvas &key left top width height)
  (:documentation "Produces a hardcopy of the canvas,~
                   or a selected region in canvas coordinates"))


(defmethod canvas-hardcopy ((self canvas) &key left top width height)
  (if (or left top width height)
       (hardcopy-bitmap self
                        :left left
                        :top top
                        :width width
                        :height height)
       (print-schedule self)))

(defmethod canvas-export ((self canvas) &key left top width height filetype)
  (unless filetype
    (setf filetype (pick-one  (list :postscript :pict))))
  (case
    filetype
    (:postscript
     (canvas-to-ps self))
    (:pict 
     (canvas-to-pict-file self))))

(defmethod CCL::WINDOW-SAVE ((window canvas))
  (canvas-export window))

(defmethod CCL::WINDOW-SAVE-copy-as  ((window canvas) &optional opt-0)
  (canvas-export window))

(defmethod window-hardcopy ((self canvas) &optional ignore )
  (declare (ignore ignore))
  (canvas-hardcopy self))


(defmethod print-contents ((self canvas))
  (let ((old-mode (display-mode-of self)))
    (unwind-protect
      (progn ()
             (setf (display-mode-of self) :mcl-printer)
             (redisplay self))
      (setf (display-mode-of self) old-mode)
      )
    )
  )



(defmethod hardcopy-bitmap ((w canvas) &key left top width height)
  (setq left (or left 72))
  (setq top (or top 72))
  (setq width (or width (canvas-width w)))
  (setq height (or height (canvas-height w)))
  (unwind-protect
    (with-cursor *arrow-cursor*
      (_PrOpen)
      (prchk $err-printer-load)
      (let ((pRec (get-print-record)))
        (when (and (_PrStlDialog :ptr pRec :boolean)
                   (_PrJobDialog :ptr pRec :boolean))
          (let ((*hc-page-open-p* nil) (ccl::*inhibit-error* t) err)
            
            ;_PrOpenDoc puts up a dialog window which causes the event system
            ;to get confused.  So we do the whole thing without interrupts, and
            ;make sure to clean up before handling errors.
            (declare (special *hc-page-open-p* ccl::*inhibit-error*
                              host-draw:*hardcopy-ptr*))
            
            (setq err (progn ;catch-error-quietly
                        (window-select w) (redisplay w)
                        (rlet ((source-rect :rect
                                            :top 0 :left 0
                                            :bottom (canvas-height w)
                                            :right (canvas-width w))
                               (target-rect :rect 
                                            :top top :left left
                                            :bottom (+ top height)
                                            :right (+ left width)))
                          (without-interrupts
                           (let* (;;(window-ptr (wptr w))
                                  (screen-bitmap (rref (wptr w) grafport.portbits))
                                  (temp-bitmap (h-draw::make-bitmap target-rect))
                                  host-draw:*hardcopy-ptr* target-bitmap)
                             (h-draw::copy-bits screen-bitmap temp-bitmap
                                                source-rect target-rect   )
                             
                             (setq host-draw:*hardcopy-ptr* (_PrOpenDoc :ptr pRec :ptr
                                                                        (ccl:%null-ptr) :ptr (ccl:%null-ptr) :ptr))
                             (setq target-bitmap (rref host-draw:*hardcopy-ptr* grafport.portbits))
                             (with-port host-draw:*hardcopy-ptr*
                               (unwind-protect
                                 (with-dereferenced-handles ((ppRec pRec))
                                   pprec
                                   (prchk $err-printer-start)
                                   (unwind-protect
                                     
                                     (_PrOpenPage :ptr host-draw:*hardcopy-ptr*
                                                  :ptr (ccl:%null-ptr))
                                     
                                     (h-draw::copy-bits temp-bitmap target-bitmap
                                                        target-rect target-rect   )
                                     (_PrClosPage :ptr host-draw:*hardcopy-ptr*)
                                     ))
                                 (_PrClosDoc :ptr host-draw:*hardcopy-ptr*))))
                           ;;; end of let
                           
                           (when t
                             ;;(eq (ccl:%hget-byte pRec $prJob.bJDocLoop)
                             ;;this test always seems to fail???!!!!
                             ;;$bSpoolLoop)
                             ;;(format t "~ccl:%Docloop ~S" (ccl:%hget-byte pRec $prJob.bJDocLoop))
                             (prchk)
                             (ccl:%stack-block ((StRec $iPrStatSize))
                                               (_PrPicFile :ptr pRec
                                                           :ptr (ccl:%null-ptr)
                                                           :ptr (ccl:%null-ptr)
                                                           :ptr (ccl:%null-ptr)
                                                           :ptr StRec))
                             (prchk))))))
            ;;; end of setq err
            t))))
    ;;; end of with-cursor *arrow-cursor*
    (_PrClose))
  )

;;-*- Mode: Lisp; Package: CCL -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; picture-files.lisp
;; Copyright 1990-1994, Apple Computer, Inc.
;; Copyright 1995 Digitool, Inc.


; Examples of reading and writing picture files.
; Adapted from the code on page V-88 of Inside Macintosh.
; If you draw a PICT2 pict to a color window with a palette
; (e.g. a PALETTE-WINDOW below), it will copy the color table
; to the palette.  Does not yet have any way to clean up your
; desktop.  You can always close all the color windows, then
; zoom the listener to fill the screen and back.

; See the function DISPLAY-PICT-FILE for an example of use.

;;;;;;;
;;
;; Modification history
;;
;; 10/18/95 bill open-pict-output-file writes 10 bytes, not 8 for the header,
;;               and it initializes *pict-output-handle* correctly so that
;;               *put-pict-data* will update the length field and the pict
;;               writing code will word-align the opcodes.
;; ------------- 3.0
;; 11/17/93 bill save-pict-to-file, :creator keyword to with-pict-output-file
;; 03/18/93 bill load-pict-file. Warnings on use of open-pict-input-file
;; 04/17/92 bill Steve Miner's def-load-pointers for *std-bits-proc*
;; ------------- 2.0
;; 12/18/91 bill (from STEVE.M) remove %getport, it's in the kernel.
;;          scale-point -> scale-lisp-point so it won't conflict with
;;          ccl:library;quickdraw.lisp.;;               
;; 10/08/91 bill move to CCL package
;;

#-ccl-2
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "This code expects MCL 2.0 or later."))

;;(in-package :ccl)

(require :mac-file-io)              ; high-level File I/O ala Inside Macintosh

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(with-pict-input-file with-pict-output-file display-pict-file
             palette-window load-pict-file)))

(defvar *pict-input-pb* nil)

(defpascal *get-pict-data* (:ptr dataPtr :word byteCount)
  (FSRead *pict-input-pb* byteCount dataPtr 0 nil))

(defvar *pict-output-pb* nil)
(defvar *pict-output-handle* nil)

(defpascal *put-pict-data* (:ptr dataPtr :word byteCount)
  (FSWrite *pict-output-pb* byteCount dataPtr 0 nil)
  (let ((handle *pict-output-handle*))
    (when handle
      (rset handle :picture.picsize 
            (+ byteCount (rref handle :picture.picsize))))))

; Color palette stuff.
; Note that this initial palette is all black.
; It really should be initialized to the default
; system color table for the device with the most bits.
; It works because set the palette from a PICT that is
; drawn here.
(defun add-palette (window)
  (when (window-color-p window)
    (let ((wptr (wptr window)))
      (when (%null-ptr-p (#_GetPalette wptr))
        (#_SetPalette 
         wptr
         (#_NewPalette 256 (%null-ptr) 2 0 )
         nil)))))

(defun remove-palette (window)
  (when (window-color-p window)
    (let* ((wptr (wptr window))
           (palette (#_GetPalette wptr)))
      (declare (dynamic-extent palette))
      (unless (%null-ptr-p palette)
        (#_SetPalette  wptr (%null-ptr) nil)
        (#_DisposePalette palette)))))
        
(defclass palette-window (window) ()    ; a class for 8-bit graphics
  (:default-initargs
    :color-p t
    :grow-icon-p nil))

(defmethod initialize-instance :after ((w palette-window) &key)
  (add-palette w))

(defmethod window-close :before ((w palette-window))
  (remove-palette w))


;;; ********** This has to appear after the macro with-pict-outout-file
;;; and so is farther down in this file
#|(defun canvas-to-pict-file (canvas &key (filepathname NIL))
  "Writes out the canvas contents to a PICT file via~
   the draw commands necessary to reproduce the canvas as displayed.  ~
   (:key ~
   (:arg filepathname NIL Complete pathname for the saved PICT file.  If NIL, ~
                      then the user is prompted for a filename.))"
  .... ETCETERA SEE BELOW) |#

(defvar *std-bits-proc* nil)
(def-load-pointers *std-bits-proc* ()
  (setq *std-bits-proc* (%int-to-ptr 0)))

(defvar *bits-proc-cnt* 0)
(defvar *palette-changes* 0)

(defpascal *bits-proc* (:ptr srcBits :ptr srcRect :ptr dstRect
                             :word mode :ptr rgnHandle)
   (incf *bits-proc-cnt*)
   (let* ((port (ccl::%getPort)))
     (declare (dynamic-extent port))
     (when (and (logbitp 15 (rref srcBits :pixMap.rowBytes :storage :pointer))
                (logbitp 15 (rref port :cGrafport.portVersion)))
       (let ((palette (#_GetPalette port)))
         (declare (dynamic-extent palette))
         (unless (%null-ptr-p palette)
           (incf *palette-changes*)
           (#_CTab2Palette (rref srcBits :pixMap.pmTable :storage :pointer)
                           palette
                           2  #x0000)   ; tolerant usage, no tolerance
           (#_ActivatePalette port)))))
   (ff-call *std-bits-proc*
                 :ptr srcBits :ptr srcRect :ptr dstRect :word mode :ptr rgnHandle))



(eval-when (:compile-toplevel :execute)
(defrecord QDProcs
  (textProc :pointer)
  (lineProc :pointer)
  (rectProc :pointer)
  (rRectProc :pointer)
  (ovalProc :pointer)
  (arcProc :pointer)
  (polyProc :pointer)
  (rgnProc :pointer)
  (bitsProc :pointer)
  (commentProc :pointer)
  (txMeasProc :pointer)
  (getPicProc :pointer)
  (putPicProc :pointer)
  (opcodeProc :pointer)
  (newProc1 :pointer)
  (newProc2 :pointer)
  (newProc3 :pointer)
  (newProc4 :pointer)
  (newProc5 :pointer)
  (newProc6 :pointer))

(defconstant $CQDProcs-size (record-length :QDProcs))
(defconstant $QDProcs-size (- $CQDProcs-size (* 7 4)))
)

(defvar *pict-input-grafProcs* nil)

; Offsets in the GrafProcs structure that we allocate for storing our state
(defconstant $gpWptr 0)
(defconstant $gpPictHand 4)
(defconstant $gpOldGrafProcs 8)
(defconstant $gpHeaderSize 12)
(defconstant $pictureSize 10)            ; length of a PICTURE header

; Returns a PICTURE handle on which you can call _DrawPicture to the window.
; If error, signal if errorp is true, or return two values, NIL and the
; error number. Note that you should never use this except when balanced
; with a call to close-pict-file (the easiest way to do this is with-pict-input-file).
; It modifies the :grafport.grafProcs field of window's wptr. close-pict-file does
; the restoration. Also, only one pict file at a time can be open with this file.
; #_DrawPicture on the resultant picture handle will work only to the specified
; window.
(defun open-pict-input-file (filename window &optional (errorp t))
  (let ((old-pb *pict-input-pb*)
        pb pict-hand errnum grafProcs)
    (when old-pb
      (error "A picture input file is already open"))
    (unwind-protect
      (progn
        (setq *pict-input-pb* t)            ; grab it
        (let* ((wptr (wptr window))
               (color-p (window-color-p window))
               (size (if color-p $CQDProcs-size $QDProcs-size)))
          (setq grafProcs (#_NewPtr :errchk (+ $gpHeaderSize size)))
          (setq pict-hand (#_NewHandle :errchk $pictureSize))
          (multiple-value-setq (pb errnum) (FSOpen filename nil 0 errorp))
          (if (not pb)
            (values nil errnum)
            (let ((oldGrafProcs (rref wptr :grafport.grafProcs))
                  (newGrafProcs (%inc-ptr GrafProcs $gpHeaderSize)))
              (declare (dynamic-extent oldGrafProcs newGrafProcs))
              (%put-ptr grafProcs wptr $gpWptr)
              (%put-ptr grafProcs pict-hand $gpPictHand)
              (%put-ptr grafProcs oldGrafProcs $gpOldGrafProcs)
              (if (%null-ptr-p oldGrafProcs)
                (if color-p
                  (#_SetStdCProcs newGrafProcs)
                  (#_SetStdProcs newGrafProcs))
                (#_BlockMove oldGrafProcs newGrafProcs size))
              (setFpos pb 512)          ; skip MacDraw header block
              (with-pointers ((pict pict-hand))
                (FSRead pb $pictureSize pict))   ; read size & picture frame
              (rset newGrafProcs :QDProcs.getPicProc *get-pict-data*)
              (when (window-color-p window)
                (let ((bitsProc (rref newGrafProcs :QDProcs.bitsProc)))
                  (declare (dynamic-extent bitsProc))
                  (unless (eql *bits-proc* bitsProc)
                    (%setf-macptr *std-bits-proc* bitsProc)
                    (rset newGrafProcs :QDProcs.bitsProc *bits-proc*))))
              (setq *pict-input-GrafProcs* grafProcs
                    *pict-input-pb* pb)
              (rset wptr :grafport.grafProcs newGrafProcs)
              pict-hand
              ))))
      (when (eq t *pict-input-pb*)
        (if pb (FSClose pb))
        (setq *pict-input-pb* nil)
        (when grafProcs
          (#_DisposePtr grafProcs))
        (when pict-hand
          (#_DisposeHandle pict-hand))))))

(defun close-pict-input-file (pict-hand)
  (let ((grafProcs *pict-input-GrafProcs*)
        (pb *pict-input-pb*))
    (unless pb
      (error "No picture input file open."))
    (unless (eql pict-hand (%get-ptr grafProcs $gpPictHand))
      (error "~s is not the pict-hand returned from open-pict-input-file"
             pict-hand))
    (let ((wptr (%get-ptr grafProcs $gpWptr))
          (oldGrafProcs (%get-ptr grafProcs $gpOldGrafProcs)))
      (rset wptr :grafport.GrafProcs oldGrafProcs)
      (#_DisposePtr grafProcs)
      (#_DisposeHandle pict-hand)
      (FSClose pb)
      (setq *pict-input-GrafProcs* nil
            *pict-input-pb* nil))))

(defmacro with-pict-input-file ((pict-hand filename window) &body body)
  `(let ((,pict-hand (open-pict-input-file ,filename ,window)))
     (unwind-protect
       (progn ,@body)
       (close-pict-input-file ,pict-hand))))


(defvar *pict-output-GrafProcs* nil)

; Picture output to a file.
; Sets up to output a picture to the file named filename.
; Picture output will be done on the given window in the picture
; frame described by the two points topleft & botright
; Same limitations as for open-pict-input-file. You need to balance
; open-pict-output-file calls with close-pict-output-file, and you
; can only open one pict output file at a time.
(defun open-pict-output-file (filename window topleft botright
                                       &key creator)
  (unless creator
    (setq creator  "dPro"))             ; MacDraw Pro
  (let ((old-pb *pict-output-pb*)
        pb pict-hand grafProcs)
    (when old-pb
      (error "A picture output file is already open"))
    (unwind-protect
      (progn
        (setq *pict-output-pb* t)            ; grab it
        (create-file filename
                     :mac-file-type "PICT"
                     :mac-file-creator creator)
        (let* ((wptr (wptr window))
               (color-p (window-color-p window))
               (size (if color-p $CQDProcs-size $QDProcs-size)))
          (setq grafProcs (#_NewPtr :errchk (+ $gpHeaderSize size)))
          (setq pb (FSOpen filename t))
          (let ((oldGrafProcs (rref wptr :grafport.grafProcs))
                (newGrafProcs (%inc-ptr GrafProcs $gpHeaderSize)))
            (declare (dynamic-extent oldGrafProcs newGrafProcs))
            (%put-ptr grafProcs wptr $gpWptr)
            (%put-ptr grafProcs oldGrafProcs $gpOldGrafProcs)
            (if (%null-ptr-p oldGrafProcs)
              (if color-p
                (#_SetStdCProcs newGrafProcs)
                (#_SetStdProcs newGrafProcs))
              (#_BlockMove oldGrafProcs newGrafProcs size))
            (%stack-block ((data (max 4 $PictureSize) :clear t))
              (dotimes (i (/ 512 4))
                (FSWrite pb 4 data)
                (FsWrite pb $PictureSize data)))
            (rset newGrafProcs :QDProcs.putPicProc *put-pict-data*)
            (unwind-protect
              (progn
                (rset wptr :grafport.grafProcs newGrafProcs)
                (setq *pict-output-GrafProcs* t
                      *pict-output-pb* pb)
                (rlet ((picFrame :rect :topleft topleft :bottomright botright))
                  (with-port wptr
                    (setq pict-hand (#_OpenPicture picFrame)
                          *pict-output-handle* pict-hand)))
                (unless (%null-ptr-p pict-hand)
                  (setq *pict-output-GrafProcs* GrafProcs)
                  (%put-ptr grafProcs pict-hand $gpPictHand)))
              (when (eq t *pict-output-GrafProcs*)
                (setq *pict-output-pb* t)
                (rset wptr :grafport.grafProcs oldGrafProcs)))
            pict-hand
            )))
      (when (eq t *pict-output-pb*)
        (if pb (FSClose pb))
        (setq *pict-output-pb* nil
              *pict-output-handle* nil)
        (when grafProcs
          (#_DisposePtr grafProcs))))))

(defun close-pict-output-file (pict-hand)
  (let ((grafProcs *pict-output-GrafProcs*)
        (pb *pict-output-pb*))
    (unless pb
      (error "No picture output file open."))
    (unless (eql pict-hand (%get-ptr grafProcs $gpPictHand))
      (error "~s is not the pict-hand returned from open-pict-output-file"
             pict-hand))
    (let ((wptr (%get-ptr grafProcs $gpWptr))
          (oldGrafProcs (%get-ptr grafProcs $gpOldGrafProcs)))
      (with-port wptr
        (#_ClosePicture))
      (rset wptr :grafport.GrafProcs oldGrafProcs)
      (#_DisposePtr grafProcs)
      (SetFpos pb 512)
      (with-pointers ((pict pict-hand))
        (FSWrite pb $PictureSize pict))
      (#_KillPicture pict-hand)
      (FSClose pb)
      (setq *pict-output-GrafProcs* nil
            *pict-output-handle* nil
            *pict-output-pb* nil))))

(defmacro with-pict-output-file ((filename window topleft botright &key creator) &body body)
  (let ((pict-hand (make-symbol "PICT-HAND")))
    `(let ((,pict-hand (open-pict-output-file
                        ,filename ,window ,topleft ,botright :creator ,creator)))
       (unwind-protect
         (progn ,@body)
         (close-pict-output-file ,pict-hand)))))


;;; ********** This has to appear after the macro with-pict-outout-file
(defun canvas-to-pict-file (canvas &key (filepathname NIL))
  "Writes out the canvas contents to a PICT file via~
   the draw commands necessary to reproduce the canvas as displayed.  ~
   (:key ~
   (:arg filepathname NIL Complete pathname for the saved PICT file.  If NIL, ~
                      then the user is prompted for a filename.))"
  (unwind-protect 
      (progn
        (when (null filepathname)
          (setf filepathname
                (ccl::choose-new-file-dialog :prompt "Save canvas as PICT file ..."))
          (with-pict-output-file 
            (filepathname canvas (ccl::make-point 0 0)
                      (ccl::make-point (canvas-width canvas)
                                       (canvas-height canvas)))
            
            (redisplay canvas))
          ))))

(defun scale-lisp-point (point factor)
  (ccl::make-point (round (* (point-h point) factor))
                   (round (* (point-v point) factor))))

(defun display-pict-file (filename &optional (scale-factor 1) window)
  (unless window
    (setq window (make-instance 'palette-window :window-show nil)))
  (with-pict-input-file (pict filename window)
    (let* ((topleft (scale-lisp-point (rref pict :picture.picFrame.topLeft) scale-factor))
           (bottomright (scale-lisp-point (rref pict :picture.picFrame.bottomRight)
                                          scale-factor))
           (size (subtract-points bottomright topleft)))
      (set-view-size window size)
      (rlet ((rect :rect :topleft topleft :bottomright bottomright))
       (window-select window)
        (event-dispatch)
        (with-focused-view window
          (#_DrawPicture pict rect))))))

; Sometimes you want to load a pict file into memory.
(defun load-pict-file (pathname)
  (with-FSOpen-file (pb pathname)
    (let* ((size (- (getEOF pb) 512))
           (pict (#_NewHandle :errchk size)))
      (setFpos pb 512)      
      (with-dereferenced-handles ((pict-pointer pict))
        (FSRead pb size pict-pointer))
      pict)))

; Sometimes you want to store a pict into a file
(defun store-pict-to-file (pict pathname &key 
                                (if-exists :error)
                                (creator "dPro"))       ; MacDraw Pro
  (setq pict (require-type pict 'macptr))
  (create-file pathname
               :if-exists if-exists
               :mac-file-type "PICT"
               :mac-file-creator creator)
  (with-FSOpen-file (pb pathname t)
    (%stack-block ((header 512))
      (dotimes (i 512) (setf (%get-byte header i) 0))
      (FSWrite pb 512 header))
    (with-dereferenced-handles ((pict-pointer pict))
      (FSWrite pb (#_GetHandleSize pict) pict-pointer))))

(provide :picture-files)

#|
; Example of use

(defparameter *w* (make-instance 'window :view-size #@(200 200) :color-p t))

(defvar *picture-file* "ccl:picture-file.temp")

; Draw a square with an X inside and save it to *picture-file*
(defun make-it ()
  (delete-file *picture-file*)
  (window-select *w*)
  (with-focused-view *w*
    (with-pict-output-file (*picture-file* *w* #@(0 0) #@(200 200))
      (#_MoveTo 50 50)
      (#_LineTo 150 50)
      (#_LineTo 150 150)
      (#_LineTo 50 150)
      (#_LineTo 50 50)
      (#_LineTo 150 150)
      (#_MoveTo 150 50)
      (#_LineTo 50 150))
    (#_EraseRect (rref (wptr *w*) :windowRecord.portrect))))

; Draw the picture that is in *picture-file* on *w* inside the given rect.
(defun draw-it (&optional (bottomright #@(200 200)) (topleft #@(0 0)))
  (window-select *w*)
  (with-focused-view *w*
    (#_EraseRect (rref (wptr *w*) :windowRecord.portrect))
    (with-pict-input-file (pict *picture-file* *w*)
      ; Real code would probably want to access
      ; (rref pict :picture.picFrame.topleft) & 
      ; (rref pict :picture.picFrame.bottomright) here
      (unless topleft (setq topleft (rref pict :picture.picFrame.topLeft)))
      (unless bottomright (setq bottomright (rref pict :picture.picFrame.bottomRight)))
      (rlet ((rect :rect :topleft topleft :bottomright bottomright))
        (#_DrawPicture pict rect)))))

(defun do-it ()
  (make-it)
  (draw-it))

|#
     


#|
	Change History (most recent last):
	2	12/27/94	akh	merge with d13
|# ;(do not edit past this line!!)
