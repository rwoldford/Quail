;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               ps-prompt.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;
;;;  Authors:
;;;     N.G. Bennett 1992
;;;     R.W. Oldford 1992
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------
;;;
;;; 
;;;

(in-package :wb)
(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(prompt-for-ps-filenames prompt-for-ps-page-properties )))

(defun prompt-for-ps-filenames (canvas)
  "Prompt user for postscript file information for the given canvas."
  (let*
    ((pi-info (postscript-info-of canvas))
     (directory-prompt "Directory:")
     (directory-default (or (write-directory-of pi-info)
                            "q:tmp;"))
     (postscript-prompt "Postscript filename (no ext.):")
     (oldname (postscript-file-of pi-info))
     (oldname-length (length oldname))
     (postscript-default (if (> oldname-length 3)
                           (cond 
                            ((equal (subseq oldname (- oldname-length 3) oldname-length) ".ps")
                             (subseq oldname 0 (- oldname-length 3)))
                            ((equal (subseq oldname (- oldname-length 4) oldname-length) ".eps")
                             (subseq oldname 0 (- oldname-length 4)))
                            (T oldname))
                           "canvas"))
     (result NIL)
     new-info)
    (setf new-info
          (collect-input
           (list
            (cons directory-prompt directory-default) 
            (cons postscript-prompt postscript-default))))
    (setf result new-info)
    (when new-info
      (setf (write-directory-of pi-info)
            (cdr (assoc directory-prompt new-info :test #'string=)))
      (reset-includes canvas)
      (let*
        ((new-name (cdr (assoc postscript-prompt new-info :test #'string=)))
         (directory (write-directory-of pi-info))
         (full-file-name (if (multiple-pages-p pi-info)
                           (concatenate 'string directory new-name ".ps")
                           (concatenate 'string directory new-name ".eps"))))
        (when (probe-file full-file-name)
          (if (prompt-t-or-f (format NIL "The file '~a' already exists. Click Retry to choose new name." new-name)
                             :true-text "Overwrite"
                             :false-text "Retry...")
            (delete-file full-file-name)
            (setf new-name 
                  (cdr (assoc postscript-prompt (prompt-for-ps-filenames canvas) :test #'string=)))))
        (if (multiple-pages-p pi-info)
          (setf (postscript-file-of pi-info) (concatenate 'string new-name ".ps"))
          (setf (postscript-file-of pi-info) (concatenate 'string new-name ".eps")))
        (setf (header-file-of pi-info) (concatenate 'string new-name "-header.temp"))
        (setf (body-file-of pi-info) (concatenate 'string new-name "-body.temp"))))
    result))

(defun prompt-for-ps-page-properties (canvas)
  "Prompt user for postscript page information for the given canvas."
  (let*
    ((pi-info (postscript-info-of canvas))
     (x-origin-prompt "x-origin:")
     (x-origin-default (if (stringp (ps-x-origin-of pi-info))
                         (format NIL (ps-x-origin-of pi-info))
                         (format NIL "~a" (ps-x-origin-of pi-info))))
     (y-origin-prompt "y-origin:")
     (y-origin-default (if (stringp (ps-y-origin-of pi-info))
                         (format NIL (ps-y-origin-of pi-info))
                         (format NIL "~a" (ps-y-origin-of pi-info))))
     (x-scale-prompt "x-scale:")
     (x-scale-default (format NIL "~a" (ps-x-scale-of pi-info)))
     (y-scale-prompt "y-scale:")
     (y-scale-default (format NIL "~a" (ps-y-scale-of pi-info)))
     (horizontal-margin-prompt "postscript horizontal margin (1/72 inches):")
     (horizontal-margin-default (format NIL "~a" (ps-horizontal-margin-of pi-info)))
     (vertical-margin-prompt "postscript vertical margin (in 1/72 inches):")
     (vertical-margin-default (format NIL "~a" (ps-vertical-margin-of pi-info)))
     (result NIL)
     new-info
     )
    (flet
      ((test-val-for-num (read-val)
         (if (and (symbolp read-val)
                  (not (boundp read-val)))
           :wrong
           (progn (setf read-val (eval read-val))
                  (if (not (numberp read-val))
                    :wrong
                    read-val))))
       ;(test-val-for-boole (read-val) ; 28JUL2023
       ;  (if (and (symbolp read-val)
       ;           (not (boundp read-val)))
       ;    :wrong
       ;    (if (eval read-val)
       ;      T
       ;      NIL))) ; 28JUL2023
       )
      (setf new-info
            (collect-input
             (list
              (cons x-origin-prompt x-origin-default) 
              (cons y-origin-prompt y-origin-default) 
              (cons x-scale-prompt x-scale-default) 
              (cons y-scale-prompt y-scale-default)
              (cons horizontal-margin-prompt horizontal-margin-default)
              (cons vertical-margin-prompt vertical-margin-default)
              )
             :prompt-text "Postscript output page properties."
             ))
      (setf result new-info)
      (when new-info
        (let* ((ans-x (cdr (assoc x-origin-prompt new-info :test #'string=)))
               (ans-y (cdr (assoc y-origin-prompt new-info :test #'string=)))
               (center-x (equal ans-x "CENTER"))
               (center-y (equal ans-y "CENTER")))
          (when center-x
            (rplacd (assoc x-origin-prompt new-info) 
                    (format NIL "~a" 
                            (round (/ (- *ps-paper-width* 
                                         (* 2 (read-from-string (cdr (assoc horizontal-margin-prompt new-info :test #'string=))))
                                         (* (ps-width-of canvas) (read-from-string (cdr (assoc x-scale-prompt new-info :test #'string=)))))
                                      2))))
            (setf (center-horizontal-p pi-info) T))
          (when center-y
            (rplacd (assoc y-origin-prompt new-info) 
                    (format NIL "~a" 
                            (round (/ (- *ps-paper-height* 
                                         (* 2 (read-from-string (cdr (assoc vertical-margin-prompt new-info :test #'string=))))
                                         (* (ps-height-of canvas) (read-from-string (cdr (assoc y-scale-prompt new-info :test #'string=)))))
                                      2))))
             (setf (center-vertical-p pi-info) T)))
        (setf new-info
              (loop for pair in new-info
                    collect
                    (let ((read-val (read-from-string (cdr pair))))
                      (cons (car pair)
                            (do
                              ((ans
                                (test-val-for-num read-val)
                                (test-val-for-num
                                 (read-from-string  (prompt-user :prompt-string
                                                                 (concatenate 'string (car pair)
                                                                              " MUST BE A NUMBER") :result-type 'string))))) ;; from :type 16JAN2021
                              ((not (eq ans :wrong)) ans))))))
        (setf (ps-x-origin-of pi-info)
              (cdr (assoc x-origin-prompt new-info :test #'string=)))
        (setf (ps-y-origin-of pi-info)
              (cdr (assoc y-origin-prompt new-info :test #'string=)))
        (setf (ps-x-scale-of pi-info)
              (cdr (assoc x-scale-prompt new-info :test #'string=)))
        (setf (ps-y-scale-of pi-info)
              (cdr (assoc y-scale-prompt new-info :test #'string=)))
        (setf (ps-horizontal-margin-of pi-info)
              (cdr (assoc horizontal-margin-prompt new-info :test #'string=)))
        (setf (ps-vertical-margin-of pi-info)
              (cdr (assoc vertical-margin-prompt new-info :test #'string=)))
        (if (is-multiple-pages? canvas)
          (setf (multiple-pages-p pi-info) T)
          (setf (multiple-pages-p pi-info) NIL))
        ))
    result))

(defun is-multiple-pages? (canvas)
  "If either the scaled x-width is larger than the page width, or the scaled y-height is 
   larger than the page height, then return T else return NIL."
  (let*
    ((pi-info  (postscript-info-of canvas))
     (width    (canvas-width canvas))
     (height   (canvas-height canvas))
     (x-origin (ps-x-origin-of pi-info))
     (y-origin (ps-y-origin-of pi-info))
     (x-scale  (ps-x-scale-of pi-info))
     (y-scale  (ps-y-scale-of pi-info))
     (x-margin (ps-horizontal-margin-of pi-info))
     (y-margin (ps-vertical-margin-of pi-info)))
    (if (or (> (+ x-origin (* x-scale width))  (- *ps-paper-width*  (* 2 x-margin)))
            (> (+ y-origin (* y-scale height)) (- *ps-paper-height* (* 2 y-margin))))
      T
      NIL)))
