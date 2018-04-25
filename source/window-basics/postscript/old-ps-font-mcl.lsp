;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               ps-font-mcl.lisp
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
;;;    N.G. Bennett 1992
;;;    R.W. Oldford 1992
;;;     
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(*canvas-to-ps-font-name*)))

(defun get-ps-font-name (canvas canvas-font)
  "Returns the PostScript font name corresponding to the canvas-font."
  (declare (special *canvas-to-ps-font-name*))
  (when (and (member :outline (canvas-font-style canvas-font))
             (not (command-included-p "Outline Font Generation Procedures")))
    (toggle-includes canvas "Outline Font Generation Procedures")
    (read-write (include-file-of "Outline Font Generation Procedures")
                (header-file-of canvas)))
  (flet ((sort-style-list (s-l)
           (sort (copy-list s-l)
                 #'(lambda (x y)
                     (< (position x *canvas-font-styles*)
                        (position y *canvas-font-styles*))))))
    (let ((result
           (assoc canvas-font *canvas-to-ps-font-name*
                  :test
                  #'(lambda (cf key)
                      (and
                       (string-equal (canvas-font-name cf)
                              (canvas-font-name key))
                       (equal (sort-style-list (canvas-font-style cf))
                              (sort-style-list (canvas-font-style key))))))))
      (if result
        (cdr result)
        "Ugly"))))

(defun ps-set-canvas-font (canvas font)
  (with-open-file (ofile (body-full-pathname canvas)
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
    (format ofile "~%/~a findfont ~a scalefont setfont~%" 
            (get-ps-font-name canvas font)
            (* (canvas-font-size font)
               (floor (/ (+ (ps-x-scale-of canvas)
                    (ps-y-scale-of canvas)) 2))))))
    

(defvar *canvas-to-ps-font-name*
      (list
       (cons (canvas-make-font :name "Helvetica"
                               :style :plain)
             "Helvetica")
       (cons (canvas-make-font :name "Helvetica"
                               :style :bold)
             "Helvetica-Bold")
       (cons (canvas-make-font :name "Helvetica"
                               :style :italic)
             "Helvetica-Oblique")
       (cons (canvas-make-font :name "Helvetica"
                               :style (list :bold :italic))
             "Helvetica-BoldOblique")

       (cons (canvas-make-font :name "Helvetica"
                               :style :outline)
             "Helvetica /Helvetica-Outline MakeOutlinePlainFont~
              /Helvetica-Outline")
       (cons (canvas-make-font :name "Helvetica"
                               :style (list :bold :outline))
             "Helvetica-Bold /Helvetica-Bold-Outline MakeOutlinePlainFont~
              /Helvetica-Bold-Outline")
       (cons (canvas-make-font :name "Helvetica"
                               :style (list :italic :outline))
             "Helvetica-Oblique /Helvetica-Oblique-Outline MakeOutlinePlainFont~
              /Helvetica-Oblique-Outline")
       (cons (canvas-make-font :name "Helvetica"
                               :style (list :bold :italic :outline))
             "Helvetica-BoldOblique /Helvetica-BoldOblique-Outline~
              MakeOutlinePlainFont~
              /Helvetica-BoldOblique-Outline")

       (cons (canvas-make-font :name "Avant Garde"
                               :style :plain)
             "AvantGarde-Book")
       (cons (canvas-make-font :name "Avant Garde"
                               :style :italic)
             "AvantGarde-BookOblique")
       (cons (canvas-make-font :name "Avant Garde"
                               :style :bold)
             "AvantGarde-Demi")
       (cons (canvas-make-font :name "Avant Garde"
                               :style (list :bold :italic))
             "AvantGarde-DemiOblique")

       (cons (canvas-make-font :name "Avant Garde"
                               :style :outline)
             "AvantGarde-Book /AvantGarde-Book-Outline MakeOutlinePlainFont~
              /AvantGarde-Book-Outline")
       (cons (canvas-make-font :name "Avant Garde"
                               :style (list :bold :outline))
             "AvantGarde-Demi /AvantGarde-Demi-Outline MakeOutlinePlainFont~
              /AvantGarde-Demi-Outline")
       (cons (canvas-make-font :name "Avant Garde"
                               :style (list :italic :outline))
             "AvantGarde-BookOblique /AvantGarde-BookOblique-Outline MakeOutlinePlainFont~
              /AvantGarde-BookOblique-Outline")
       (cons (canvas-make-font :name "Avant Garde"
                               :style (list :bold :italic :outline))
             "AvantGarde-DemiOblique /AvantGarde-DemiOblique-Outline~
              MakeOutlinePlainFont~
              /AvantGarde-DemiOblique-Outline")

       (cons (canvas-make-font :name "Bookman"
                               :style :plain)
             "Bookman-Light")
       (cons (canvas-make-font :name "Bookman"
                               :style :italic)
             "Bookman-LightItalic")
       (cons (canvas-make-font :name "Bookman"
                               :style :bold)
             "Bookman-Demi")
       (cons (canvas-make-font :name "Bookman"
                               :style (list :bold :italic))
             "Bookman-DemiItalic")

       (cons (canvas-make-font :name "Bookman"
                               :style :outline)
             "Bookman-Light /Bookman-Light-Outline MakeOutlinePlainFont~
              /Bookman-Light-Outline")
       (cons (canvas-make-font :name "Bookman"
                               :style (list :bold :outline))
             "Bookman-Demi /Bookman-Demi-Outline MakeOutlinePlainFont~
              /Bookman-Demi-Outline")
       (cons (canvas-make-font :name "Bookman"
                               :style (list :italic :outline))
             "Bookman-LightItalic /Bookman-LightItalic-Outline MakeOutlinePlainFont~
              /Bookman-LightItalic-Outline")
       (cons (canvas-make-font :name "Bookman"
                               :style (list :bold :italic :outline))
             "Bookman-DemiItalic /Bookman-DemiItalic-Outline~
              MakeOutlinePlainFont~
              /Bookman-DemiItalic-Outline")

       (cons (canvas-make-font :name "Courier"
                               :style :plain)
             "Courier")
       (cons (canvas-make-font :name "Courier"
                               :style :italic)
             "Courier-Oblique")
       (cons (canvas-make-font :name "Courier"
                               :style :bold)
             "Courier-Bold")
       (cons (canvas-make-font :name "Courier"
                               :style (list :bold :italic))
             "Courier-BoldOblique")

       (cons (canvas-make-font :name "Courier"
                               :style :outline)
             "Courier /Courier-Outline MakeOutlinePlainFont~
              /Courier-Outline")
       (cons (canvas-make-font :name "Courier"
                               :style (list :bold :outline))
             "Courier-Bold /Courier-Bold-Outline MakeOutlinePlainFont~
              /Courier-Bold-Outline")
       (cons (canvas-make-font :name "Courier"
                               :style (list :italic :outline))
             "Courier-Oblique /Courier-Oblique-Outline MakeOutlinePlainFont~
              /Courier-Oblique-Outline")
       (cons (canvas-make-font :name "Courier"
                               :style (list :bold :italic :outline))
             "Courier-BoldOblique /Courier-BoldOblique-Outline~
              MakeOutlinePlainFont~
              /Courier-BoldOblique-Outline")

       (cons (canvas-make-font :name "Palatino"
                               :style :plain)
             "Palatino-Roman")
       (cons (canvas-make-font :name "Palatino"
                               :style :italic)
             "Palatino-Italic")
       (cons (canvas-make-font :name "Palatino"
                               :style :bold)
             "Palatino-Bold")
       (cons (canvas-make-font :name "Palatino"
                               :style (list :bold :italic))
             "Palatino-BoldItalic")

       (cons (canvas-make-font :name "Palatino"
                               :style :outline)
             "Palatino-Roman /Palatino-Roman-Outline MakeOutlinePlainFont~
              /Palatino-Roman-Outline")
       (cons (canvas-make-font :name "Palatino"
                               :style (list :bold :outline))
             "Palatino-Bold /Palatino-Bold-Outline MakeOutlinePlainFont~
              /Palatino-Bold-Outline")
       (cons (canvas-make-font :name "Palatino"
                               :style (list :italic :outline))
             "Palatino-Italic-Outline /Palatino-Italic-Outline MakeOutlinePlainFont~
              /Palatino-Italic-Outline")
       (cons (canvas-make-font :name "Palatino"
                               :style (list :bold :italic :outline))
             "Palatino-BoldItalic /Palatino-BoldItalic-Outline~
              MakeOutlinePlainFont~
              /Palatino-BoldItalic-Outline")

       (cons (canvas-make-font :name "Times"
                               :style :plain)
             "Times-Roman")
       (cons (canvas-make-font :name "Times"
                               :style :italic)
             "Times-Italic")
       (cons (canvas-make-font :name "Times"
                               :style :bold)
             "Times-Bold")
       (cons (canvas-make-font :name "Times"
                               :style (list :bold :italic))
             "Times-BoldItalic")

       (cons (canvas-make-font :name "Times"
                               :style :outline)
             "Times-Roman /Times-Roman-Outline MakeOutlinePlainFont~
              /Times-Roman-Outline")
       (cons (canvas-make-font :name "Times"
                               :style (list :bold :outline))
             "Times-Bold /Times-Bold-Outline MakeOutlinePlainFont~
              /Times-Bold-Outline")
       (cons (canvas-make-font :name "Times"
                               :style (list :italic :outline))
             "Times-Italic /Times-Italic-Outline MakeOutlinePlainFont~
              /Times-Italic-Outline")
       (cons (canvas-make-font :name "Times"
                               :style (list :bold :italic :outline))
             "Times-BoldItalic /Times-BoldItalic-Outline~
              MakeOutlinePlainFont~
              /Times-BoldItalic-Outline")

       (cons (canvas-make-font :name "Symbol"
                               :style :plain)
             "Symbol")
       (cons (canvas-make-font :name "Symbol"
                               :style :italic)
             "Symbol")
       (cons (canvas-make-font :name "Symbol"
                               :style :bold)
             "Symbol")
       (cons (canvas-make-font :name "Symbol"
                               :style (list :bold :italic))
             "Symbol")

       (cons (canvas-make-font :name "Symbol"
                               :style :outline)
             "Symbol /Symbol-Outline MakeOutlinePlainFont~
              /Symbol-Outline")
       (cons (canvas-make-font :name "Symbol"
                               :style (list :bold :outline))
             "Symbol /Symbol-Outline MakeOutlinePlainFont~
              /Symbol-Outline")
       (cons (canvas-make-font :name "Symbol"
                               :style (list :italic :outline))
             "Symbol /Symbol-Outline MakeOutlinePlainFont~
              /Symbol-Outline")
       (cons (canvas-make-font :name "Symbol"
                               :style (list :bold :italic :outline))
             "Symbol /Symbol-Outline~
              MakeOutlinePlainFont~
              /Symbol-Outline")

       (cons (canvas-make-font :name "Zapf Dingbats"
                               :style :plain)
             "Zapf-Dingbats")
       (cons (canvas-make-font :name "Zapf Dingbats"
                               :style :italic)
             "Zapf-Dingbats")
       (cons (canvas-make-font :name "Zapf Dingbats"
                               :style :bold)
             "Zapf-Dingbats")
       (cons (canvas-make-font :name "Zapf Dingbats"
                               :style (list :bold :italic))
             "Zapf-Dingbats")

       (cons (canvas-make-font :name "Zapf Dingbats"
                               :style :outline)
             "Zapf-Dingbats /Zapf-Dingbats-Outline MakeOutlinePlainFont~
              /Zapf-Dingbats-Outline")
       (cons (canvas-make-font :name "Zapf Dingbats"
                               :style (list :bold :outline))
             "Zapf-Dingbats /Zapf-Dingbats-Outline MakeOutlinePlainFont~
              /Zapf-Dingbats-Outline")
       (cons (canvas-make-font :name "Zapf Dingbats"
                               :style (list :italic :outline))
             "Zapf-Dingbats /Zapf-Dingbats-Outline MakeOutlinePlainFont~
              /Zapf-Dingbats-Outline")
       (cons (canvas-make-font :name "Zapf Dingbats"
                               :style (list :bold :italic :outline))
             "Zapf-Dingbats /Zapf-Dingbats-Outline~
              MakeOutlinePlainFont~
              /Zapf-Dingbats-Outline")

       (cons (canvas-make-font :name "New Century Schlbk"
                               :style :plain)
             "NewCenturySchlbk-Roman")
       (cons (canvas-make-font :name "New Century Schlbk"
                               :style :italic)
             "NewCenturySchlbk-Italic")
       (cons (canvas-make-font :name "New Century Schlbk"
                               :style :bold)
             "NewCenturySchlbk-Bold")
       (cons (canvas-make-font :name "New Century Schlbk"
                               :style (list :bold :italic))
             "NewCenturySchlbk-BoldItalic")

       (cons (canvas-make-font :name "New Century Schlbk"
                               :style :outline)
             "NewCenturySchlbk-Roman /NewCenturySchlbk-Roman-Outline MakeOutlinePlainFont~
              /NewCenturySchlbk-Roman-Outline")
       (cons (canvas-make-font :name "New Century Schlbk"
                               :style (list :bold :outline))
             "NewCenturySchlbk-Bold /NewCenturySchlbk-Bold-Outline MakeOutlinePlainFont~
              /NewCenturySchlbk-Bold-Outline")
       (cons (canvas-make-font :name "New Century Schlbk"
                               :style (list :italic :outline))
             "NewCenturySchlbk-Italic /NewCenturySchlbk-Italic-Outline MakeOutlinePlainFont~
              /NewCenturySchlbk-Italic-Outline")
       (cons (canvas-make-font :name "New Century Schlbk"
                               :style (list :bold :italic :outline))
             "NewCenturySchlbk-BoldItalic /NewCenturySchlbk-BoldItalic-Outline~
              MakeOutlinePlainFont~
              /NewCenturySchlbk-BoldItalic-Outline")

       (cons (canvas-make-font :name "AGaramond"
                               :style :plain)
             "Palatino")
       (cons (canvas-make-font :name "AGaramond"
                               :style :italic)
             "Palatino-Italic")
       (cons (canvas-make-font :name "AGaramond"
                               :style :bold)
             "Palatino-Bold")
       (cons (canvas-make-font :name "AGaramond"
                               :style (list :bold :italic))
             "Palatino-BoldItalic")

       (cons (canvas-make-font :name "AGaramond"
                               :style :outline)
             "Palatino /Palatino-Outline MakeOutlinePlainFont~
              /Palatino-Outline")
       (cons (canvas-make-font :name "AGaramond"
                               :style (list :bold :outline))
             "Palatino-Bold /Palatino-Bold-Outline MakeOutlinePlainFont~
              /Palatino-Bold-Outline")
       (cons (canvas-make-font :name "AGaramond"
                               :style (list :italic :outline))
             "Palatino-Italic /Palatino-Italic-Outline MakeOutlinePlainFont~
              /Palatino-Italic-Outline")
       (cons (canvas-make-font :name "AGaramond"
                               :style (list :bold :italic :outline))
             "Palatino-BoldItalic /Palatino-BoldItalic-Outline~
              MakeOutlinePlainFont~
              /Palatino-BoldItalic-Outline")

       (cons (canvas-make-font :name "AGaramond Bold"
                               :style :plain)
             "Palatino")
       (cons (canvas-make-font :name "AGaramond Bold"
                               :style :italic)
             "Palatino-Italic")
       (cons (canvas-make-font :name "AGaramond Bold"
                               :style :bold)
             "Palatino-Bold")
       (cons (canvas-make-font :name "AGaramond Bold"
                               :style (list :bold :italic))
             "Palatino-BoldItalic")

       (cons (canvas-make-font :name "AGaramond Bold"
                               :style :outline)
             "Palatino /Palatino-Outline MakeOutlinePlainFont~
              /Palatino-Outline")
       (cons (canvas-make-font :name "AGaramond Bold"
                               :style (list :bold :outline))
             "Palatino-Bold /Palatino-Bold-Outline MakeOutlinePlainFont~
              /Palatino-Bold-Outline")
       (cons (canvas-make-font :name "AGaramond Bold"
                               :style (list :italic :outline))
             "Palatino-Italic /Palatino-Italic-Outline MakeOutlinePlainFont~
              /Palatino-Italic-Outline")
       (cons (canvas-make-font :name "AGaramond Bold"
                               :style (list :bold :italic :outline))
             "Palatino-BoldItalic /Palatino-BoldItalic-Outline~
              MakeOutlinePlainFont~
              /Palatino-BoldItalic-Outline")

       (cons (canvas-make-font :name "AGaramond BoldItalic"
                               :style :plain)
             "Palatino")
       (cons (canvas-make-font :name "AGaramond BoldItalic"
                               :style :italic)
             "Palatino-Italic")
       (cons (canvas-make-font :name "AGaramond BoldItalic"
                               :style :bold)
             "Palatino-Bold")
       (cons (canvas-make-font :name "AGaramond BoldItalic"
                               :style (list :bold :italic))
             "Palatino-BoldItalic")

       (cons (canvas-make-font :name "AGaramond BoldItalic"
                               :style :outline)
             "Palatino /Palatino-Outline MakeOutlinePlainFont~
              /Palatino-Outline")
       (cons (canvas-make-font :name "AGaramond BoldItalic"
                               :style (list :bold :outline))
             "Palatino-Bold /Palatino-Bold-Outline MakeOutlinePlainFont~
              /Palatino-Bold-Outline")
       (cons (canvas-make-font :name "AGaramond BoldItalic"
                               :style (list :italic :outline))
             "Palatino-Italic /Palatino-Italic-Outline MakeOutlinePlainFont~
              /Palatino-Italic-Outline")
       (cons (canvas-make-font :name "AGaramond BoldItalic"
                               :style (list :bold :italic :outline))
             "Palatino-BoldItalic /Palatino-BoldItalic-Outline~
              MakeOutlinePlainFont~
              /Palatino-BoldItalic-Outline")

       (cons (canvas-make-font :name "AGaramond Italic"
                               :style :plain)
             "Palatino")
       (cons (canvas-make-font :name "AGaramond Italic"
                               :style :italic)
             "Palatino-Italic")
       (cons (canvas-make-font :name "AGaramond Italic"
                               :style :bold)
             "Palatino-Bold")
       (cons (canvas-make-font :name "AGaramond Italic"
                               :style (list :bold :italic))
             "Palatino-BoldItalic")

       (cons (canvas-make-font :name "AGaramond Italic"
                               :style :outline)
             "Palatino /Palatino-Outline MakeOutlinePlainFont~
              /Palatino-Outline")
       (cons (canvas-make-font :name "AGaramond Italic"
                               :style (list :bold :outline))
             "Palatino-Bold /Palatino-Bold-Outline MakeOutlinePlainFont~
              /Palatino-Bold-Outline")
       (cons (canvas-make-font :name "AGaramond Italic"
                               :style (list :italic :outline))
             "Palatino-Italic /Palatino-Italic-Outline MakeOutlinePlainFont~
              /Palatino-Italic-Outline")
       (cons (canvas-make-font :name "AGaramond Italic"
                               :style (list :bold :italic :outline))
             "Palatino-BoldItalic /Palatino-BoldItalic-Outline~
              MakeOutlinePlainFont~
              /Palatino-BoldItalic-Outline")

       (cons (canvas-make-font :name "AGaramond Semibold"
                               :style :plain)
             "Palatino")
       (cons (canvas-make-font :name "AGaramond Semibold"
                               :style :italic)
             "Palatino-Italic")
       (cons (canvas-make-font :name "AGaramond Semibold"
                               :style :bold)
             "Palatino-Bold")
       (cons (canvas-make-font :name "AGaramond Semibold"
                               :style (list :bold :italic))
             "Palatino-BoldItalic")

       (cons (canvas-make-font :name "AGaramond Semibold"
                               :style :outline)
             "Palatino /Palatino-Outline MakeOutlinePlainFont~
              /Palatino-Outline")
       (cons (canvas-make-font :name "AGaramond Semibold"
                               :style (list :bold :outline))
             "Palatino-Bold /Palatino-Bold-Outline MakeOutlinePlainFont~
              /Palatino-Bold-Outline")
       (cons (canvas-make-font :name "AGaramond Semibold"
                               :style (list :italic :outline))
             "Palatino-Italic /Palatino-Italic-Outline MakeOutlinePlainFont~
              /Palatino-Italic-Outline")
       (cons (canvas-make-font :name "AGaramond Semibold"
                               :style (list :bold :italic :outline))
             "Palatino-BoldItalic /Palatino-BoldItalic-Outline~
              MakeOutlinePlainFont~
              /Palatino-BoldItalic-Outline")

       (cons (canvas-make-font :name "AGaramond SemiboldItalic"
                               :style :plain)
             "Palatino")
       (cons (canvas-make-font :name "AGaramond SemiboldItalic"
                               :style :italic)
             "Palatino-Italic")
       (cons (canvas-make-font :name "AGaramond SemiboldItalic"
                               :style :bold)
             "Palatino-Bold")
       (cons (canvas-make-font :name "AGaramond SemiboldItalic"
                               :style (list :bold :italic))
             "Palatino-BoldItalic")

       (cons (canvas-make-font :name "AGaramond SemiboldItalic"
                               :style :outline)
             "Palatino /Palatino-Outline MakeOutlinePlainFont~
              /Palatino-Outline")
       (cons (canvas-make-font :name "AGaramond SemiboldItalic"
                               :style (list :bold :outline))
             "Palatino-Bold /Palatino-Bold-Outline MakeOutlinePlainFont~
              /Palatino-Bold-Outline")
       (cons (canvas-make-font :name "AGaramond SemiboldItalic"
                               :style (list :italic :outline))
             "Palatino-Italic /Palatino-Italic-Outline MakeOutlinePlainFont~
              /Palatino-Italic-Outline")
       (cons (canvas-make-font :name "AGaramond SemiboldItalic"
                               :style (list :bold :italic :outline))
             "Palatino-BoldItalic /Palatino-BoldItalic-Outline~
              MakeOutlinePlainFont~
              /Palatino-BoldItalic-Outline")

       (cons (canvas-make-font :name "B Garamond Bold"
                               :style :plain)
             "Palatino-Bold")
       (cons (canvas-make-font :name "B Garamond Bold"
                               :style :italic)
             "Palatino-BoldItalic")
       (cons (canvas-make-font :name "B Garamond Bold"
                               :style :bold)
             "Palatino-Bold")
       (cons (canvas-make-font :name "B Garamond Bold"
                               :style (list :bold :italic))
             "Palatino-BoldItalic")

       (cons (canvas-make-font :name "B Garamond Bold"
                               :style :outline)
             "Palatino-Bold /Palatino-Bold-Outline MakeOutlinePlainFont~
              /Palatino-Bold-Outline")
       (cons (canvas-make-font :name "B Garamond Bold"
                               :style (list :bold :outline))
             "Palatino-Bold /Palatino-Bold-Outline MakeOutlinePlainFont~
              /Palatino-Bold-Outline")
       (cons (canvas-make-font :name "B Garamond Bold"
                               :style (list :italic :outline))
             "Palatino-BoldItalic /Palatino-BoldItalic-Outline MakeOutlinePlainFont~
              /Palatino-BoldItalic-Outline")
       (cons (canvas-make-font :name "B Garamond Bold"
                               :style (list :bold :italic :outline))
             "Palatino-BoldItalic /Palatino-BoldItalic-Outline~
              MakeOutlinePlainFont~
              /Palatino-BoldItalic-Outline")

       (cons (canvas-make-font :name "BI Garamond BoldItalic"
                               :style :plain)
             "Palatino-BoldItalic")
       (cons (canvas-make-font :name "BI Garamond BoldItalic"
                               :style :italic)
             "Palatino-BoldItalic")
       (cons (canvas-make-font :name "BI Garamond BoldItalic"
                               :style :bold)
             "Palatino-BoldItalic")
       (cons (canvas-make-font :name "BI Garamond BoldItalic"
                               :style (list :bold :italic))
             "Palatino-BoldItalic")

       (cons (canvas-make-font :name "BI Garamond BoldItalic"
                               :style :outline)
             "Palatino-BoldItalic /Palatino-BoldItalic-Outline MakeOutlinePlainFont~
              /Palatino-BoldItalic-Outline")
       (cons (canvas-make-font :name "BI Garamond BoldItalic"
                               :style (list :bold :outline))
             "Palatino-BoldItalic /Palatino-BoldItalic-Outline MakeOutlinePlainFont~
              /Palatino-BoldItalic-Outline")
       (cons (canvas-make-font :name "BI Garamond BoldItalic"
                               :style (list :italic :outline))
             "Palatino-BoldItalic /Palatino-BoldItalic-Outline MakeOutlinePlainFont~
              /Palatino-BoldItalic-Outline")
       (cons (canvas-make-font :name "BI Garamond BoldItalic"
                               :style (list :bold :italic :outline))
             "Palatino-BoldItalic /Palatino-BoldItalic-Outline~
              MakeOutlinePlainFont~
              /Palatino-BoldItalic-Outline")

       (cons (canvas-make-font :name "I Garamond LightItalic"
                               :style :plain)
             "Palatino-Italic")
       (cons (canvas-make-font :name "I Garamond LightItalic"
                               :style :italic)
             "Palatino-Italic")
       (cons (canvas-make-font :name "I Garamond LightItalic"
                               :style :bold)
             "Palatino-BoldItalic")
       (cons (canvas-make-font :name "I Garamond LightItalic"
                               :style (list :bold :italic))
             "Palatino-BoldItalic")

       (cons (canvas-make-font :name "I Garamond LightItalic"
                               :style :outline)
             "Palatino-Italic /Palatino-Italic-Outline MakeOutlinePlainFont~
              /Palatino-Italic-Outline")
       (cons (canvas-make-font :name "I Garamond LightItalic"
                               :style (list :bold :outline))
             "Palatino-BoldItalic /Palatino-BoldItalic-Outline MakeOutlinePlainFont~
              /Palatino-BoldItalic-Outline")
       (cons (canvas-make-font :name "I Garamond LightItalic"
                               :style (list :italic :outline))
             "Palatino-Italic /Palatino-Italic-Outline MakeOutlinePlainFont~
              /Palatino-Italic-Outline")
       (cons (canvas-make-font :name "I Garamond LightItalic"
                               :style (list :bold :italic :outline))
             "Palatino-BoldItalic /Palatino-BoldItalic-Outline~
              MakeOutlinePlainFont~
              /Palatino-BoldItalic-Outline")

       (cons (canvas-make-font :name "Garamond"
                               :style :plain)
             "Palatino")
       (cons (canvas-make-font :name "Garamond"
                               :style :italic)
             "Palatino-Italic")
       (cons (canvas-make-font :name "Garamond"
                               :style :bold)
             "Palatino-Bold")
       (cons (canvas-make-font :name "Garamond"
                               :style (list :bold :italic))
             "Palatino-BoldItalic")

       (cons (canvas-make-font :name "Garamond"
                               :style :outline)
             "Palatino /Palatino-Outline MakeOutlinePlainFont~
              /Palatino-Outline")
       (cons (canvas-make-font :name "Garamond"
                               :style (list :bold :outline))
             "Palatino-Bold /Palatino-Bold-Outline MakeOutlinePlainFont~
              /Palatino-Bold-Outline")
       (cons (canvas-make-font :name "Garamond"
                               :style (list :italic :outline))
             "Palatino-Italic /Palatino-Italic-Outline MakeOutlinePlainFont~
              /Palatino-Italic-Outline")
       (cons (canvas-make-font :name "Garamond"
                               :style (list :bold :italic :outline))
             "Palatino-BoldItalic /Palatino-BoldItalic-Outline~
              MakeOutlinePlainFont~
              /Palatino-BoldItalic-Outline")

       (cons (canvas-make-font :name "Zapf Chancery"
                               :style :plain)
             "Palatino")
       (cons (canvas-make-font :name "Zapf Chancery"
                               :style :italic)
             "Palatino-Italic")
       (cons (canvas-make-font :name "Zapf Chancery"
                               :style :bold)
             "Palatino-Bold")
       (cons (canvas-make-font :name "Zapf Chancery"
                               :style (list :bold :italic))
             "Palatino-BoldItalic")

       (cons (canvas-make-font :name "Zapf Chancery"
                               :style :outline)
             "Palatino /Palatino-Outline MakeOutlinePlainFont~
              /Palatino-Outline")
       (cons (canvas-make-font :name "Zapf Chancery"
                               :style (list :bold :outline))
             "Palatino-Bold /Palatino-Bold-Outline MakeOutlinePlainFont~
              /Palatino-Bold-Outline")
       (cons (canvas-make-font :name "Zapf Chancery"
                               :style (list :italic :outline))
             "Palatino-Italic /Palatino-Italic-Outline MakeOutlinePlainFont~
              /Palatino-Italic-Outline")
       (cons (canvas-make-font :name "Zapf Chancery"
                               :style (list :bold :italic :outline))
             "Palatino-BoldItalic /Palatino-BoldItalic-Outline~
              MakeOutlinePlainFont~
              /Palatino-BoldItalic-Outline")

       (cons (canvas-make-font :name "Merrion Square"
                               :style :plain)
             "Ugly")
       (cons (canvas-make-font :name "Merrion Square"
                               :style :italic)
             "Ugly")
       (cons (canvas-make-font :name "Merrion Square"
                               :style :bold)
             "Ugly")
       (cons (canvas-make-font :name "Merrion Square"
                               :style (list :bold :italic))
             "Ugly")

       (cons (canvas-make-font :name "Merrion Square"
                               :style :outline)
             "Ugly /Ugly-Outline MakeOutlinePlainFont~
              /Ugly-Outline")
       (cons (canvas-make-font :name "Merrion Square"
                               :style (list :bold :outline))
             "Ugly /Ugly-Outline MakeOutlinePlainFont~
              /Ugly-Outline")
       (cons (canvas-make-font :name "Merrion Square"
                               :style (list :italic :outline))
             "Ugly /Ugly-Outline MakeOutlinePlainFont~
              /Ugly-Outline")
       (cons (canvas-make-font :name "Merrion Square"
                               :style (list :bold :italic :outline))
             "Ugly /Ugly-Outline~
              MakeOutlinePlainFont~
              /Ugly-Outline")

       (cons (canvas-make-font :name "Chicago"
                               :style :plain)
             "Times-Roman")
       (cons (canvas-make-font :name "Chicago"
                               :style :italic)
             "Times-Italic")
       (cons (canvas-make-font :name "Chicago"
                               :style :bold)
             "Times-Bold")
       (cons (canvas-make-font :name "Chicago"
                               :style (list :bold :italic))
             "Times-BoldItalic")

       (cons (canvas-make-font :name "Chicago"
                               :style :outline)
             "Times-Roman /Times-Roman-Outline MakeOutlinePlainFont~
              /Times-Roman-Outline")
       (cons (canvas-make-font :name "Chicago"
                               :style (list :bold :outline))
             "Times-Bold /Times-Bold-Outline MakeOutlinePlainFont~
              /Times-Bold-Outline")
       (cons (canvas-make-font :name "Chicago"
                               :style (list :italic :outline))
             "Times-Italic /Times-Italic-Outline MakeOutlinePlainFont~
              /Times-Italic-Outline")
       (cons (canvas-make-font :name "Chicago"
                               :style (list :bold :italic :outline))
             "Times-BoldItalic /Times-BoldItalic-Outline~
              MakeOutlinePlainFont~
              /Times-BoldItalic-Outline")

       (cons (canvas-make-font :name "Geneva"
                               :style :plain)
             "AvantGarde")
       (cons (canvas-make-font :name "Geneva"
                               :style :italic)
             "AvantGarde-Oblique")
       (cons (canvas-make-font :name "Geneva"
                               :style :bold)
             "AvantGarde-Demi")
       (cons (canvas-make-font :name "Geneva"
                               :style (list :bold :italic))
             "AvantGarde-DemiOblique")

       (cons (canvas-make-font :name "Geneva"
                               :style :outline)
             "AvantGarde /AvantGarde-Outline MakeOutlinePlainFont~
              /AvantGarde-Outline")
       (cons (canvas-make-font :name "Geneva"
                               :style (list :bold :outline))
             "AvantGarde-Demi /AvantGarde-Demi-Outline MakeOutlinePlainFont~
              /AvantGarde-Demi-Outline")
       (cons (canvas-make-font :name "Geneva"
                               :style (list :italic :outline))
             "AvantGarde-Oblique /AvantGarde-Oblique-Outline MakeOutlinePlainFont~
              /AvantGarde-Oblique-Outline")
       (cons (canvas-make-font :name "Geneva"
                               :style (list :bold :italic :outline))
             "AvantGarde-DemiOblique /AvantGarde-DemiOblique-Outline~
              MakeOutlinePlainFont~
              /AvantGarde-DemiOblique-Outline")

       (cons (canvas-make-font :name "Monaco"
                               :style :plain)
             "Times-Roman")
       (cons (canvas-make-font :name "Monaco"
                               :style :italic)
             "Times-Italic")
       (cons (canvas-make-font :name "Monaco"
                               :style :bold)
             "Times-Bold")
       (cons (canvas-make-font :name "Monaco"
                               :style (list :bold :italic))
             "Times-BoldItalic")

       (cons (canvas-make-font :name "Monaco"
                               :style :outline)
             "Times-Roman /Times-Roman-Outline MakeOutlinePlainFont~
              /Times-Roman-Outline")
       (cons (canvas-make-font :name "Monaco"
                               :style (list :bold :outline))
             "Times-Bold /Times-Bold-Outline MakeOutlinePlainFont~
              /Times-Bold-Outline")
       (cons (canvas-make-font :name "Monaco"
                               :style (list :italic :outline))
             "Times-Italic /Times-Italic-Outline MakeOutlinePlainFont~
              /Times-Italic-Outline")
       (cons (canvas-make-font :name "Monaco"
                               :style (list :bold :italic :outline))
             "Times-BoldItalic /Times-BoldItalic-Outline~
              MakeOutlinePlainFont~
              /Times-BoldItalic-Outline")

       (cons (canvas-make-font :name "N Helvetica Narrow"
                               :style :plain)
             "Helvetica")
       (cons (canvas-make-font :name "N Helvetica Narrow"
                               :style :italic)
             "Helvetica-Oblique")
       (cons (canvas-make-font :name "N Helvetica Narrow"
                               :style :bold)
             "Helvetica-Bold")
       (cons (canvas-make-font :name "N Helvetica Narrow"
                               :style (list :bold :italic))
             "Helvetica-BoldOblique")

       (cons (canvas-make-font :name "N Helvetica Narrow"
                               :style :outline)
             "Helvetica /Helvetica-Outline MakeOutlinePlainFont~
              /Helvetica-Outline")
       (cons (canvas-make-font :name "N Helvetica Narrow"
                               :style (list :bold :outline))
             "Helvetica-Bold /Helvetica-Bold-Outline MakeOutlinePlainFont~
              /Helvetica-Bold-Outline")
       (cons (canvas-make-font :name "N Helvetica Narrow"
                               :style (list :italic :outline))
             "Helvetica-Oblique /Helvetica-Oblique-Outline MakeOutlinePlainFont~
              /Helvetica-Oblique-Outline")
       (cons (canvas-make-font :name "N Helvetica Narrow"
                               :style (list :bold :italic :outline))
             "Helvetica-BoldOblique /Helvetica-BoldOblique-Outline~
              MakeOutlinePlainFont~
              /Helvetica-BoldOblique-Outline")

       (cons (canvas-make-font :name "New York"
                               :style :plain)
             "Times-Roman")
       (cons (canvas-make-font :name "New York"
                               :style :italic)
             "Times-Italic")
       (cons (canvas-make-font :name "New York"
                               :style :bold)
             "Times-Bold")
       (cons (canvas-make-font :name "New York"
                               :style (list :bold :italic))
             "Times-BoldItalic")

       (cons (canvas-make-font :name "New York"
                               :style :outline)
             "Times-Roman /Times-Roman-Outline MakeOutlinePlainFont~
              /Times-Roman-Outline")
       (cons (canvas-make-font :name "New York"
                               :style (list :bold :outline))
             "Times-Bold /Times-Bold-Outline MakeOutlinePlainFont~
              /Times-Bold-Outline")
       (cons (canvas-make-font :name "New York"
                               :style (list :italic :outline))
             "Times-Italic /Times-Italic-Outline MakeOutlinePlainFont~
              /Times-Italic-Outline")
       (cons (canvas-make-font :name "New York"
                               :style (list :bold :italic :outline))
             "Times-BoldItalic /Times-BoldItalic-Outline~
              MakeOutlinePlainFont~
              /Times-BoldItalic-Outline")

       "This variable maps the standard Macintosh fonts to the~
                       standard Postscript fonts."
))
