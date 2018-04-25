**********************************************************************

(IN-PACKAGE :WB)
----------------------------------------------------------------------
**********************************************************************

(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (EXPORT '(*LIGHT-GREY-COLOR* *GREY-COLOR* *DARK-GREY-COLOR*
            *CYAN-COLOUR* *BLACK-COLOUR* *WHITE-COLOUR* *PINK-COLOUR*
            *RED-COLOUR* *ORANGE-COLOUR* *YELLOW-COLOUR*
            *GREEN-COLOUR* *DARK-GREEN-COLOUR* *LIGHT-BLUE-COLOUR*
            *BLUE-COLOUR* *PURPLE-COLOUR* *BROWN-COLOUR* *TAN-COLOUR*
            *LIGHT-GRAY-COLOUR* *GRAY-COLOUR* *DARK-GRAY-COLOUR*
            *LIGHT-GREY-COLOUR* *GREY-COLOUR* *DARK-GREY-COLOUR*
            *BRIGHT-GREEN-COLOR* *MAGENTA-COLOR*
            *BRIGHT-GREEN-COLOUR* *MAGENTA-COLOUR* *COLORS*
            *BRIGHT-WHITE-COLOR* *DEFAULT-CANVAS-BACKGROUND-COLOR*
            *DEFAULT-CANVAS-PEN-COLOR* SHADE-TO-COLOR COLOR-TO-SHADE
            OPTIMIZE-COLOR-RECORD DEOPTIMIZE-COLOR-RECORD RGB_TO_LHS
            LHS_TO_RGB HUE-OF LIGHTNESS-OF SATURATION-OF LIGHTEN
            DARKEN)))
----------------------------------------------------------------------
**********************************************************************

(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (EXPORT '(*BLACK-COLOR* *WHITE-COLOR* *PINK-COLOR* *RED-COLOR*
            *ORANGE-COLOR* *YELLOW-COLOR* *GREEN-COLOR*
            *DARK-GREEN-COLOR* *LIGHT-BLUE-COLOR* *BLUE-COLOR*
            *PURPLE-COLOR* *BROWN-COLOR* *TAN-COLOR*
            *LIGHT-GRAY-COLOR* *GRAY-COLOR* *DARK-GRAY-COLOR*)))
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *BLACK-COLOR* (MAKE-COLOR 0.0 0.0 0.0))
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *WHITE-COLOR* (MAKE-COLOR 1.0 1.0 1.0))
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *PINK-COLOR*
    (MAKE-COLOR 0.9490196 0.031372547 0.5176471))
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *RED-COLOR*
    (MAKE-COLOR 0.8666667 0.031372547 0.023529412))
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *ORANGE-COLOR* (MAKE-COLOR 1.0 0.39215687 0.007843137))
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *YELLOW-COLOR*
    (MAKE-COLOR 0.9882353 0.9529412 0.019607844))
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *GREEN-COLOR*
    (MAKE-COLOR 0.12156863 0.7176471 0.078431375))
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *DARK-GREEN-COLOR*
    (MAKE-COLOR 0.0 0.39215687 0.06666667))
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *LIGHT-BLUE-COLOR*
    (MAKE-COLOR 0.007843137 0.67058825 0.91764706))
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *BLUE-COLOR* (MAKE-COLOR 0.0 0.0 0.83137256))
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER WB::*CYAN-COLOR* (WB:MAKE-COLOR 0.0 1.0 1.0))
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *PURPLE-COLOR* (MAKE-COLOR 0.2745098 0.0 0.64705884))
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *BROWN-COLOR*
    (MAKE-COLOR 0.3372549 0.17254902 0.019607844))
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *TAN-COLOR*
    (MAKE-COLOR 0.5647059 0.44313726 0.22745098))
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *LIGHT-GRAY-COLOR*
    (MAKE-COLOR 0.7529412 0.7529412 0.7529412))
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *GRAY-COLOR*
    (MAKE-COLOR 0.50196075 0.50196075 0.50196075))
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *DARK-GRAY-COLOR*
    (MAKE-COLOR 0.25098038 0.25098038 0.25098038))
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *GREY-COLOR* *GRAY-COLOR*)
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *LIGHT-GREY-COLOR* *LIGHT-GRAY-COLOR*)
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *DARK-GREY-COLOR* *DARK-GRAY-COLOR*)
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *BLACK-COLOUR* *BLACK-COLOR*)
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *WHITE-COLOUR* *WHITE-COLOR*)
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *PINK-COLOUR* *PINK-COLOR*)
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *RED-COLOUR* *RED-COLOR*)
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *ORANGE-COLOUR* *ORANGE-COLOR*)
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *YELLOW-COLOUR* *YELLOW-COLOR*)
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *GREEN-COLOUR* *GREEN-COLOR*)
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *DARK-GREEN-COLOUR* *DARK-GREEN-COLOR*)
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *LIGHT-BLUE-COLOUR* *LIGHT-BLUE-COLOR*)
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *BLUE-COLOUR* *BLUE-COLOR*)
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *CYAN-COLOUR* *CYAN-COLOR*)
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *PURPLE-COLOUR* *PURPLE-COLOR*)
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *BROWN-COLOUR* *BROWN-COLOR*)
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *TAN-COLOUR* *TAN-COLOR*)
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *LIGHT-GRAY-COLOUR* *LIGHT-GRAY-COLOR*)
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *GRAY-COLOUR* *GRAY-COLOR*)
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *DARK-GRAY-COLOUR* *DARK-GRAY-COLOR*)
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *GREY-COLOUR* *GRAY-COLOR*)
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *LIGHT-GREY-COLOUR* *LIGHT-GRAY-COLOR*)
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *DARK-GREY-COLOUR* *DARK-GRAY-COLOR*)
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *BRIGHT-GREEN-COLOR* (MAKE-COLOR 0 1.0 0))
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *BRIGHT-GREEN-COLOUR* *BRIGHT-GREEN-COLOR*)
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *MAGENTA-COLOR* (MAKE-COLOR 1.0 0 1.0))
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *MAGENTA-COLOUR* *MAGENTA-COLOR*)
----------------------------------------------------------------------
**********************************************************************

(DEFVAR *COLORS*
    (LIST *WHITE-COLOR* *PINK-COLOR* *RED-COLOR* *ORANGE-COLOR*
     *YELLOW-COLOR* *GREEN-COLOR* *CYAN-COLOR* *BLUE-COLOR*
     *PURPLE-COLOR* *BROWN-COLOR* *TAN-COLOR* *GRAY-COLOR*
     *BLACK-COLOR* *DARK-GREEN-COLOR* *LIGHT-GRAY-COLOR*
     *LIGHT-BLUE-COLOR* *DARK-GRAY-COLOR*)
  "The list of predefined colors -- useful as an iteration sequence.")
----------------------------------------------------------------------
**********************************************************************

(DEFPARAMETER *BRIGHT-WHITE-COLOR* (MAKE-COLOR 1.0 1.0 1.0))
----------------------------------------------------------------------
**********************************************************************

(DEFVAR *DEFAULT-CANVAS-BACKGROUND-COLOR* *WHITE-COLOR*
  "The default colour for the background of a canvas. ~
   (:see-also *default-canvas-pen-color*)")
----------------------------------------------------------------------
**********************************************************************

(DEFVAR *DEFAULT-CANVAS-PEN-COLOR* *BLACK-COLOR*
  "The default colour for the pen of a canvas. ~
   (:see-also *default-canvas-background-color*)")
----------------------------------------------------------------------
**********************************************************************

(DEFUN SHADE-TO-COLOR (SHADE)
  "Translates a shade to a color."
  (DECLARE (SPECIAL *BLACK-SHADE* *WHITE-SHADE* *LIGHT-GRAY-SHADE*
            *DARK-GRAY-SHADE* *GRAY-SHADE* *BLACK-COLOR*
            *WHITE-COLOR* *LIGHT-GRAY-COLOR* *DARK-GRAY-COLOR*
            *GRAY-COLOR*))
  (COND ((EQ *BLACK-SHADE* SHADE) *BLACK-COLOR*)
        ((EQ *WHITE-SHADE* SHADE) *WHITE-COLOR*)
        ((EQ *LIGHT-GRAY-SHADE* SHADE) *LIGHT-GRAY-COLOR*)
        ((EQ *GRAY-SHADE* SHADE) *GRAY-COLOR*)
        ((EQ *DARK-GRAY-SHADE* SHADE) *DARK-GRAY-COLOR*)
        (T *GRAY-COLOR*)))
----------------------------------------------------------------------
USE-EQL: Unless something special is going on, use EQL, not EQ.
----------------------------------------------------------------------
**********************************************************************

(DEFUN COLOR-TO-SHADE (COLOR)
  "Translates a color into a shade."
  (DECLARE (SPECIAL *BLACK-SHADE* *WHITE-SHADE* *LIGHT-GRAY-SHADE*
            *DARK-GRAY-SHADE* *GRAY-SHADE* *BLACK-COLOR*
            *WHITE-COLOR* *LIGHT-GRAY-COLOR* *DARK-GRAY-COLOR*))
  (COND ((EQ-COLORS *BLACK-COLOR* COLOR) *BLACK-SHADE*)
        ((EQ-COLORS *WHITE-COLOR* COLOR) *WHITE-SHADE*)
        ((EQ-COLORS *LIGHT-GRAY-COLOR* COLOR) *LIGHT-GRAY-SHADE*)
        ((EQ-COLORS *GRAY-COLOR* COLOR) *GRAY-SHADE*)
        ((EQ-COLORS *DARK-GRAY-COLOR* COLOR) *DARK-GRAY-SHADE*)
        (T *GRAY-SHADE*)))
----------------------------------------------------------------------
**********************************************************************

(DEFUN LHS_TO_RGB (L H S &KEY (MODEL :TRIANGLE))
  (IF (AND (>= L 0) (<= L 1))
      (IF (OR (NULL H) (AND (>= H 0) (< H 360)))
          (IF (AND (>= S 0) (<= S 1))
              (LET (R G B)
                (IF (OR (ZEROP S) (NOT H))
                    (SETQ R L G L B L)
                  (LET* ((M 1)
                         (K (FLOOR H 60))
                         (F (- (/ H 60) K))
                         (FPRIME (IF (ODDP K) (- 1 F) F))
                         LATQ
                         MIN
                         MID
                         MAX
                         WMIN
                         WMID
                         WMAX)
                    (COND ((EQUAL MODEL :TRIANGLE)
                           (SETQ WMIN (/ 1 3)
                                 WMID (/ 1 3)
                                 WMAX (/ 1 3)))
                          ((EQUAL MODEL :HEXCONE)
                           (SETQ WMIN 0 WMID 0 WMAX 1))
                          ((EQUAL MODEL :DOUBLE-HEXCONE)
                           (SETQ WMIN (/ 1 2) WMID 0 WMAX (/ 1 2)))
                          (T
                           (ERROR "Don't know this model ~s ."
                                  MODEL)))
                    (SETF LATQ (* (+ (* WMID FPRIME) WMAX) M))
                    (IF (<= L LATQ)
                        (PROGN (SETF MIN (* (- 1 S) L))
                               (SETF
                                MID
                                (/
                                 (+
                                  (* FPRIME L)
                                  (*
                                   MIN
                                   (-
                                    (* (- 1 FPRIME) WMAX)
                                    (* FPRIME WMIN))))
                                 (+ WMAX (* FPRIME WMID))))
                               (SETF
                                MAX
                                (/
                                 (- L (* WMID MID) (* WMIN MIN))
                                 WMAX)))
                      (PROGN (SETF MAX (+ (* S M) (* (- 1 S) L)))
                             (SETF
                              MID
                              (/
                               (-
                                (* (- 1 FPRIME) L)
                                (*
                                 MAX
                                 (-
                                  (* (- 1 FPRIME) WMAX)
                                  (* FPRIME WMIN))))
                               (+ (* (- 1 FPRIME) WMID) WMIN)))
                             (SETF
                              MIN
                              (IF
                               (> WMIN 0)
                               (/
                                (- L (* WMAX MAX) (* WMID MID))
                                WMIN)
                               (/
                                (- MID (* FPRIME MAX))
                                (- 1 FPRIME))))))
                    (CASE K
                      (0 (SETQ R MAX G MID B MIN))
                      (1 (SETQ R MID G MAX B MIN))
                      (2 (SETQ R MIN G MAX B MID))
                      (3 (SETQ R MIN G MID B MAX))
                      (4 (SETQ R MID G MIN B MAX))
                      (5 (SETQ R MAX G MIN B MID)))))
                (LIST R G B))
            (ERROR "Saturation values range from 0 to 1: ~s " S))
        (ERROR "Hue values range from 0 up to, but not including 360; ~%~
                    NIL for chromatic colours: ~s " H))
    (ERROR "Lightness values range from 0 to 1: ~s " L)))
----------------------------------------------------------------------
FUNCTION-TOO-LONG: Definition way too long!
----------------------------------------------------------------------
NESTED-IFS: Avoid nested IF's. Use AND, if possible, or a single COND.
----------------------------------------------------------------------
PROGN-IN-IF: Don't use IF and PROGN, use COND
----------------------------------------------------------------------
PROGN-IN-IF: Don't use IF and PROGN, use COND
----------------------------------------------------------------------
LET-ATOMS: Always initialize LET variables like B with (B NIL), not
just B. It's too easy to misread what's being initialized to what.
----------------------------------------------------------------------
LET-ATOMS: Always initialize LET variables like G with (G NIL), not
just G. It's too easy to misread what's being initialized to what.
----------------------------------------------------------------------
LET-ATOMS: Always initialize LET variables like R with (R NIL), not
just R. It's too easy to misread what's being initialized to what.
----------------------------------------------------------------------
USE-EQL: Unless something special is going on, use EQL, not EQUAL.
----------------------------------------------------------------------
**********************************************************************

(DEFUN RGB_TO_LHS (R G B &KEY (MODEL :TRIANGLE))
  (LET* ((CMAX (MAX R G B))
         (CMIN (MIN R G B))
         (CMID (ELT (SORT (LIST R G B) #'<=) 1))
         L
         H
         S)
    (IF (EQUAL CMAX CMIN)
        (SETQ L CMAX H NIL S 0)
      (LET* ((M 1)
             (K
              (COND ((AND (> R G) (>= G B)) 0)
                    ((AND (>= G R) (> R B)) 1)
                    ((AND (> G B) (>= B R)) 2)
                    ((AND (>= B G) (> G R)) 3)
                    ((AND (> B R) (>= R G)) 4)
                    ((AND (>= R B) (> B G)) 5)))
             (F
              (IF (EVENP K)
                  (/ (- CMID CMIN) (- CMAX CMIN))
                (/ (- CMAX CMID) (- CMAX CMIN))))
             WMIN
             WMID
             WMAX
             LATQ)
        (COND ((EQUAL MODEL :TRIANGLE)
               (SETQ WMIN (/ 1 3) WMID (/ 1 3) WMAX (/ 1 3)))
              ((EQUAL MODEL :HEXCONE) (SETQ WMIN 0 WMID 0 WMAX 1))
              ((EQUAL MODEL :DOUBLE-HEXCONE)
               (SETQ WMIN (/ 1 2) WMID 0 WMAX (/ 1 2)))
              (T (ERROR "Don't know this model ~s ." MODEL)))
        (SETF LATQ (* (+ (* WMID F) WMAX) M))
        (PROGN (SETF L (+ (* WMAX CMAX) (* WMID CMID) (* WMIN CMIN)))
               (SETF H (* (+ K F) 60))
               (SETF S
                     (IF (<= L LATQ)
                         (/ (- L CMIN) L)
                       (/ (- CMAX L) (- M L)))))))
    (LIST L H S)))
----------------------------------------------------------------------
FUNCTION-TOO-LONG: Definition way too long!
----------------------------------------------------------------------
COND-WITHOUT-DEFAULT: All COND's should have an ELSE branch, i.e., (T
...).
----------------------------------------------------------------------
USE-EQL: Unless something special is going on, use EQL, not EQUAL.
----------------------------------------------------------------------
**********************************************************************

(DEFUN HUE-OF (COLOUR &KEY (MODEL :TRIANGLE))
  (SECOND (RGB_TO_LHS (RED-OF COLOUR) (GREEN-OF COLOUR)
           (BLUE-OF COLOUR) :MODEL MODEL)))
----------------------------------------------------------------------
**********************************************************************

(DEFUN LIGHTNESS-OF (COLOUR &KEY (MODEL :TRIANGLE))
  (FIRST (RGB_TO_LHS (RED-OF COLOUR) (GREEN-OF COLOUR)
          (BLUE-OF COLOUR) :MODEL MODEL)))
----------------------------------------------------------------------
**********************************************************************

(DEFUN SATURATION-OF (COLOUR &KEY (MODEL :TRIANGLE))
  (THIRD (RGB_TO_LHS (RED-OF COLOUR) (GREEN-OF COLOUR)
          (BLUE-OF COLOUR) :MODEL MODEL)))
----------------------------------------------------------------------
**********************************************************************

(DEFUN LIGHTEN (COLOUR &KEY (FACTOR 0.2) (MODEL :TRIANGLE))
  (LET* ((H (WB::HUE-OF COLOUR))
         (L (WB::LIGHTNESS-OF COLOUR))
         (S (WB::SATURATION-OF COLOUR))
         (NEW-L (+ FACTOR L)))
    (COND ((< NEW-L 0) (SETF NEW-L 0)) ((> NEW-L 1) (SETF NEW-L 1)))
    (APPLY #'WB:MAKE-COLOR (WB::LHS_TO_RGB NEW-L H S :MODEL MODEL))))
----------------------------------------------------------------------
FUNCTION-TOO-LONG: Definition a little too long!
----------------------------------------------------------------------
COND-WITHOUT-DEFAULT: All COND's should have an ELSE branch, i.e., (T
...).
----------------------------------------------------------------------
**********************************************************************

(DEFUN DARKEN (COLOUR &KEY (FACTOR 0.2) (MODEL :TRIANGLE))
  (LIGHTEN COLOUR :FACTOR (* -1 FACTOR) :MODEL MODEL))
----------------------------------------------------------------------
