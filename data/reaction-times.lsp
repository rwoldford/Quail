(in-package :q-user)

(defvar reaction-times
  NIL
  "Thirty teams, of two people each, conducted a randomized experiment to ~
   measure the effect lighting has on a person's time to react to a visual ~
   stimulus. ~
   ~%~
   Each team had a wooden ruler which one person would drop for the other ~
   to catch between thumb and index finger.  ~
   The visual stimulus would be the movement of the ruler. ~
   ~%~
   In each team only one person dropped and the other caught on all trials.  ~
   Which member of the team would be the catcher  ~
   was determined by a coin toss for each team. ~
   ~%~
   The distance the ruler falls before being caught is a surrogate measurement of ~
   the catcher's reaction time.   ~
   A detailed protocol was established for dropping the ruler and measuring ~
   the distance dropped.  The distance is measured in millimetres along the ~
   ruler itself. ~
   The maximum value that a ruler could be measured to drop was 290 mm. ~
   ~%~
   There were two lighting conditions, low and high.  ~
   High light conditions were achieved by having all lights on in the classroom ~
   where the experimental study was conducted.  This gave a fairly ~
   uniform light intensity about the room.  ~
   Low light was achieved by having the doors closed and all lights turned ~
   out but that of a partially covered overhead projector located roughly in the ~
   centre of the room. ~
   Consequently, the low light condition was not evenly distributed ~
   throughout the room and so each team also recorded their distance from ~
   the low light source using ceiling tiles to provide a city block distance ~
   in units of tile-widths.  ~
   ~%~
   Under each condition two ~
   measurements were taken and their average reported.  ~  
   The order of lighting condition was randomized with roughly half the ~
   teams doing low light first then high light and half doing the opposite ~
   order. ~
   One team ~
   dropped the ruler on one of their low light trials so all that was ~
   known was that the measurement was > 290 mm.  Consequently the average ~
   the recorded under low light condition is also a lower bound. ~
   ~%~
   The experiment was carried out the morning of July 5, 1995 in my ~
   introductory Statistics course. ... R.W. Oldford.")

(<- reaction-times
    (array 
     '(163.5 150.5   13.0  4   "Male" "High Low" "OK"
       114.0 122.0   -8.0  2   "Male" "High Low" "OK"
       258.0 176.0   82.0  4   "Male" "High Low" "OK"
       256.5 155.0  101.5  6   "Male" "High Low" "OK"
       235.0 155.0   80.0  8   "Male" "Low High" "OK"
       277.0 218.0   59.0  7 "Female" "High Low" "OK"
       164.5 138.0   26.0  8 "Female" "Low High" "OK"
       216.0 222.0   -6.0 11 "Female" "Low High" "OK"
       204.0 121.5   82.5 12   "Male" "Low High" "OK"
       200.0 129.0   71.0  3   "Male" "Low High" "OK"
       209.0 144.0   65.0  9   "Male" "Low High" "OK"
       270.0 212.5   57.5  5 "Female" "Low High" "Ruler dropped in low light"
       110.0 145.0  -35.0  5 "Female" "High Low" "OK"
       103.5 156.5  -53.0  4   "Male" "High Low" "OK"
       236.5 173.0   63.5 12   "Male" "Low High" "OK"
       241.0 141.5   99.5 11   "Male" "Low High" "OK"
       234.0 146.5   87.5 10   "Male" "High Low" "OK"
       221.5 189.0   32.5 12   "Male" "Low High" "OK"
       167.0 139.0  128.0  7   "Male" "High Low" "OK"
       272.5 183.5   89.0  6 "Female" "High Low" "OK"
       162.5  64.0   98.5  6   "Male" "High Low" "OK"
       170.0 124.5   45.5  9 "Female" "Low High" "OK"
       247.0 163.0   84.0 11 "Female" "High Low" "OK"
       206.5 202.5    4.0  6 "Female" "Low High" "OK"
       242.5 205.0   37.5  8 "Female" "High Low" "OK"
       169.0 128.0   41.0  4 "Female" "Low High" "OK"
       180.0 126.5   53.5  3   "Male" "High Low" "OK"
       130.5 143.0  -12.5  5 "Female" "High Low" "OK"
       205.5 147.5   58.0 10 "Female" "High Low" "OK"
       157.0 131.0   26.0  7   "Male" "High Low" "OK")
     :dimensions '(30 7)))
    

(<- reaction-vars
    (list "low light (mm)" "high light (mm)" "Low - high (mm)"
          "City-block distance from source (ceiling tiles)"
          "Sex of catcher"
          "Lighting order"
          "Other comments"))



(dataset reaction-times
         :variates reaction-vars
         :name "Reaction times")





                    
                    
