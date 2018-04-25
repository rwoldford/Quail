
(in-package :q-user)

(defvar coal-data
  NIL
  "Coal miners often encounter methane gas. ~
   Methane in coal mines has several implications: ~
   it can cause dangerous explosions; if captured, ~
   methane is potentially a cleaner fuel source than coal itself; ~
   and if simply released into the earth's atmosphere, ~
   methane contributes substantially to the greenhouse ases that may ~
   change the earth's climate. Depper coal tends to contain more ~
   methane. ~
   The following table gives depth and methane content of 15 core samples from the  ~
   Hartshorne coalbed of Oklahoma.~&~
   Depth is in feet; methane is in cc/g."
  )


(<- coal-vars '("depth" "methane"))
(<- coal-data
    (array
     '((175  2.5) 
       (252  5.7)
       (318  8.7)
       (356  10.8)
       (516  11.8)
       (553  13.1)
       (571  11.8)
       (561  11.5)
       (556 10.9 )
       (823 15.5)
       (892 16.8)
       (1439 17.1)
       (1440 16.7)
       (489 10.9)
       (1296 17.5))))

(dataset coal-data :variates coal-vars :name "Coal data")




