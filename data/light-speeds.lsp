(defvar light-speeds
  NIL
  "Historical measurements of the speed of light. ~
   (:reference K.D. Froome and L. Essen 1969 The Velocity of Light and Radio Waves. ~
   Academic Press, J.H. Sanders 1965 Velocity of Light Pergamon Press, ~
   Simon Newcomb 1882 Measures of the Velocity of Light, US Nautical Almanac pp 107-239.)"
)

(<- light-speeds
    (array
     '(
       ;;  Astronomical measurements rows 0-2
       1676	"Roemer"		        "Jupiter satellites"	214000	       NaN 0  0
       1676	"Delambre"		        "Jupiter satellites"	300270	       NaN 0  0
       1726	"Bradley"      		        "Aberration of stars"	301000	       NaN 0  0
       
       ;; Optical methods rows 3-5
       1849	"Fizeau"		        "Toothed wheel"	        315300         NaN 1  1
       1872	"Cornu"			        "Toothed wheel"	        298500         900 1  1
       1908	"Perrotin & Prim"	        "Toothed wheel"	        299901        	84 1  1
       
       ;; Optical methods: rotating mirror(s) 6-16
       1862	"Foucault"		        "Deflection of light"	298000	       500 2  1
       1874	"Cornu"			        "Deflection of light"	298500         NaN 2  1
       1878	"Cornu"			        "Deflection of light"	300400         800 2  1
       1878	"Michelson"		        "Deflection of light"	300140	       700 2  1
       1879	"Michelson"		        "Deflection of light"	299910	        50 2  1
       1882	"Newcomb"		        "Deflection of light"	299810	        50 2  1
       1882	"Newcomb"		        "Deflection of light"	299860	        30 2  1
       1882	"Michelson"		        "Deflection of light"	299853        	60 2  1
       1924	"Michelson"	                "Deflection of light"	299802	        30 2  1
       1926	"Michelson"	                "Deflection of light"	299796	         4 2  1
       1935	"Michelson, Pease and Pearson"	"Deflection of light"	299774	        11 2  1
       
       ;; Optical methods: Kerr cell 17-20
       1928	"Karolus and Mittelstaedt"	"Kerr cell"        	299778	        20 3 1
       1937	"Anderson"	                "Kerr cell"		299771	        12 3 1
       1940	"Huttel"	                "Kerr cell"		299768	        10 3 1
       1941	"Anderson"	                "Kerr cell"		299776	        14 3 1
       
       ;; Electrical methods: Velocity of electomagnetic radiation in vacuo - rows 21-25
       1891	"Blondlot"		        "Velocity of electomagnetic radiation in vacuo: Lecher wires"
       297600         NaN 4 2
       1895	"Trowbridge & Duane"	        "Velocity of electomagnetic radiation in vacuo: Lecher wires"
       300300         NaN 4 2
       1897	"Saunders"		        "Velocity of electomagnetic radiation in vacuo: Lecher wires"
       299700         NaN 4 2
       1899	"MacLean"		        "Velocity of electomagnetic radiation in vacuo: Free space"
       299100         NaN 4 2
       1923	"Mercier"	                "Velocity of electomagnetic radiation in vacuo:Lecher wires"
       299700 	        30 4 2

       ;; Electrical methods: Ratio of electromagnetic to electrostatic units - rows 26 - 42
       1857	"Weber and Kohlrausch"	"Ratio of electromagnetic to electrostatic units" 
       310800         NaN 5 2
       1868	"Maxwell" "Ratio of electromagnetic to electrostatic units" 
       284300         NaN 5 2
       1869	"Thomson and King" "Ratio of electromagnetic to electrostatic units" 
       280900         NaN 5 2
       1874	"McKichan" "Ratio of electromagnetic to electrostatic units" 
       289700         NaN 5 2
       1879	"Ayrton and Perry" "Ratio of electromagnetic to electrostatic units" 
       296100         NaN 5 2
       1880	"Shida" "Ratio of electromagnetic to electrostatic units" 
       295600         NaN 5 2
       1883	"Thomson, J.J."	 "Ratio of electromagnetic to electrostatic units" 
       296400         NaN 5 2
       1884	"Klemencic" "Ratio of electromagnetic to electrostatic units" 
       302000         NaN 5 2
       1888	"Thomson, W." "Ratio of electromagnetic to electrostatic units" 
       300500         NaN 5 2
       1889	"Rosa" "Ratio of electromagnetic to electrostatic units" 
       300090         NaN 5 2
       1890	"Thomson, J.J. and Searle" "Ratio of electromagnetic to electrostatic units" 
       299690         NaN 5 2
       1891	"Pellat" "Ratio of electromagnetic to electrostatic units"  
       301010         NaN 5 2
       1892	"Abraham" "Ratio of electromagnetic to electrostatic units"  
       299220         NaN 5 2
       1897	"Hurmuzescu" "Ratio of electromagnetic to electrostatic units"  
       300190         NaN 5 2
       1898	"Perot and Fabry" "Ratio of electromagnetic to electrostatic units"  
       299870         NaN 5 2
       1899	"Lodge and Glazesbrook"	 "Ratio of electromagnetic to electrostatic units" 
       301000         NaN 5 2
       1907	"Rosa and Dorsey"  "Ratio of electromagnetic to electrostatic units"
       299710	        30 5 2
      
       ;; Cavity Resonator - rows 43-45
       1947	"Essen and Gordon-Smith"	"Cavity Resonator"	299792	         3 6 3
       1950	"Essen"	                        "Cavity resonator"	299792.5	 1 6 3
       1950	"Hansen and Bol"	        "Cavity resonator"	299789.3  	 0.8 6 3
       
       ;; Radar - rows 46-50
       1947	"Smith, Franklin and Whiting"	"Radar"	                299695	        50 7 4
       1947	"Jones"	                        "Radar"			299687	        25 7 4
       1949	"Aslakson"	                "Radar"			299792.4	 2.4 7 4
       1949	"Jones and Cornford"	        "Radar"		        299701	        25 7 4
       1951	"Aslakson"	                "Radar"			299794.2	 1.4 7 4
       
       ;; Modulated light beam Methods - rows 51-58
       1949	"Bergstrand"	                "Geodimeter"		299796	         2 8 5
       1950	"Bergstrand"	                "Geodimeter"		299793.1	 0.26 8 5
       1951	"Bergstrand"	                "Geodimeter"		299793.1	 0.4 8 5
       1954     "Mackenzie"                     "Geodimeter"            299792.3         0.5 8 5
       1955	"Scholdstrom"	                "Geodimeter"		299792.4	 0.4 8 5
       1956	"Edge"	                        "Geodimeter"		299792.4	0.11 8 5
       1956	"Edge"	                        "Geodimeter"		299792.2	0.13 8 5
       1956     "Waller"                        "Geodimeter"            299792.5        NaN 8 5
       
       ;; Modulated light beam Methods - rows 59-60
       1966	"Karolus"	                "Modulated light beam"	299792.1	 0.2 9 5
       1967	"Karolus"	                "Modulated light beam: Corrected 1967"
                                                                        299792.44	0.2 9 5
       
       ;; Modulated light beam Methods - quartz - rows 61-2 
       1950	"McKinley"	                "Quartz modulator"	299780	        70 10 5
       1950	"Houstoun"	                "Quartz modulator"	299775	         9 10 5
       
       ;; Radio interferometry - rows 63-68
       1951	"Froome"	                "Radio interferometer"	299792.6	 0.7 11 6
       1954	"Froome"	                "Radio interferometer"	299793.0	 0.3 11 6
       1958	"Froome"	                "Radio interferometer corrected 1958"	
                                                                        299792.75	 0.3 11 6
       1954	"Florman"	                "Radio interferometer"	299795.1	 3.1 11 6
       1958	"Froome"	                "Radio interferometer"	299792.5	 0.1 11 6
       1967	"Simkin, Lukin, Sikora and Strelenskii"	"Microwave interferometer"
                                                                        299792.56	0.11 11 6
                                                                      
       ;; Spectral Lines  - rows 69-72
       1952	"Rank, Ruth and Vanden Sluis"	"Spectral lines"	299776	         6 12 7
       1954	"Rank, Shearer and Wiggins"	"Spectral lines"	299789.8	 3 12 7
       1955	"Plyler, Blaine and Cannon"	"Spectral lines"	299792	         6 12 7
       1956	"Rank, Bennett and Bennett"	"Spectral lines"	299791.9	 2 12 7
       
       ;; Tellurometer - rows 73-75
       1956	"Wadley"	                "Tellurometer"		299792.9       NaN   13 8
       1956	"Wadley"	                "Tellurometer"		299792.7	 2.0 13 8
       1957	"Wadley"                	"Tellurometer"		299792.6	 1.2 13 8
       
       )
:dimensions '(76 7)))

(<- light-speed-vars '("Year" "Authors" "Method" "Speed" "Error" "Method number" "Method group number"))

(dataset light-speeds :variates light-speed-vars :name "Light Speed Determinations")


(defvar astronomical-light
      (ref light-speeds
           '(0 1 2)
           T)
      "Light speeds calculated from astronomical observations. ~
       (:see-also light-speeds)")
(dataset astronomical-light :name "Light speed from astronomical observations."
         :variates light-speed-vars)

(defvar toothed-wheel
      (ref light-speeds
           '(3 4 5)
           T)
      "Light speeds calculated using Fizeau's toothed wheel. ~
       (:see-also light-speeds)")
(dataset toothed-wheel :name "Light speed: toothed wheel."
         :variates light-speed-vars)

(defvar rotating-mirror
      (ref light-speeds
           (loop for i from 6 to 16 collect i)
           T)
      "Light speeds using optical methods based on deflecting light from a rotating mirror. ~
       (:see-also light-speeds)")
(dataset rotating-mirror :name "Optical methods: rotating mirror(s)"
         :variates light-speed-vars)

(defvar kerr-cell
      (ref light-speeds
           (loop for i from 17 to 20 collect i)
           T)
      "Light speeds using optical methods based on a Kerr cell. ~
       (:see-also light-speeds)")
(dataset kerr-cell :name "Optical methods: Kerr cell"
         :variates light-speed-vars)

(defvar electromagnetic-radiation
      (ref light-speeds
           (loop for i from 21 to 25 collect i)
           T)
      "Light speeds using electrical methods based on the velocity of ~
       electromagnetic radiation in vacuo. ~
       (:see-also light-speeds)")
(dataset electromagnetic-radiation :name "Electrical methods: Electromagnetic radiation"
         :variates light-speed-vars)

(defvar ratio-of-units
      (ref light-speeds
           (loop for i from 26 to 42 collect i)
           T)
      "Light speeds using electrical methods based on the ratio of ~
       electromagnetic to electrostatic units. ~
       (:see-also light-speeds)")
(dataset ratio-of-units :name "Electrical methods: Ratio of units"
         :variates light-speed-vars)

(defvar cavity-resonator
      (ref light-speeds
           (loop for i from 43 to 45 collect i)
           T)
      "Light speeds using a cavity resonator. ~
       (:see-also light-speeds)")
(dataset cavity-resonator :name "Cavity resonator"
         :variates light-speed-vars)

(defvar radar
      (ref light-speeds
           (loop for i from 46 to 50 collect i)
           T)
      "Light speeds using radar. ~
       (:see-also light-speeds)")
(dataset radar :name "Radar"
         :variates light-speed-vars)

(defvar geodimeter
      (ref light-speeds
           (loop for i from 51 to 58 collect i)
           T)
      "Light speeds using a geodimeter. ~
       (:see-also light-speeds)")
(dataset geodimeter :name "Geodimeter"
         :variates light-speed-vars)

(defvar mod-light-beams
      (ref light-speeds
           (loop for i from 59 to 62 collect i)
           T)
      "Light speeds using modulated light beams. ~
       (:see-also light-speeds)")
(dataset mod-light-beams :name "Modulated light beams: other"
         :variates light-speed-vars)

(defvar radio-interferometry
      (ref light-speeds
           (loop for i from 63 to 68 collect i)
           T)
      "Light speeds using radio interferometry. ~
       (:see-also light-speeds)")
(dataset radio-interferometry :name "Radio interferometry"
         :variates light-speed-vars)

(defvar spectral-lines
      (ref light-speeds
           (loop for i from 69 to 72 collect i)
           T)
      "Light speeds using spectral lines. ~
       (:see-also light-speeds)")
(dataset spectral-lines :name "spectral lines"
         :variates light-speed-vars)

(defvar tellurometer
      (ref light-speeds
           (loop for i from 73 to 75 collect i)
           T)
      "Light speeds using a tellurometer. ~
       (:see-also light-speeds)")
(dataset tellurometer :name "spectral lines"
         :variates light-speed-vars)

(defconstant speed-of-light
  299792.458
  "The 1986 established speed of light in kilometres per second. ~
   (:reference Cohen, E.R. and B.N. Taylor 1987 The 1986 adjustment of the ~
   physical constants. Reviews of Modern Physics Volume 59 No. 4 pp. 1121-1148.)")
                                   

#| Some pictures

(loop for dataset in
      (list astronomical-light toothed-wheel rotating-mirror  kerr-cell electromagnetic-radiation
        ratio-of-units cavity-resonator radar geodimeter mod-light-beams radio-interferometry
        spectral-lines tellurometer)
      as title in
      '("Astronomy" "Toothed wheel" "Rotating Mirror" "kerr cell" 
        "electromagnetic radiation" "ratio of units" "cavity resonator"
        "radar and microwave" "geodimeter" "modulated light beams" "radio interferometry"
        "spectroscopy" "tellurometer")
      
      do
      (let ((sp (scatterplot :data dataset :x "Year" :y "Speed" :title (string-capitalize title)
                             :symbol :cross)))
        (add-line (interior-view-of sp) :orientation :horizontal :intercept speed-of-light)
      ))

(let (layout scatterplots )
  (setf scatterplots
        (loop for dataset in
              (list kerr-cell geodimeter
                     electromagnetic-radiation cavity-resonator
                    radar  mod-light-beams radio-interferometry
                    spectral-lines tellurometer)
              as title in
              '("kerr cell" "geodimeter"
                 "electromagnetic radiation" "cavity resonator"
                "radar and microwave" "modulated light beams" "radio interferometry"
                "spectroscopy" "tellurometer")
              collect
              
              (let ((sp
                     (scatterplot :data dataset :x "Year" :y "Speed" :title (string-capitalize title)
                                  :draw? NIL
                                  :symbol :cross)))
                
                sp)))
  (<- layout (grid-layout :subviews scatterplots
                          :nrows 3  :box-views? NIL
                          :gap-x 0.05 :gap-y 0.05))
  (grid-plot :interior-view layout :gap-x 0.05 :gap-y 0.05
             :draw? T)
  (loop for s in scatterplots
        do (add-line (interior-view-of s)
                     :slope 0.00001 :intercept speed-of-light
                     :orientation :horizontal))

  )

|#