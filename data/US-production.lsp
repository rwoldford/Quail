(in-package :q-user)

(defvar US-production
  NIL
  "A dataset containing on output, labour input, and capital input ~
   for the United States for the period 1929-1967. ~
   (:elaboration The variables are year, the index of gross national product ~
   in constant dollars, the labour input index or number of persons adjusted ~
   for hours of work and educational level, the number of persons engaged, ~
   the capital input index or capital stock adjusted for rates of utilization, ~
   and the capital stock in constant dollars.) ~
   (:references L.R. Christensen and D.W. Jorgenson. U.S. Real Product and ~
   Real Factor Input 1929-67, Review of Income and Wealth, March 1970. ~
   Data were taken from G.S. Maddala 1988 Introduction to Econometrics: ~
   Second Edition. MacMillan Publishing Company, New York.)"
  )

(<- prod-vars (list "Year" "GNP" "Labour"
                    "Persons" "Capital"
                    "Stock"))

(<- US-production
    (array
     '(1929 189.8 173.3 44.151 87.8 888.9

       1930 172.1 165.4 41.898 87.8 904.0
       1931 159.1 158.2 36.948 84.0 900.2
       1932 135.6 141.7 35.686 78.3 883.6
       1933 132.0 141.6 35.533 76.6 851.4
       1934 141.8 148.0 37.854 76.0 823.7

       1935 153.9 154.4 39.014 77.7 805.3
       1936 171.5 163.5 40.765 79.1 800.4
       1937 183.0 172.0 42.484 80.0 805.5
       1938 173.2 161.5 40.039 77.6 817.6
       1939 188.5 168.6 41.443 81.4 809.8

       1940 205.5 176.5 43.149 87.0 814.1
       1941 236.0 192.4 45.576 96.2 830.3
       1942 257.8 205.1 49.010 104.4 857.9
       1943 277.5 210.1 49.695 110.0 851.4
       1944 291.1 208.8 48.668 107.8 834.6

       1945 284.5 202.1 47.136 102.1 819.3
       1946 274.0 213.4 49.950 97.2 812.3
       1947 279.9 223.6 52.350 105.9 851.3
       1948 297.6 228.2 53.336 113.0 888.3
       1949 297.7 221.3 51.469 114.9 934.6
       
       1950 328.9 228.8 52.972 124.1 964.6
       1951 351.4 239.0 55.101 134.5 1021.4
       1952 360.4 241.7 55.385 139.7 1068.5
       1953 378.9 245.2 56.226 147.4 1100.3
       1954 375.8 237.4 54.387 148.9 1134.6

       1955 406.7 245.9 55.718 158.6 1163.2
       1956 416.3 251.6 56.770 167.1 1213.9
       1957 422.8 251.5 56.809 171.9 1255.5
       1958 418.4 245.1 55.023 173.1 1287.9
       1959 445.7 254.9 56.215 182.5 1305.8

       1960 457.3 259.6 56.743 189.0 1341.4
       1961 466.3 258.1 56.211 194.1 1373.9
       1962 495.3 264.6 57.078 202.3 1399.1
       1963 515.5 268.5 57.540 205.4 1436.7
       1964 544.1 275.4 58.508 215.9 1477.8

       1965 579.2 285.3 60.055 225.0 1524.4
       1966 615.6 297.4 62.130 236.2 1582.2
       1967 631.1 305.0 63.162 247.9 1645.3)
     :dimensions '(39 6)))

(<- years (loop for i from 1929 to 1967 collect (format NIL "US ~s" i)))

(dataset us-production :identifiers years :variates prod-vars
         :name "US Production 1929-1967")
