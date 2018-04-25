(in-package :q-user)

(defvar brain-body-wts
  NIL
  "The average brain weight and average body weight of many species.")

(<- brain-body-wts
    (array 
     '( 3.385   44.500   .530 1.648
        .480   15.499  -.319 1.190
        1.350    8.100   .130  .909
        465.000  423.000  2.667 2.626
        36.330  119.500  1.560 2.077
        27.660  115.000  1.442 2.061
        14.830   98.200  1.171 1.992
        1.040    5.500   .017  .740
        4.190   58.000   .622 1.763
        .425    6.400  -.372  .806
        .101    4.000  -.996  .602
        .920    5.700  -.036  .756
        1.000    6.600      0  .820
        .005     .140 -2.301 -.854
        .060    1.000 -1.222     0
        3.500   10.800   .544 1.033
        2.000   12.300   .301 1.090
        1.700    6.300   .230  .799
        2547.000 4603.000  3.406 3.663
        .023     .300 -1.638 -.523
        187.100  419.000  2.272 2.622
        521.000  655.000  2.717 2.816
        .785    3.500  -.105  .544
        10.000  115.000  1.000 2.061
        3.300   25.600   .519 1.408
        .200    5.000  -.699  .699
        1.410   17.500   .149 1.243
        529.000  680.000  2.723 2.833
        207.000  406.000  2.316 2.609
        85.000  325.000  1.929 2.512
        .750   12.300  -.125 1.090
        62.000 1320.000  1.792 3.121
        6654.000 5712.000  3.823 3.757
        3.500    3.900   .544  .591
        6.800  179.000   .833 2.253
        35.000   56.000  1.544 1.748
        4.050   17.000   .607 1.230
        .120    1.000  -.921  .000
        .023     .400 -1.638 -.398
        .010     .250 -2.000 -.602
        1.400   12.500   .146 1.097
        250.010  489.997  2.398 2.690
        2.500   12.100   .398 1.083
        55.500  175.000  1.744 2.243
        100.000  157.000  2.000 2.196
        52.160  440.000  1.717 2.643
        10.550  179.500  1.023 2.254
        .550    2.400  -.260  .380
        60.000   81.000  1.778 1.908
        3.600   21.000   .556 1.322
        4.288   39.200   .632 1.593
        .280    1.900  -.553  .279
        .075    1.200 -1.125  .079
        .122    3.000  -.914  .477
        .048     .330 -1.319 -.482
        192.000  180.000  2.283 2.255
        3.000   25.000   .477 1.398
        160.000  169.000  2.204 2.228
        .900    2.600  -.046  .415
        1.620   11.400   .210 1.057
        .104    2.500  -.983  .398
        4.235   50.400   .627 1.702)
     :dimensions '(62 4)))

(<- brain-body-wt-vars
    (list "body weight" "brain weight" "log body weight" "log brain weight"))

(<- species (list "Arctic fox" "Owl Monkey" "Mountain beaver" "Cow" "Gray wolf" 
                  "Goat" "Roe Deer" "Guinea Pig" "Vervet" "Chinchilla"
                  "Ground squirrel" "Arctic ground squirrel" 
                  "African giant pouched rat" "Lesser short-tailed shrew"
                  "Star nosed mole" "Nine-banded armadillo" "Tree hyrax"
                  "North American opossum" "Asian elephant" "Big brown bat"
                  "Donkey" "Horse" "European hedgehog" "Patas monkey"
                  "Cat" "Galago" "Genet" "Giraffe" "Gorrila" "Gray Seal"
                  "Rock hyrax" "Human" "African elephant" "Water opossum"
                  "Rhesus monkey" "Kangaroo" "Yellow-bellied marmot"
                  "Golden hamster" "Mouse" "Little brown bat"
                  "Slow loris" "Okapi" "Rabbit" "Sheep" "Jaguar"
                  "Chimpanzee" "Baboon" "Desert hedgehog" "Giant armadillo"
                  "Rock hyrax" "Racoon" "Rat" "Eastern American Mole"
                  "Mole rat" "Musk shrew" "Pig" "Echidna" "Brazilian tapir"
                  "Tenrec" "Phalanger" "Tree shrew" "Red fox"))

(dataset brain-body-wts
         :identifiers species
         :variates brain-body-wt-vars
         :name "Brain and body weights")





                    
                    
