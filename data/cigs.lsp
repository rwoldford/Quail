(in-package :q-user)

(<- cig-names (list "Alpine" "Benson & Hedges" "Bull Durham"
                    "Camel Lights" "Carlton" "Chesterfield"
                    "Golden Lights" "Kent" "Kool" "L&M"
                    "Lark Lights" "Marlboro" "Merit"
                    "Multifilter" "Newport Lights" "Now"
                    "Old Gold" "Pall Mall Light" "Raleigh"
                    "Salem Ultra" "Tareyton" "True"
                    "Viceroy Rich Lights" "Virginia Slims" "Winston Lights"))


(<- cig-data '((14.1   0.86   .9853  13.6)
               (16.0  1.06  1.0938  16.6)
               (29.8  2.03  1.1650  23.5)
               (8.0    .67   .928   10.2)
               (4.1    .4    .9462   5.4)
               (15.0  1.04   .8885  15)
               (8.8    .76   1.0267   9)
               (12.4   .95   .9225  12.3)
               (16.6  1.12   .9372  16.3)
               (14.9  1.02   .8858  15.4)
               (13.7  1.01   .9643  13)
               (15.1   .9    .9316  14.4)
               (7.8    .57   .9705  10)
               (11.4   .78  1.124   10.2)
               (9.0    .74   .8517   9.5)
               (1.0    .13   .7851   1.5)
               (17    1.26   .9186  18.5)
               (12.8  1.08  1.0395  12.6)
               (15.8   .96   .9573  17.5)
               (4.5    .42   .9106   4.9)
               (14.5  1.01  1.007   15.9)
               (7.3    .61   .9806   8.5)
               (8.6    .69   .9693  10.6)
               (15.2  1.02   .9496  13.9)
               (12.0   .82  1.1184  14.9)))

(<- cig-vars (list "tar" "nicotine" "weight" "carbon-monoxide"))


(<- cigs (array cig-data))

(dataset cigs :identifiers cig-names :variates cig-vars
         :name "Cigarettes")