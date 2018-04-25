;;; BUGS in MOdels


;;; From the cars example.  try instead the following

(glm "amount ~ 1 + v_age + ph_age + group + group . v_age"
              cars-dat
              :family :gamma
              :link :reciprocal
              :weight (eref cars-dat "number")
              :tolerance 1e-3)
#|
You get 

> Error: value NAN is not of the expected type NUMBER.
> While executing: #<STANDARD-METHOD FIT (GENERALIZED-LINEAR-MODEL-FIT)>
> Type Command-. to abort.
See the RestartsÉ menu item for further choices.
1 > 

I believe this is the result of a 0/0.0 around observation 98.
There are several NaNs.
