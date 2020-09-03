******************************
* BIO210 LAB 08              *
* October 19-20, 2017        *
******************************

* Reading in the GLOW dataset
import delimited glow.csv, clear

******************************
* Model Building             *
******************************

* Running a forward selection procedure
stepwise, pe(0.15): logit fracture fracscore raterisk smoke armassist momfrac premeno bmi height age priorfrac

* Running a backward elimination procedure
stepwise, pr(0.15): logit fracture fracscore raterisk smoke armassist momfrac premeno bmi height age priorfrac

* Running a stepwise selection procedure
* 	Remember that Stata forces p_removal > p_entry
stepwise, pr(.15) pe(.10) forward: logit fracture fracscore raterisk smoke armassist momfrac premeno bmi height age priorfrac

* Running a best subsets selection procedure
ssc install gvselect
gvselect <term> fracscore raterisk smoke armassist momfrac premeno bmi height age priorfrac, nmodels(2): logit fracture <term>

******************************
* Calibration                *
******************************

/* Pearson Chi-Square Test */

* Printing out a contingency table for our two covariates
table priorfrac armassist fracture

* Performing the Pearson Chi-Square test
logit fracture priorfrac armassist
estat gof, table

/* Hosmer-Lemeshow Test */

* Performing the H-L Test for the forward selection model
logit fracture fracscore raterisk height priorfrac
estat gof, group(10) table

* Performing the H-L Test for the backward elimination model
logit fracture raterisk armassist momfrac height age priorfrac
estat gof, group(10) table

******************************
* Discrimination             *
******************************

* Calculating the AUC for the forward selection model
logit fracture priorfrac raterisk fracscore height
lroc

* Calculating the AUC for the backward elimination model
logit fracture raterisk armassist momfrac height age priorfrac
lroc
