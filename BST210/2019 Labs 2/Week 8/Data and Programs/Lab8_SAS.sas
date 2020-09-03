/******************************/
/* BST 210 Lab 08             */
/* October 19-20, 2016        */
/******************************/

/* Model Building */

* Reading in the GLOW dataset;
proc import file='glow.csv' out=glow dbms=csv replace;
	getnames=yes;
run;

* Running a forward selection procedure;
proc logistic data=glow descending;
	model fracture = fracscore raterisk smoke armassist momfrac premeno 
		 	bmi height age priorfrac /selection = forward slentry = 0.15;
run;

* Running a backward elimination procedure;
proc logistic data=glow descending;
	model fracture = fracscore raterisk smoke armassist momfrac premeno 
  			bmi height age priorfrac /selection = backwards slstay = 0.15;
run;

* Running a stepwise selection procedure;
proc logistic data=glow descending;
	model fracture = fracscore raterisk smoke armassist momfrac premeno 
  			bmi height age priorfrac /selection = stepwise slentry = 0.15 slstay = 0.15;
run;

* Running a best subsets selection procedure;
* 	Note that SAS does not appear to allow best subset selection on the basis of AIC, but
* 	instead uses a 'chi-square' score statistic to determine the best subsets;
proc logistic data=glow descending;
	model fracture = fracscore raterisk smoke armassist momfrac premeno 
  			bmi height age priorfrac /selection = score;
run;

/* Calibration & the Pearson Chi-Square Test */

* Fitting our smaller model (just for this exercise);
proc logistic data=glow descending;
	model fracture = priorfrac armassist;
run;

* Printing out a contingency table for our two covariates;
proc freq data=glow;
	by fracture;
	table priorfrac*armassist;
run;

* Performing the Pearson Chi-Square test;
* 	Adding the aggregate scale=none option causes the Pearson chi-square results to be output as part of
*	the standard logistic regression printout; 
proc logistic data=glow descending;
	model fracture = priorfrac armassist / aggregate scale=none;
run;

* Performing the H-L Test for the forward selection model;
proc logistic data=glow descending;
	model fracture = fracscore raterisk height priorfrac /lackfit;
run;

* Performing the H-L Test for the backward elimination model;
proc logistic data=glow descending;
	model fracture = raterisk armassist momfrac height age priorfrac /lackfit;
run;


/* Discrimination & the Receiver-Operator Curve */

ods graphics on;

* Forward selection model;
proc logistic data=glow descending plots(only)=roc(id=obs);
      model fracture = fracscore raterisk height priorfrac;
run;

* Backward selection model;
proc logistic data=glow descending plots(only)=roc(id=obs);
      model fracture = raterisk armassist momfrac height age priorfrac;
run;

ods graphics off;
