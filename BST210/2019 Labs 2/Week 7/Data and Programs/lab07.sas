/******************************/
/* BST 210 Lab 07             */
/* October 13-14, 2016        */
/******************************/

/* Connection to Contingency Tables */

* Reading in the smoker dataset;
proc import datafile='smoker.dta' out=smoker dbms=dta replace;
run;

* Fitting the logistic regression;
*	Note: by default, SAS assumes that--when we have binary outcomes--the outcome
*	Y=0 is the outcome of interest. Typically (and in both R and Stata), we consider 
*	the outcome of interest to be Y=1. To get SAS to treat Y=1 as the outcome of
*	interest, we need to include the 'descending' option;
proc logistic data=smoker descending;
	freq freq;
	model ssmoke = psmoke;
run;

/* Hypothesis Testing */

* Reading in the GLOW dataset;
proc import file='glow.csv' out=glow dbms=csv replace;
	getnames=yes;
run;

* Creating a new dataset with binary, categorical, and ordinal age;
data glow2;
	set glow;
	
	* Creating binary age;
	if age > 75 then age75 = 1;
		else age75 = 0;

	* Creating categorical age (with indicators);
	if age > 65 and age <= 75 then agei1 = 1;
		else agei1 = 0;
	if age > 75 then agei2 = 1;
		else agei2 = 0;

	* Creating ordinal age;
	if age <= 65 then agecat = 0;
		else if age <= 75 then agecat = 1;
			else agecat = 2;
run;

/* Binary Age */

* Creating a contingency table for Age > 75 years and bone fracture;
proc freq data=glow2;
	tables fracture*age75;
run;

* Fitting a simple logistic regression model;
*	Note that the Wald Chi-Square that SAS reports is the same Wald test we talked
*	about in class and in the lab -- it's simply the Wald Z-statistic squared. The
*	square of a Normal(0,1) random variable follows a Chi-square distribution, and 
*	so the SAS test should produce the same p-value/conclusion as the Wald Z-score
*	tests in Stata and R;
proc logistic data=glow2 descending;
	model fracture = age75;
	test age75 /* This doesn't quite perform a LRT, as it's still the Wald test */
run;

/* Categorical Age */

* Fitting a logistic regression model with categorical age;
proc logistic data=glow2 descending;
	model fracture = agei1 agei2;
	test agei2 = 2*agei1; /* This allows us to compare the cat models, but again it's a Wald test and not an LRT */
run;

* Fitting a logistic regression model with ordinal age;
proc logistic data=glow2 descending;
	model fracture = agecat;
run;

/* Continuous Age */

* Fitting a regression model with continuous age;
proc logistic data=glow2 descending;
	model fracture = age;
run;

/* Confounding */

* Fitting the unadjusted model;
proc logistic data=glow2 descending;
	model fracture = priorfrac;
run;

* Fitting the adjusted model;
proc logistic data=glow2 descending;
	model fracture = priorfrac age;
run;

/* Effect Measure Modification */

* Assessing whether prior history of fractures is an effect modifier;
proc logistic data=glow2 descending;
	model fracture = priorfrac|age;
	output out=pred
			predicted=phat;
run;

* Creating a scatterplot showing the predicted probabilities; 
proc sgplot data=pred;
	title 'Fitted Probability of Bone Fracture as a Function of Age';
	scatter x=age y=fracture;			
	scatter x=age y=phat / group=priorfrac;
run;

* Creating a scatterplot showing the predicted log odds; 
data pred2;
	set pred;
	logitphat = log(phat/(1-phat));
run;
proc sgplot data=pred2;
	title 'Fitted Log Odds of Bone Fracture as a Function of Age';			
	scatter x=age y=logitphat / group=priorfrac;
run;
