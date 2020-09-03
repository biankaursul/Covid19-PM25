******************************
* BIO210 LAB 06              *
* October 10-11, 2019        *
******************************

* Reading in the Framingham Dataset;
proc import datafile='framingham.dta' out=framingham dbms=dta replace;
run;

* Remove all missing observations;
data framingham;
	set framingham;
	if cmiss(of _all_) then delete;
run;

/* Testing Hypotheses */

* Fitting regression model 1;
*	Note that this output includes all diagnostic plots for the model, which
*	we will return to later;
proc reg data=framingham;
	model totchol = age sex diabp cigpday bmi;
run;

* Performing an F-test;
*	SAS allows us to perform tests of coefficients/combinations or collections of
* 	coefficients through the 'test' option in PROC REG. So we can run several tests
*	at once. The first 'test' option below tests whether all the beta coefficients
* 	are 0, while the second 'test' option tests whether the bmi coefficient is 0;
proc reg data=framingham;
	model totchol = age sex diabp cigpday bmi;
	test age, sex, diabp, cigpday, bmi;
	test bmi;
run;

* Fitting a spline in SAS;
*	We can't perform an F-test of whether the model with linear bmi alone suffices directly from
*	PROC TRANSREG. Instead, we need to create the spline terms in the procedure, save them to a
* 	new dataset, and then use this dataset to fit a regression in PROC REG. From there, we can
* 	perform the F-test as before, with the 'test' option;
*
*	I'm still trying to figure out how exactly the PROC TRANSREG procedure works, and how it
*	generates the splines/stores them. Once I do have that worked out, I will update this code with
*	the PROC TRANSREG procedure.
*
*	We could also manually create cubic spline terms, fit a regression using them, and then test as
* 	before with the 'test' option. However, for the sake of simplicity and less coding, we'll just
*	fit a GAM for now.;
proc gam data=framingham;
	model totchol = param(age sex diabp cigpday bmi) spline(bmi);
run;

/* Robust Variances */

* Recall that the diagnostic plots for the first model we fit suggested that our
*	line assumptions might not hold;

* Running Model (1) again, this time with a robust variance estimator;
*	Note that SAS still provides the incorrect standard errors, as well. The corrected
*	standard errors can be found under the heading 'heteroscedasticity consistent';
proc reg data=framingham;
	model totchol = age sex diabp cigpday bmi / WHITE;
run;

/* Introduction to Logistic Regression */

* Reading in the Smokers Dataset;
proc import datafile='smoker.dta' out=smoker dbms=dta replace;
run;

* Creating a contingency table for the relationship between student smoking
* 	and parental smoking;
proc freq data=smoker;
	tables psmoke*ssmoke;
	weight freq;
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
