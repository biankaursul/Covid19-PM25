/******************************************************************************/
/* BST 210 Lab 14
/* November 30-December 1, 2017
/******************************************************************************/

/* Two Sample Test of Proportions */

* Determining power to detect p0 = 0.6 versus p1=0.4 for a sample size of 100;
proc power;
	twosamplefreq test=pchi
		groupproportions = (.6 .4)
		npergroup = 50
		power = .;
run;

* Determining the sample size needed to achieve 90% power;
proc power; 
	twosamplefreq test=pchi
		groupproportions = (.6 .4)
		npergroup = .
		power = 0.9;
run;

* Determining power and sample size for a one-sided test;
proc power;
	twosamplefreq test=pchi
		groupproportions = (.6 .4)
		sides = 1
		npergroup = 50
		power = .;
proc power;
	twosamplefreq test=pchi
		groupproportions = (.6 .4)
		sides = 1
		npergroup = .
		power = 0.9;
run;

* Determining power given a 1:2 allocation ratio and a sample size of 261;
proc power;
	twosamplefreq test=pchi
		oddsratio = 0.4444
		refproportion = 0.6
		groupweights = (1 2)
		ntotal = 261
		power = .;
run;

* What sample size would we need if we wanted to achieve 90% power with a 1:2 allocation ratio;
proc power;
	twosamplefreq test=pchi
		oddsratio = 0.4444
		refproportion = 0.6
		groupweights = (1 2)
		ntotal = .
		power = 0.9;
run;


/* Logistic Regression */

* NOTE: SAS's method for assessing the power and sample size for a logistic regression analysis assumes that all predictor variables
* 	are independent of one another. In other words, it assumes that the R^2 is fixed at 0. This may or may not be applicable to the
* 	new study we are planning on designing: while it may be reasonable to assume that any additional predictors are uncorrelated with 
* 	our predictor of interest in a randomized trial, the independence assumption will likely not hold if we are conducting an 
* 	observational study.; 

* Reading in the GLOW data;
proc import file='glow.csv' out=glow dbms=csv replace;
	getnames=yes;
run;

* Determining the mean covariate values in our dataset, as well as the standard deviation for age;
* 	To get the fitted values, you can always add new rows to the glow dataset: one row corresponding to individuals with a mean value
* 	for all three covariates, and the second row corresponding to an individual with mean values for BMI and priorfrac, and with an
* 	age equal to mean_age + sd_age. Keep both of their outcomes as missing. Then, when we fit the logistic regression model on this new 
* 	data set, we can output the fitted probabilities for all observations. The last two rows of this output will correspond to the 
* 	fitted values that we're interested in. However, I would recommend just doing the calculation by hand, as that's a little bit easier
* 	especially given that we're only considering/concerning ourselves with a small number of covariates.;
proc means data=glow;
	var age bmi priorfrac;
run;
proc logistic data=glow descending;
	model fracture = age bmi priorfrac;
run;

* Determining the sample size needed for an array of desired powers;
proc power;
	logistic
		vardist("age") = normal(0, 1)
			testpredictor = "age"
			testoddsratio = 1.505334
			responseprob = 0.234
		alpha = 0.05
		power = 0.60 0.70 0.80 0.90
		defaultnbins = 100
		ntotal = .;
run;

* See lab and the corresponding Stata code for how it is that the sample size calculations change/are impacted by changes in the R^2
* 	between the different covariates included in our logistic regression model;


/* Survival Analysis */

* Reading in the data;
proc import datafile='chemotherapy.dta' out=chemo dbms=dta replace;
run;

* Hazard ratio and CI for the pilot study;
proc phreg data=chemo;
	model time*relapse(0) = group /rl;
run;

* Standard deviation of our predictor variable (needed for the sample size calculation in SAS);
proc means data=chemo;
	var group;
run;

* Please see https://support.sas.com/documentation/cdl/en/statug/63033/HTML/default/viewer.htm#statug_power_sect014.htm
* 	for how to perform power and sample size calculations in SAS for a two-sample logrank test. Unfortunately, the SAS function 
* 	requires specifying more information than either Stata or R (for example, if we want to characterize our effect of interest in
* 	terms of the hazard ratio, we need to include an estimate of the Kaplan-Meier curve for the control group. In the interest of
* 	saving time, I would recommend not using SAS for the logrank power calculations, but instead using R or Stata. There may be another
* 	way of calculating power for a log-rank test in SAS, but I can't currently find anything simpler.

* If we want to fit a Cox proportional hazards model and look at the hazard ratio associated with a continuous covariate (and 
* 	allowing for the possibility that there may be additional predictors of any type in the model), we can use the coxreg option in
*	the power proc to calculate sample sizes and/or power. To get a sense of how this function works, please see
* 	http://support.sas.com/documentation/cdl/en/statug/68162/HTML/default/viewer.htm#statug_power_syntax02.htm
