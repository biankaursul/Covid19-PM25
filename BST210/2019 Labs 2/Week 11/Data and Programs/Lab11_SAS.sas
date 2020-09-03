/******************************/
/* BST 210 Lab 11             */
/* November 8-9, 2017         */
/******************************/

/* Poisson Regression: An Example */

* Reading in the dataset;
proc import file="melanoma.csv" out=melanoma dbms=csv;
	getnames = YES;
run; 

* Fitting a Poisson regression model for the relationship between latitude and melanoma incidence;
* 	We typically use PROC GENMOD (as in generalized model) for fitting GLMs, including Poisson
*	regression. Note that we cannot use (the somewhat misleadingly named) PROC GLM to fit GLMs. 
*	That procedure was created before the generalized linear model framework had been developed,
*	and so PROC GLM actually only handles linear models.
*
*	SAS requires that we manually log-transform the observed person-time to create the offset, so
*	we need to create a new dataset first;
data melanoma2;
	set melanoma;
	logt = log(persyrs);
run;
proc genmod data=melanoma2;
	class latitude / param=glm; /* Allows us to store the results of the model later */
	model inccases = latitude / dist = poisson
								offset = logt
								type3; /* includes significance tests for each covariate */ 
	store lat1;
run;

* Testing significance of and finding a confidence interval for Northern versus Middle latitudes;
proc genmod data=melanoma2;
	class latitude; 
	model inccases = latitude / dist = poisson
								offset = logt
								type3; 
	estimate 'north_v_middle' latitude -1 1;
run;

* Adding in the main effect of age category;
proc genmod data=melanoma2;
	class latitude ageg / param=glm; 
	model inccases = latitude ageg / dist = poisson
									 offset = logt
									 type3; 
	store lat2;
run;

* Adding in an interaction term between age and latitude;
proc genmod data=melanoma2;
	class latitude ageg / param=glm; 
	model inccases = latitude ageg latitude*ageg / dist = poisson
									 			   offset = logt
									 			   type3; 
	store lat3;
run;

/* Overdispersion */

* Negative Binomial Regression;
proc genmod data=melanoma2;
	class latitude;
	model inccases = latitude / dist=negbin offset = logt type3;
run;

* Robust Variance Estimation;
proc genmod data=melanoma2;
	class latitude VAR1;
	model inccases = latitude / dist=poisson offset = logt type3;
	repeated subject=VAR1 / covb;
run;

/* Zero-Inflated Poisson Regression: An Example */

* Reading in the dataset;
proc import file="C:\Users\H00039478\Downloads\school_data.csv" out=school dbms=csv;
	getnames = YES;
run; 

* Creating a histogram of absences;
title 'Student Absences over the Course of a School Year';
proc univariate data=school noprint;
   histogram daysabs;
run;

* Fitting a ZIP with constant structural zero probability;
title ;
proc genmod data = school;
  model daysabs = mathnce langnce female /dist=zip;
  zeromodel /link = logit ;
run;

* Fitting a ZIP with structural zero probability dependent on mathnce;
proc genmod data = school;
  model daysabs = mathnce langnce female /dist=zip;
  zeromodel mathnce /link = logit ;
run;
