/******************************************************************************/
/* BST 210 Lab 2
/* September 7-8, 2017
/******************************************************************************/

/* Importing the csv file 'lab2.csv' */
proc import file='lab2.csv' out=lab dbms=csv replace;
	getnames=yes;
run;

/* Creating a scatterplot for the relationship between SBP and quet */
title 'Relationship between SBP and quet';		/* Adds a title to the plot */
proc sgplot data=lab;
	scatter x=quet y=sbp;
run;

/* Running simple linear regression for the association between SBP and quet */
title ;
proc reg data=lab;
	model sbp=quet;
run;

/* Assessing Model Fit */
/*  Statistics such as the R^2, adjusted R^2, and MSE are all
/*  reported as part of the standard PROC REG output */

/* Hypothesis Testing */
/*  This is included in the output of the PROC REG procedure, but
/*  can also be done by adding a 'test' statement to the procedure */
proc reg data=lab;
	model sbp=quet;
	test quet=0; /*If we want to compare the coefficient to a number other than zero, we can adjust here */
run;

/* Confidence interval for the slope */
/*  The standard error is given in the PROC REG output.
/*  We can also use the 'clb' option of the model statement
/*  of the PROC REG procedure (see end of lab). */

/* Confounding */
/*  Performing multiple linear regression with both age and quet as predictors */
proc reg data=lab;
	model sbp = quet age;
run;

/* Creating a dataset with an additional interaction variable between smoking status and quet */
data lab2;
	set lab;
	quet_smk = quet*smk;
run;

/* Checking that we correctly added a new variable for the interaction term */
proc print data=lab2;
run;

/* Effect Measure Modification */
/*  Performing multiple linear regression with the interaction term included */
proc reg data=lab2;
	model sbp = quet smk quet_smk;
run;

/* Plotting Fitted Regression Lines Over a Scatterplot */
/* 	Please note that, while SAS is great at many things, graphics are not its 
/* 	strong suit. So the code below will not plot the fitted regression models
/* 	for smokers and non-smokers, per se, but will instead overlay the fitted 
/* 	regression values on top of the scatterplot of SBP on quetelet index, 
/*  colorcoding those fitted values by smoking status. */

/* 	We first need to output a dataset that includes our fitted/predicted values */
proc reg data=lab2;
	model sbp = quet smk quet_smk;
	output out=pred
		p=yhat;
run;
/* 	We can then overlay these fitted values on top of our scatter plot */
proc sgplot data=pred;	
	xaxis label = "Quetelet Index";
	yaxis label = "Systolic Blood Pressure (mmHg)"; 	
	scatter x=quet y=sbp / ;			
	scatter x=quet y=yhat / GROUP=smk markerattrs = (symbol = circlefilled); 
	* GROUP specified color-coding by group, and markerattrs changes the specific attributes of the points,
	*  here making them filled dots;
run;


/* Additional Topic: Confidence Intervals for Predicted Values */

/* Confidence interval for a predicted subpopulation mean */
/*  SAS is able to generate predictions for observations
/*  in the dataset. If you want to generate a prediction 
/*  for an unobserved value, add it as an additional
/*  observation to your dataset, with a missing value for
/*  the outcome (see below). 
/*  The 'clm' option of the model statement in the PROC REG
/*  procedure will generate confidence intervals for a 
/*  predicted mean. */

/* Confidence interval for an individual prediction */
/*  The 'cli' option of the model statement in the PROC REG
/*  procedure will generate confidence intervals for a 
/*  predicted individual value. */

/* Adding observation with quet=3.5 to the dataset */
data lab3;
	set lab2 end=eof;
	output;
	if eof then do;
		person=33;
		quet=3.5;
		sbp=.;
		output;
	end;
run;

/* Checking that we correctly added an individual corresponding to our desired predicted value */
title "check that last row is added";
proc print data=lab3;
run;

/* Running simple linear regression with added arguments for all three (95%) CIs */
title ;
proc reg data=lab3;
	model sbp = quet / clb clm cli;
run;

proc print data=lab;
run;
