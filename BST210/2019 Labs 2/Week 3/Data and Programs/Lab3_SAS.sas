*###########################
#   BST210 LAB 03          #
#   September 19-20, 2019  #
############################;

proc import datafile="lab03_outliers.csv" out=lab03 dbms=csv replace;
 getnames=yes;
 format smk smklabel.;
 run;

* scatter plot, systolic blood pressure (sbp) versus individual covariates;
** creating and applying label for smoking variable (to ease plotting);
proc format;
 value smklabel
  0='nonsmoker                '
  1='current or present smoker';
 run;

proc datasets nolist;
 modify lab03;
 format smk smklabel.;
 label smk="smoker status";
 run;

* generating the actual plot;
proc sgplot data=lab03;
 scatter x=quet y=sbp / group=smk;
 xaxis label="body size (units)";
 yaxis label="systolic blood pressure (SBP)";
 title "Systolic Blood Pressure Versus Body Index";
 run;

proc sgplot data=lab03;
 scatter x=age y=sbp / group=smk;
 xaxis label="age (years)";
 yaxis label="systolic blood pressure (SBP)";
 title "Systolic Blood Pressure Versus Age";
 run;

* regress sbp on covariates;
proc reg data=lab03;
 model sbp = age quet smk;
 output out=diagnostics
 	h = leverage
	cookd = cooksd
	dffits = dffits
	student = sy;
 run;

* The default PROC REG output produces a number of residual
  plots for us:
  1. residuals versus predicted values
  2. jackknife residuals versus predicted values
  3. jackknife residuals versus leverage
  4. Normal Q-Q plot with residuals
  5. observed outcome versus predicted outcome
  6. Cook's Distance plot
  7. histogram of residuals w/ overlayed Normal curve

  ... as well as plots of the residuals versus each covariate.;

 * print observations with high leverage;
 proc print data=diagnostics;
 	where leverage > (2*(3+1))/34;
	run;

* print observations with high cook's distance;
proc print data=diagnostics;
	where cooksd > 4/(34-2);
	run;

* print observations with large dffits;
proc print data=diagnostics;
	where abs(dffits) > 2*sqrt((3+1)/34);
	run;

* create a dataset with obs 8, 9, and 34 removed;
data nooutliers;
	set lab03;
	if _N_ = 8 or _N_ = 9 or _N_ = 34 then delete;						* can also include logical statement so that we delete observationson the basis of some covariate/diagnostic;
	run;
proc print data=nooutliers;
	run;

* regression without outliers;
proc reg data=nooutliers;
	model sbp = age quet smk;
	run;
