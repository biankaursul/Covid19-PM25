****************************
* BST 210 LAB 2			   *
* September 7-8, 2017	   *
****************************

* Set working directory (make sure to specify the filepath on your computer!) and load data
clear all
cd "\BST 210\Labs\Lab Week 2" 
use "lab2.dta", clear

****************************
* SIMPLE LINEAR REGRESSION *
****************************

* Creating a scatterplot for the relationship between SBP and quet
twoway (scatter sbp quet), title(Relationship between SBP and quet)

* Running simple linear regression
regress sbp quet

* Regression diagnostics
*    We can use the predict function to find the residuals and fitted values
*    for our model, and then plot them to assess our assumptions
predict resids, residuals
predict yhat
twoway (scatter resids yhat), title(Residuals vs. Fitted Values)
* add 0 line for reference
twoway (scatter resids yhat), title(Residuals vs. Fitted Values) yline(0)
* assess normality
qnorm resids
histogram resids, bin(5)

* Note that model fit statistics, hypothesis test results, and confidence
* intervals for the regression coefficients are all reported as part of 
* the output from a "regress" call

******************************
* MULTIPLE LINEAR REGRESSION *
******************************

* Is age a confounder of the relationship between quet and sbp?
regress sbp quet age

* Is smk an effect modifier of the relationship between quet and sbp?
generate intx = quet * smk
regress sbp quet smk intx

* Plotting the fitted regression lines
*    We first predict the fitted values for all observations in our
*	 dataset, and then separate out the fitted values by smoking status.
*	 To create the final plot, we overlay these fitted values onto a 
*	 scatterplot of SBP and quetelet index, and then connect the points
*	 by a line
predict intyhat
gen intyhat_smk = intyhat if smk == 1
gen intyhat_nosmk = intyhat if smk == 0
scatter sbp intyhat_smk intyhat_nosmk quet, symbol(O i i i) connect(. l l) lcolor(black blue red) xtitle("Quetelet Index") ytitle("Systolic Blood Pressure (mmHG)") legend(order(2 "Smokers" 3 "Non-smokers" ))

****************************************************************
* ADDITIONAL TOPICS: CONFIDENCE INTERVALS FOR PREDICTED VALUES *
****************************************************************

* Re-running the simple linear regression
*	Whenever we use commands like "predict", Stata uses the most recently
*	run regression model to make these predictions
regress sbp quet

* Plotting regression line, confidence bands, prediction bands
* get our standard errors for predicting mean value as well as new observations
predict se_mean, stdp
predict se_ynew, stdf

* generate confidence bands around mean values
generate up_mean = yhat + 1.96 * se_mean
generate lo_mean = yhat - 1.96 * se_mean

* generate confidence bands around new predicted observations
generate up_pred = yhat + 1.96 * se_ynew
generate lo_pred = yhat - 1.96 * se_ynew

* sort by quet to graph correctly
sort quet
scatter sbp yhat up_mean lo_mean up_pred lo_pred quet, symbol(o i i i i i) connect(. l l l l l) lcolor(black red blue blue green green)

