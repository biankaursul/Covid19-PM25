******************************
* BIO210 LAB 07              *
* October 13-14, 2016        *
******************************

******************************
* Connection to Contingency Tables
******************************

* Reading in the smoker.dta file
cd "your_path_here"
use "smoker.dta", clear

* Creating a contingency table for the relationship between student smoking 
* and parental smoking
cs ssmoke psmoke [fweight=freq]

* Fitting the logistic regression
logit ssmoke psmoke [fweight=freq]

******************************
* Hypothesis Testing
******************************

* Reading in the GLOW dataset
import delimited glow.csv, clear

******************************
* Binary Age

* Creating an indicator for Age > 75 years
gen age75 = age > 75

* Creating a contingency table with Age > 75 and Bone Fracture
cs fracture age75, or woolf

* Fitting simple linear regression model
logit fracture age75
estimates store binage

* Performing a likelihood ratio test for age75 coefficient
*	Note that the syntax is 'lrtest full reduced'
logit fracture
estimates store intercept
lrtest binage intercept

******************************
* Categorical Age

* Creating categorical age
*	The at() argument requires that the intervals are specified so that they
*	include the lower limit and do not include the upper limit. So the at()
*	specification below creates age intervals that go from 55 years of age up
*	to but not include 66 years of age, from 66 years of age up to but not
*	include 76 years of age, and from 76 years of age up to but not including
*	96 years of age. Note that these are the same intervals that were created
*	in R (just specified differently).
*	
*	The icodes option starts counting at 0.
egen agecat = cut(age), at(55, 66, 76, 96) icodes

* Alternative method for generating categorical age
gen agecat2 = 0 
replace agecat2 = 1 if age > 65
replace agecat2 = 2 if age > 75

* Fitting a logistic model with categorical age
logit fracture i.agecat
estimates store catage

* Fitting a logistic model with ordinal age
logit fracture agecat
estimates store ordage

* Conducting a likelihood ratio test (comparing categorical & ordinal)
lrtest catage ordage

******************************
* Continuous Age

* Fitting a logistic regression model with continuous age
logit fracture age

******************************
* Confounding
******************************

* Fitting the adjusted and unadjusted model 
logit fracture priorfrac
logit fracture priorfrac age

******************************
* Effect Modification
******************************

* Fitting a model with an interaction term
gen ageprior = age*priorfrac
logit fracture priorfrac age ageprior

* Plotting the fitted probabilities
predict fittedprob
scatter fittedprob age, title("Fitted Probabilities of Fracture")

* Plotting the log odds
gen logitprob = log(fittedprob/(1-fittedprob))
scatter logitprob age, title("Fitted Log Odds of Fracture")
