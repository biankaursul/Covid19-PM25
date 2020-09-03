********************************
* BIO210 LAB 14                *
* November 30-December 1, 2017 *         
********************************

********************************
* Binomial Proportions
********************************

* Determining power to detect p0 = 0.6 versus p1=0.4
power twoproportions 0.6 0.4, n1(50) n2(50)

* Determining the sample size needed to achieve 90% power
power twoproportions 0.6 0.4, power(0.9)

* Determining power and sample size for a one-sided test
power twoproportions 0.6 0.4, n1(50) n2(50) onesided
power twoproportions 0.6 0.4, power(0.9) onesided

* Sample size inflation needed to handle drop-out
display 130/(1-0.2)

* Examining power given a 1:2 allocation ratio and a sample size of 261
power twoproportions 0.6 0.4, n1(87) n2(174)

* Sample size needed to achieve 90% power given a 1:2 allocation ratio
power twoproportions 0.6 0.4, nratio(2) power(0.9)

********************************
* Logistic Regression
********************************

* Installing the powerlog package
* 	Uncomment the following command and click through the help documentation
* 	until you find a hyperlinked command allowing you to install the powerlog
* 	package (note that it may already be installed in your version of Stata)
* findit powerlog

* Reading in the GLOW dataset
import delimited glow.csv, clear

* Determining the mean covariate values in our dataset, as well as the 
* 	standard deviations for age
summarize age
gen mean_age = r(mean)
gen sd_age = r(sd)
summarize bmi
gen mean_bmi = r(mean)
summarize priorfrac
gen mean_prior = r(mean)

* Calculating the fitted probabilities of interest, corresponding to the fitted
* 	probability of a fracture for a participant with mean values of all of the 
* 	coefficients--BMI, prior fracture, and age--and for a participant with 
* 	mean values of BMI and prior fracture, but a one standard deviation higher
*	age (i.e., age = mean_age + sd_age). The fitted probabilities from the
* 	GLOW study give us reasonable alternative effect estimates for which to
* 	power GLOOM. We also calculate the R^2 for the linear regression of age on
* 	BMI and prior fracture history.
*
* 	Note that we could alternatively calculate the fitted probabilities by 
*	adding two new rows to our data set (one with all covariates set to the
*	mean values, and one with age set to mean age + one standard deviation)
* 	and then using the "predict" command
logit fracture age
logit fracture age bmi priorfrac
display exp(-5.11+.045*mean_age+0.023*mean_bmi+0.818*mean_prior)/ ///
	(1+exp(-5.11+.045*mean_age+0.023*mean_bmi+0.818*mean_prior))
display exp(-5.11+.045*(mean_age+sd_age)+0.023*mean_bmi+0.818*mean_prior)/ ///
	(1+exp(-5.11+.045*(mean_age+sd_age)+0.023*mean_bmi+0.818*mean_prior))
regress age bmi priorfrac

* Determining power and sample size for GLOOM
powerlog, p1(0.234) p2(0.315) rsq(0.134)

* Examining how the results change for different R^2 values
* 	R^2 = 0
powerlog, p1(0.234) p2(0.315)
* 	R^2 = 0.3
powerlog, p1(0.234) p2(0.315) rsq(0.3)
* 	R^2 = 0.5
powerlog, p1(0.234) p2(0.315) rsq(0.5)
*	R^2 = 0.7
powerlog, p1(0.234) p2(0.314) rsq(0.7)
* 	R^2 = 0.9
powerlog, p1(0.234) p2(0.314) rsq(0.9)

********************************
* Survival Analysis
********************************

* Reading in the chemotherapy dataset
use "chemotherapy.dta", clear

* Hazard ratio and CI for the pilot study
stset time, failure(relapse)
stcox group

* Determining power for a fixed sample size (18 events) and an array of HRs
stpower logrank, hratio(0.15 0.3 0.4 0.7 0.9 1.01 1.1) n(18) alpha(0.05)

* Determining sample size for a fixed power (80%) and an array of HRs
stpower logrank, hratio(0.15 0.3 0.4 0.7 0.9 1.01 1.1) power(0.8) alpha(0.05)

* Impact of a 20% censoring rate on our sample size calculations
stpower logrank, hratio(0.15 0.3 0.4 0.7 0.9 1.01 1.1) power(0.8) alpha(0.05) wdprob(0.2)

* Performing calculations for a Cox PH model with a continuous covariate
* 	Suppose we also had information on some continuous covariate, say age
* 	Then we could run the following command to get power or sample size
* 	calculations for our Cox proportional hazards model with that single 
* 	covariate, assuming that we want to detect a log(HR) of +1 (positive one),
* 	and assuming the standard deviation of our continuous covariate is 0.5
* stpower cox +1, sd(0.5)


