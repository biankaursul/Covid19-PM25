******************************
* BIO210 LAB 06              *
* October 10-11, 2019        *
******************************

* Reading in the Framingham Dataset
cd "your_path_name"
use "framingham.dta", clear

* Removing all missing observations
foreach v of var * {
 drop if mi(`v')
}

******************************
* Testing Hypotheses
******************************

* Fitting regression model (1)
regress totchol sex age diabp cigpday bmi

* Storing residuals and fitted values (for later)
predict lmresid, residuals
predict lmfit

* Performing an F-test (testing all betas equal to 0)
test sex age diabp cigpday bmi

* Performing an F-test (testing beta_bmi equal to 0)
test bmi

* Making a cubic spline of bmi with 4 knots
mkspline bmis=bmi, cubic nknots(5)

* Fitting the full model (including spline term)
regress totchol age sex diabp cigpday bmi bmis1 bmis2 bmis3 bmis4

* Performing an F-test (testing whether the slopes of the spline terms are
* equal to 0)
test bmis1 bmis2 bmis3 bmis4

******************************
* Robust Variances
******************************

* Creating residual plots for Model (1): qqplot and Lowess
qnorm lmresid
lowess lmresid lmfit

* Running Model (1) again, this time with a robust variance estimator
regress totchol age sex diabp cigpday bmi, vce(robust)

******************************
* Introduction to Logistic Regression
******************************

* Reading in the smoker.dta file
use "smoker.dta", clear

* Creating a contingency table for the relationship between student smoking
* and parental smoking
cs ssmoke psmoke [fweight=freq]

* Fitting the logistic regression
logit ssmoke psmoke [fweight=freq]
