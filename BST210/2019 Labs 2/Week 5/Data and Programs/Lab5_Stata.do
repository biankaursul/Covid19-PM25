****************************
* BST 210 LAB 5			   *
* September 28-29, 2017	   *
****************************

* Setting the working directory and loading data
clear all
cd "\BST 210\Labs\Lab Week 5"
use "framingham.dta", clear

* Dropping missing obserations - one variable at a time
foreach v of var * {
 drop if mi(`v')
}

****************************
* MODEL BUILDING   		   *
****************************

* Fitting the Full Model
regress totchol sex age sysbp diabp cursmoke cigpday bmi diabetes prevhyp prevchd

* NOTE: if we want the AIC or BIC of a particular model fit, we cn use the command:
estat ic

* Fitting Forward Selection
* 	pe(.15) - pvalue for entry < 0.15
stepwise, pe(.15): regress totchol sex age sysbp diabp cursmoke cigpday bmi diabetes prevhyp prevchd
estimates store forward

* Fitting Backward Elimination
* 	pr(.15) - pvalue for removal > 0.15
stepwise, pr(.15): regress totchol sex age sysbp diabp cursmoke cigpday bmi diabetes prevhyp prevchd
estimates store backward

* Fitting Forward Stepwise Selection
* 	pe(.10) - pvalue for entry < 0.10
* 	pr(.15) - pvalue for removal > 0.15
* 	NOTE: Stata forces pr > pe
stepwise, pr(.15) pe(.10) forward: regress totchol sex age sysbp diabp cursmoke cigpday bmi diabetes prevhyp prevchd
estimates store forward_step

* Fitting Backward Stepwise Selection
* 	pe(.10) - pvalue for entry < 0.10
*  	pr(.15) - pvalue for removal > 0.15
*  	NOTE: Stata forces pr > pe
stepwise, pr(.15) pe(.10): regress totchol sex age sysbp diabp cursmoke cigpday bmi diabetes prevhyp prevchd
estimates store backward_step

* Performing Best Subset Selection
* 	Note that this package will report the 'optimal' model(s) for each number of
* 	covariates (the best model(s) including only one covariate, the best model(s)
* 	including only two covariates, etc.); we can control the number of optimal 
* 	models reported via the nmodels(#) command. It reports both the AIC and BIC
*   for all models, from which you can then choose the best across all numbers of
* 	covariates. You will need to re-run the "reg totchol ______" command to 
*   actually get parameter estimates for the final model.
ssc install gvselect
gvselect <term> cursmoke sex age sysbp diabp cigpday bmi diabetes prevhyp prevchd, nmodels(2): reg totchol <term>

* Fitting Forward Stepwise Selection with cursmoke in the Model
stepwise, pr(.15) pe(.10) forward lockterm1: regress totchol cursmoke sex age sysbp diabp cigpday bmi diabetes prevhyp prevchd
estimates store forward_step_cursmoke

* Fitting Backward Selection with an Interaction Term
* 	Note that both cursmoke and the interaction term end up in the model, but
* 	the main effect of BMI does not! 
gen cbmi=cursmoke*bmi
stepwise, pr(.15): regress totchol sex age sysbp diabp cursmoke cigpday bmi diabetes prevhyp prevchd cbmi


***********************************
* ADDITIONAL CODE: BOOTSTRAPPING  *
***********************************

* Bootstrapping the Forward Stepwise Selection Procedure
ssc install swboot
swboot totchol cursmoke sex age sysbp diabp cigpday bmi diabetes prevhyp prevchd, reps(100) pr(.15) pe(.10) forward
