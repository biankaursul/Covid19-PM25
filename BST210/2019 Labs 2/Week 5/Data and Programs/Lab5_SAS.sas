/******************************************************************************/
/* BST 210 Lab 5
/* September 28-29, 2017
/******************************************************************************/

/* Importing the csv file 'framingham.dta' */
proc import datafile='framingham.dta' out=framingham dbms=dta replace;
run;

/* Keeping only those observations with complete data */
data framingham;
	set framingham;
	if cmiss(of _all_) then delete;
run;

/* Fitting the Full Model */
proc reg data=framingham;
	model totchol =  sex age sysbp diabp cursmoke cigpday bmi diabetes prevhyp prevchd;
run;

/* Forward Selection */
proc reg data=framingham;
	model totchol =  sex age sysbp diabp cursmoke cigpday bmi diabetes prevhyp prevchd / slentry=0.15
	selection=forward ss2 sse aic;
	*output out=out1 p=p r=r; 
run; 

/* Backward Selection */
proc reg data=framingham;
	model totchol =  sex age sysbp diabp cursmoke cigpday bmi diabetes prevhyp prevchd / slstay=0.15 
	selection=backward ss2 sse aic;
run;

/* Stepwise Selection */
proc reg data=framingham;
	model totchol =  sex age sysbp diabp cursmoke cigpday bmi diabetes prevhyp prevchd / slstay=0.15 slentry=0.15
	selection=stepwise ss2 sse aic;
run;

/* Performing Best Subsets Selection */
proc reg data=framingham outest=estout;
	model totchol =  sex age sysbp diabp cursmoke cigpday bmi diabetes prevhyp prevchd / 
	selection=adjrsq sse aic bic adjrsq;
run;
* Sorting and printing out the top 8 models by AIC;
proc sort data=estout; 
	by _aic_;
proc print data=estout(obs=8); 
run;
* Sorting and printing out the top 8 models by BIC;
proc sort data=estout; 
	by _bic_;
proc print data=estout(obs=8); 
run;
* Sorting and printing out the top 8 models by Adjusted R^2;
proc sort data=estout; 
	by descending _adjrsq_;
proc print data=estout(obs=8); 
run;
* Sorting and printing out the top 8 models by Root MSE;
proc sort data=estout; 
	by _rmse_;
proc print data=estout(obs=8); 
run;

/* Performing Stepwise Selection with cursmoke in the Model */
proc reg data=framingham;
	model totchol =  cursmoke sex age sysbp diabp cigpday bmi diabetes prevhyp prevchd / slstay=0.15 slentry=0.15
	selection=stepwise ss2 sse aic
	include=1;
run; 

/* Considering the Interaction between cusmoke and BMI */
data framingham;
	set framingham;
	cbmi = cursmoke*bmi;
run;
proc reg data=framingham;
	model totchol =  cursmoke sex age sysbp diabp cigpday bmi diabetes prevhyp prevchd cbmi/ slstay=0.15 slentry=0.15
	selection=stepwise ss2 sse aic;
run; 

/**************************************************/
/* Additional, Supplemental Code: Bootstrapping   */
/**************************************************/

/* Bootstrapping the Stepwise Selection Procedure */
proc glmselect data=framingham plots=all;
	model totchol =  cursmoke sex age sysbp diabp cigpday bmi diabetes prevhyp prevchd / slstay=0.15 slentry=0.15
	select=sl;
	modelAverage nsamples=1000 subset(best=1);
run;

/* Performing Stepwise Selection on the Basis of AIC (works for other metrics as well) */
proc glmselect data=framingham plots=all;
	model totchol =  cursmoke sex age sysbp diabp cigpday bmi diabetes prevhyp prevchd / slstay=0.15 slentry=0.15
	select=aic;
run;
