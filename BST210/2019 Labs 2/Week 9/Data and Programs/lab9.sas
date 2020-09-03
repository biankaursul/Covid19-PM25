
/******************************************************************************/
/* BST 210 Lab 9
/******************************************************************************/
/* Importing the csv file 'lab9.csv' */
proc import file='\\tsclient\BST 210\Labs (Fall 2017)\Week 9\lab9.csv' out=dat dbms=csv replace;
	getnames=yes;
run;

* Multinomial regression;
proc logistic data = dat;
class outcome (ref = "1") treatment (ref = "Penicillin") / param = ref;
model outcome = treatment / link = glogit;
output out = fittedprobs p = prob;
run;

* Table Proportions - columnwise ;
proc freq data = dat;
tables outcome*treatment / norow nopercent;
run;

* Ordinal Logistic Regression - Proportional Odds Assumption;
* NOTE: This gives us the negative of the coefficients we got in Stata, but same "intercepts"
* Because SAS fits a model P(Y<=1)/P(Y>1) = aj + b1X1 + ... + bpXp,
* while Stata fits a model P(Y<=1)/P(Y>1) = aj - (b1X1 + ... + bpXp)
* Again, be aware of how your package specifies the ordinal model!;
proc logistic data = dat;
class outcome(ref = "1") treatment(ref = "Penicillin") / param=reference;
model outcome = treatment;
run;
