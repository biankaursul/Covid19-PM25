**********************
* BST 210 Week 9 Lab *
**********************
* Input data
clear all
use "data_lab9.dta" 

*Multinomial logistic regression
mlogit outcome i.treatment [fweight = count], baseoutcome(1)
predict fitm1 fitm2 fitm3
list outcome treatment fitm1 fitm2 fitm3
mlogit outcome i.treatment [fweight = count], baseoutcome(1) rrr
mlogit outcome i.treatment [fweight = count]
*If we don't specifiy the reference group, Stata automatically uses
*the most frequent outcome as the reference group.
estat ic

*Ordinal logistic regression
ologit outcome i.treatment [fweight = count]
* Note that Stata uses P(Y <= j)/P(Y > j), etc., rather than
* P(Y >= j)/P(Y < j) as used in the lecture notes in class.
* However, since they fit the model P(Y <= j)/P(Y > j) = aj - (b1X1+...+bpXp),
* You will get the same interpretation for b1,...,bp as for the betas
* in the lecture notes if you compare higher to lower levels (basically P(Y >= j)/P(Y < j)),
* yet aj will be the negative to alpha_j as in lecture notes.
* (in Stata, the aj's are defined as "cut points")
predict fito1 fito2 fito3
list outcome treatment fito1 fito2 fito3
ologit outcome i.treatment [fweight = count], or
estat ic
gen r1 = (outcome > 1)
gen r2 = (outcome > 2)
logistic r1 i.treatment [fweight = count] 
logistic r2 i.treatment [fweight = count]
*use "ssc install gologit2" if you don't have this package
*In general, you can search for packages using "findit" (e.g., "findit gologit2")
gologit2 outcome i.treatment [fweight = count], rrr
predict fitgo1 fitgo2 fitgo3
list outcome treatment fitgo1 fitgo2 fitgo3
estat ic
gen spectlow = (treatment == 2)
gen specthigh = (treatment == 3)
omodel logit outcome spectlow specthigh [fweight = count]
*use "ssc install omodel" if you don't have this package
