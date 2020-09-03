clear all
*************************************
** Conditional Logistic Regression **
*************************************
clear all 

infile id crpq sbp dm age smok chdlq caco using "C:/Users/Jeremy/Dropbox/Harvard/G3 Fall/BST 210/Labs/Lab Week 10/whsmtch.dat"
gen hisbp=sbp>=140 if sbp<.

* Question 1
clogit caco hisbp, strata(id) or

* Question 2
gen sbpcat=recode(sbp, 119, 139, 159, 175)
recode sbpcat 119 = 1
recode sbpcat 139 = 2
recode sbpcat 159 = 3
recode sbpcat 175 = 4
ta sbpcat sbp

clogit caco i.sbpcat, strata(id) or
estimates store catsbp

clogit caco sbpcat, strata(id) or
estimates store ordsbp

lrtest ordsbp catsbp