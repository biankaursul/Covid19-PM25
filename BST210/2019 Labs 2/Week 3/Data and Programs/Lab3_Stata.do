****************************
*   BST210 LAB 03          *
*   September 19-20, 2019  *
****************************

use "lab03_outliers.dta"

* scatter plot, systolic blood pressure (sbp) versus individual covariates
scatter sbp quet, by(smk)
scatter sbp age, by(smk)

* regress sbp on covariates
regress sbp smk quet age

* generate a new variable named `res', that contains the residuals
predict res, residuals
label variable res "Residuals"

* generate a new variable named `sbp_hat', that contains the predicted means
predict sbp_hat, xb
label variable sbp_hat "predicted (mean) outcome"

* generate diagnostic plots using the residuals
histogram res
graph box res
scatter res sbp_hat

* generate a new variable named `inStu', that contains internal studentized residuals
predict inStu, rstandard
label variable inStu "Internally Studentized"
* generate a new variable named `exStu', that contains externally studentized residuals
predict exStu, rstudent
label variable exStu "Externally Studentized"
* generate a new variable named `stdRes' that standardizes the residuals
gen stdRes = res / e(rmse)
label variable stdRes "Standardized"

* create Normal Q-Q plots of the residuals
qnorm res, saving(qqres, replace)
qnorm inStu, saving (qqinStu, replace)
qnorm exStu, saving(qqexStu, replace)
qnorm stdRes, saving (qqstd, replace)

* combine into one plot
gr combine qqres.gph qqinStu.gph qqexStu.gph qqstd.gph, xsize(20) ysize(20)

* generate a new variable named `h', that contains leverage values
predict h, leverage
* generate a new variable named 'cook', that contains cook's d values
predict cook, cooksd
* generate a new variable named `dfit', that contains dfitscvalues
predict dfit, dfits

*list above threshold data points for leverage
clist person h sbp age smk quet if h> (2*(3+1))/34, noobs

*list above threshold data points for cook's d
clist person cook sbp age smk quet if cook>4/(34-2), noobs

*list above threshold data points for dffits
clist person dfit sbp age smk quet if abs(dfit)>2*sqrt((3+1)/34), noobs

*remove observations 8, 9, 34 from the data set
drop if person==8 | person==9 | person==34

*regress again
regress sbp smk quet age
