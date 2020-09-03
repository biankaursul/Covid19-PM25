
import delimited "\\tsclient\Desktop\BST210\lab_test\Week 12\srt.csv"

*Summarize by treatment group
sort sorb
by sorb: su	

*Set up survival analysis in stata
stset fup, id(id) failure(status)

*Part 1: K-M estimate
*Compute K-M estimates
sts list if sex==1, by(sorb sex)

*Plot Kaplan-Meier estimate
sts graph if sex==1, by(sorb sex)

*test two K-M curves using Log-Rank test
sts test if sex==1 sorb

*Stratified Log-Rank Test
sts test sorb, strata(sex)

*Part 2: Parametric survival analysis
*Fit exponential survival regression model
streg i.sorb i.sex tgh dur, distribution(exponential)
*Plot survival function for sorbinil and placebo group separately
stcurve, survival at1(sorb=0, sex=1) at2(sor=1, sex=1)
*Plot hazard function for sorbinil and placebo group separately
stcurve, hazard at1(sorb=0, sex=1) at2(sor=1, sex=1)
*Plot hazard function for sorbinil and placebo group separately (log scale)
stcurve, hazard at1(sorb=0, sex=1) at2(sor=1, sex=1) yscale(log)

*Fit Weibull survival regression model and plot survival curves
streg i.sorb i.sex tgh dur, distribution(weibull)
*Plot survival function for sorbinil and placebo group separately
stcurve, survival at1(sorb=0, sex=1) at2(sor=1, sex=1)
*Plot hazard function for sorbinil and placebo group separately
stcurve, hazard at1(sorb=0, sex=1) at2(sor=1, sex=1)
*Plot hazard function for sorbinil and placebo group separately (log scale)
stcurve, hazard at1(sorb=0, sex=1) at2(sor=1, sex=1) yscale(log)

*Part 3: Cox Proportional Hazards Models

*Full Cox Model
stcox i.sorb i.sex tgh dur
estimates store fullmodel

*Plot survival function for sorbinil and placebo group separately
stcurve, survival at1(sorb=0, sex=1) at2(sor=1, sex=1)
*Plot hazard function for sorbinil and placebo group separately
stcurve, hazard at1(sorb=0, sex=1) at2(sor=1, sex=1)
*Plot hazard function for sorbinil and placebo group separately (log scale)
stcurve, hazard at1(sorb=0, sex=1) at2(sor=1, sex=1) yscale(log)
