******************************
* BIO210 LAB 11              *
* November 8-9, 2017         *
******************************

******************************
* Poisson Regression
******************************

* Reading in the dataset
import delimited melanoma.csv
generate north = latitude == "Northern"
generate middle = latitude == "Middle"

* Fitting a Poisson regression model for the relationship between latitude
* and melanoma incidence
poisson inccases north middle, exposure(persyrs)

* Is the association statistically significant?
test north middle

* Testing significance of and finding a confidence interval for Northern 
* versus Middle latitudes
generate southern = latitude == "Southern"
poisson inccases north southern, exposure(persyrs) irr

* Alternative method for finding/testing the IRR for Northern versus Middle
poisson inccases north middle, exposure(persyrs)
lincom north-middle

* Adding in the main effect of age category;
generate under35 = ageg == "<35_years"
generate age35to44 = ageg == "35-44_years"
generate age45to54 = ageg == "45-54_years"
generate age55to64 = ageg == "55-64_years"
generate age65to74 = ageg == "65-74_years"
generate over75 = ageg == ">=75_years"
poisson inccases north middle under35 age35to44 age45to54 age55to64 age65to74, exposure(persyrs)

* Is age an independent predictor of melanoma?
test under35 age35to44 age45to54 age55to64 age65to74

* Alternative method for determining if age is an independent predictor
estimates store agelat
poisson inccases north middle, exposure(persyrs)
lrtest agelat

* Adding in an interaction term between age and latitude;
generate midunder35 = ageg=="<35_years" & latitude=="Middle"
generate mid35to44 = ageg=="35-44_years" & latitude=="Middle"
generate mid45to54 = ageg=="45-54_years" & latitude=="Middle"
generate mid55to64 = ageg=="55-64_years" & latitude=="Middle"
generate mid65to74 = ageg=="65-74_years" & latitude=="Middle"
generate northunder35 = ageg=="<35_years" & latitude=="Northern"
generate north35to44 = ageg=="35-44_years" & latitude=="Northern"
generate north45to54 = ageg=="45-54_years" & latitude=="Northern"
generate north55to64 = ageg=="55-64_years" & latitude=="Northern"
generate north65to74 = ageg=="65-74_years" & latitude=="Northern"
poisson inccases north middle under35 age35to44 age45to54 age55to64 age65to74 midunder35 mid35to44 mid45to54 mid55to64 mid65to74 northunder35 north35to44 north45to54 north55to64 north65to74, exposure(persyrs)

* Testing for effect modification
estimates store interact
lrtest interact agelat

******************************
* Overdispersion
******************************

* Finding Goodness-of-Fit statistics
poisson inccases north middle, exposure(persyrs)
estat gof

* Negative Binomial Regression
nbreg inccases north middle, exposure(persyrs)

* Robust Variance Estimation
poisson inccases north middle, exposure(persyrs) vce(robust)

******************************
* Zero-Inflated Poisson
******************************

* Reading in the dataset
clear all
import delimited school_data.csv

* Creating a histogram of student absences
histogram daysabs, discrete frequency

* Fitting a ZIP with constant structural zero probability
zip daysabs mathnce langnce female, inflate(_cons) irr vuong

* Fitting a ZIP with structural zero probability dependent on mathnce
zip daysabs mathnce langnce female, inflate(mathnce langnce) irr vuong
