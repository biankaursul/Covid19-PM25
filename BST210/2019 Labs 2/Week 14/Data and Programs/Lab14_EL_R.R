#---------------------------------#
# BST 210: Lab Week 14            #
# 2019 						      #
#---------------------------------#

# In R there are a number of packages that you can use to calculate power and sample size
# They won't all give you the same answer (as they all implement slightly different approaches),
# and they won't necessarily agree with the answers from Stata. This is just something to be aware of.
# The packages used in the lab are just some of many that you could use on your homework/in practice.

#----------------------------------
# Linear Regression
#----------------------------------

# Note that we don't cover linear regression here but this is a great reference/resource:
# https://www.statmethods.net/stats/power.html


#----------------------------------
# Binomial Proportions
#----------------------------------

# Determining power to detect p0 = 0.6 versus p1=0.4
power.prop.test(n = 50, p1 = 0.6, p2 = 0.4, sig.level = 0.05)

# Determining the sample size needed to achieve 90% power
power.prop.test(p1 = 0.6, p2 = 0.4, sig.level = 0.05, power = .90)

# Determining power and sample size for a one-sided test
power.prop.test(n = 50, p1 = 0.6, p2 = 0.4, sig.level = 0.05, alternative = "one.sided")
power.prop.test(p1 = 0.6, p2 = 0.4, sig.level = 0.05, power = .90, alternative = "one.sided")

# We'll need a new package in order to specify an unequal allocation ratio
# Similar to HW9 1a,b
library(Hmisc)

# Determining power given a 1:2 allocation ratio and a sample size of 261
bpower(p1 = 0.4, p2 = 0.6, alpha = 0.05, n1 = 174, n2 = 87)

# What would the sample size need to be now if we want to maintain 90% power?
bsamsize(p1 = 0.6, p2 = 0.4, fraction = 1/3, alpha = 0.05, power = .90)


#----------------------------------
# Logistic Regression
#----------------------------------

# Installing one possible sample size/power package
install.packages("powerMediation")
library(powerMediation)

# Function that helps us calculate fitted probabilities more easily
expit <- function(x)
{
  return (exp(x)/(1 + exp(x)))
}

# Reading in the dataset
glow <- read.csv("~/Desktop/glow.csv")
summary(glow)
#View(glow)
head(glow)

# Determining the mean covariate values in our dataset, as well as the standard deviation for age
mean.age <- mean(glow$AGE)
mean.bmi <- mean(glow$BMI)
mean.priorfrac <- mean(glow$PRIORFRAC)
sd.age <- sd(glow$AGE)

# Fitting both the unadjusted and adjusted regression models
mod.2 <- glm(FRACTURE ~ AGE + BMI + PRIORFRAC, family = "binomial", data = glow)

# Calculating the fitted probabilities and OR associated with a one standard deviation increase
p1 <- (expit( coef(mod.2)[1] + mean.age*coef(mod.2)[2] +
               mean.bmi*coef(mod.2)[3] +
               mean.priorfrac* coef(mod.2)[4]))
p2 <- (expit( coef(mod.2)[1] + (mean.age + sd.age)*coef(mod.2)[2] +
               mean.bmi*coef(mod.2)[3] +
               mean.priorfrac* coef(mod.2)[4]))
p1; p2

# Calculating the R^2 between age, BMI, and history of prior fracture for use re: SS or power
summary(lm(AGE ~ BMI + PRIORFRAC, data=glow))$r.squared

# Determining power and sample size 
#   (Note that this is just to demonstrate how the function works. Because this function assumes that 
#   we only have a simple logistic regression model, it does not give the correct sample size
#   estimates for this case, in which we also have covariates in our model. I lay code out for covariates
#   present in the model, below.
#OR <- (p2 * (1-p1)) / (p1 * (1 - p2))
#powerLogisticCon(n = n, p1 = p1, OR = OR, alpha = 0.05)
#SSizeLogisticCon(p1 = p1, OR = OR, alpha = 0.05, power = 0.9) 

#_______________________________________________

# Next here we have the clever methods described in: 
# http://dx.doi.org/10.1002/(SICI)1097-0258(19980730)17:14<1623::AID-SIM871>3.0.CO;2-S
#and
# http://personal.health.usf.edu/ywu/logistic.pdf

## sample size for simple logistic regression
p1 <- 0.234        # the event rate at the mean of the predictor X
OR <- (.315/(1-.315))/(.234/(1-.234))   # expected odds ratio. 
#log(OR) is the change in log odds for an increase of one unit in X.
# beta*=log(OR) is the effect size to be tested

n1 <- SSizeLogisticCon(p1, OR, alpha = 0.05, power = 0.9)

## sample size for multiple logistic regression
## n_p = n_1 / (1-rho^2)
## rho^2 = R^2, for X_1 ~ X_2 + ... + X_p
r2 <- 0.134  # R^2
np <- n1 / (1-r2)

## variance inflation for the multiple logistic regression case
ssize.multi <- function(p1, OR, r2, alpha=0.05, power=0.9) 
{
    n1 <- SSizeLogisticCon(p1, OR, alpha, power)
    np <- n1 / (1-r2)
    return(np)
}

## formula (4) from above reference
## p1: as above
## p2: event rate at one SD above the mean of X
ssize.whittemore <- function (p1, p2, alpha = 0.05, power = 0.9) 
{
    beta.star <- log(p2*(1-p1)/(p1*(1-p2)))
    za <- qnorm(1 - alpha/2)
    zb <- qnorm(power)
    V0 <- 1
    Vb <- exp(-beta.star^2 / 2)
    delta <- (1+(1+beta.star^2)*exp(5*beta.star^2 / 4)) * (1+exp(-beta.star^2 / 4))^(-1)
    n <- (V0^(1/2)*za + Vb^(1/2)*zb)^2 * (1+2*p1*delta) / (p1*beta.star^2)
    n.int <- ceiling(n)
    return(n.int)
}

## multiple case
ssize.whittemore.multi <- function(p1, p2, r2, alpha=0.05, power=0.9) 
{
    n1 <- ssize.whittemore(p1, p2, alpha, power)
    np <- n1 / (1-r2)
    return(np)
}
# _____________________________________________________

### For our GLOW data:
ssize.multi(p1, OR, r2, alpha=0.05, power=0.9)				# Multivariable Logistic Utilizes R2
ssize.whittemore(p1, p2, alpha = 0.05, power = 0.9) 			# Simple Logistic
ssize.whittemore.multi(p1, p2, r2, alpha=0.05, power=0.9)		# Multivariable Logistic Utilizes R2


#----------------------------------
# Survival Analysis
#----------------------------------

# Loading the packages needed to conduct 
install.packages("powerSurvEpi")
library(foreign)
library(survival)
library(powerSurvEpi)

## Note the following key functions for SS and Power calculations using Cox model:
# powerCT.default()
# ssizeCT.default() -- This is for your Log-Rank test of 2 groups (RR option means hazard ratio)
#					   note that k=n1/n2 (such as in HW9 1b) can be used to elicit desired 
#					   proportions reflected in population, or desired in study arms

chemo <- read.dta("~/Desktop/chemotherapy.dta")

# Hazard ratio and CI for the pilot study
mod.3 <- coxph(Surv(time, relapse)~group,data=chemo)
summary(mod.3)

# Determining the power for a fixed sample size and an array of HRs
for (hr in c(0.15, 0.3, 0.4, 0.7, 0.9, 1.01, 1.1))
{
  print(paste("HR:", hr, "N:", 9))
  print(paste("Power:", 
              round(powerCT.default(nE = 9, nC = 9, pE = 1, pC = 1, RR = hr),5) ))
}

# Determining the sample size for a fixed power and an array of HRs
for (hr in c(0.15, 0.3, 0.4, 0.7, 0.9, 1.01, 1.1))
{
  print(paste("HR:", hr, "Power:", 0.8))
  print(round(as.numeric(ssizeCT.default(power = 0.8, k = 1, pE = 1, pC = 1, RR = hr, 
                              alpha = 0.05),5)))
}

# Note:
#   The method for determining the sample size for a log-rank test (comparing two
#   groups) is the same as the sample size for a Cox proportional hazards model with a single
#   binary covariate. If we want to fit an adjusted Cox proportional hazards model with a binary
#   or continuous covariate of interest, we can use the functions ssizeEpi/powerEpi (for 
#   binary covariates) and ssizeEpiCont/powerEpiCont (for continuous covariates) to get 
#   sample sizes and power estimates.

# Suppose we want to fit a Cox PH for our event of interest (recorded in the event indicator "event")
#   and that we want to be able to detect an HR of "HR" associated with our covariate of interest
#   "cov.int" with 80% power. We also want to adjust for a potential confounder "X2" in our model.
#   If all of the data is stored in a dataset called "dat", and if we want to conduct a 0.05-level
#   test, the following code calculates the sample size; powerEpiCont is similar.
# 
#   ssizeEpiCont(formula = cov.int ~ X2, dat = dat, X1 = cov.int, failureFlag = event,
#                power = 0.8, theta = HR, alpha = 0.05)
# 
#   You can also use ssizeEpiCont.default() to do the calculation. It takes different inputs (see
#   documentation) but the inputs it takes seem more similar to the functions in Stata.

# See documentation for information on how to implement ssizeEpi(), ssizeEpi.default(), powerEpi() 
#   and powerEpi.default()