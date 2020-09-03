#---------------------------------#
# BST 210: Lab Week 5             #
# September 28-29, 20             #
#---------------------------------#

# Installing Necessary Packages
library(foreign)
library(MASS)
library(leaps)

# Reading in the Data
framingham <- read.dta(file="framingham.dta")

# Dropping Missing Observations
framingham <- framingham[complete.cases(framingham), ]

#----------------------------------
# Model Building
#----------------------------------

# Fitting the Full Regression Model
full.mod <- lm(totchol ~ sex + age + sysbp + diabp + cursmoke + cigpday + bmi + diabetes + prevhyp + prevchd, data=framingham)
summary(full.mod)

# Forward Selection Procedure (using AIC)
#   Base R does not appear to have any functions that perform automated model building procedures, and 
#   all of the functions within other packages (such as the step() function, in the MASS package) 
#   seem to operate on some criterion other than p-value. Here, the criterion is based on the AIC
mod_forw <- step(lm(totchol ~ 1, data = framingham), ~sex + age + sysbp + diabp + cursmoke + cigpday + bmi + diabetes + prevhyp + prevchd, direction = "forward")
summary(mod_forw)

# Backward Selection Procedure (using AIC)
mod_back <- step(full.mod, direction = "backward")
summary(mod_back)

# Stepwise Selection Procedure (using AIC)
mod_step <- step(lm(totchol ~ 1, data = framingham), ~sex + age + sysbp + diabp + cursmoke + cigpday + bmi + diabetes + prevhyp + prevchd, direction = "both")
summary(mod_step)

# Best Subsets Procedure (usind Adjusted R^2)
#   I realize that the code below for the best subsets procedure is a little complicated/hard to read.
#   Essentially, the leaps() function allows us to perform best subsets selection, though of all the metrics
#   we saw in class, leaps() only computes the R^2 or the Adjusted R^2. 
#   These first two lines actually perform the best subset selection:
covariates <- c("sex", "age", "sysbp", "diabp", "cursmoke", "cigpday", "bmi", "diabetes", "prevhyp", "prevchd")
best_sub <- leaps(x=framingham[, covariates], y=framingham$totchol, method="adjr2")
#   These next two lines help us pick out the covariates that are in the "best" model:
best_sub_index <- which(best_sub$adjr2==max(best_sub$adjr2))
best_sub_cov <- covariates[best_sub$which[best_sub_index, ]]
#   This next line just fits a linear regression model with our selected variables
best_sub_mod <- lm(formula(paste("totchol ~", paste(best_sub_cov, collapse = "+"))), data=framingham)
summary(best_sub_mod)

# Stepwise Selection Procedure (using AIC, and with cursmoke in the model)
null_cursmoke <- lm(totchol ~ cursmoke, data=framingham)
mod_step_cursmoke <- step(lm(totchol ~ cursmoke, data = framingham), ~sex + age + sysbp + diabp + cursmoke + cigpday + bmi + diabetes + prevhyp + prevchd, direction = "both", scope=list(lower=null_cursmoke, upper=full.mod))
summary(mod_step_cursmoke)

# Running Stepwise Selection with an Interaction between cursmoke and BMI
#   Note that in this case, we actually end up with a model that doesn't include the interaction
#   term and that is hierarchically well-formulated. That is not, however, guaranteed to happen.
mod_step <- step(lm(totchol ~ 1, data = framingham), ~sex + age + sysbp + diabp + cursmoke + cigpday + bmi + diabetes + prevhyp + prevchd + cursmoke*bmi, direction = "both")
summary(mod_step)

#----------------------------------
# Additional Code: Bootstrapping
#----------------------------------

# I was unable to find a function in R that bootstrapped the model selection procedure
# and reported the frequency with which each covariate was included, as we were able
# to do in SAS and Stata. This isn't to say that some function doesn't exist out there
# somewhere; I just couldn't find it.

# I wrote my own function so that, if you want to use bootstrapping in the group 
# projects to validate your final model choice, you may. Since I wrote this myself,
# it runs a little slowly, but will give you the same output that you see in Stata 
# and SAS.

# This loads in the code for the function
source("LinearReg_Bootstrap.R")

# Model Selection Bootstrapping
covariates <- c("sex", "age", "sysbp", "diabp", "cursmoke", "cigpday", "bmi", "diabetes", "prevhyp", "prevchd")
outcome <- "totchol"
selection <- "stepwise"
run_boot(covariates, outcome, framingham, selection)
