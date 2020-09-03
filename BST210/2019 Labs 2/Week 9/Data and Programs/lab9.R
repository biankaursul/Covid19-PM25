# Set working directory and load in data

library(foreign)
library(nnet)
install.packages("splitstackshape")
library(splitstackshape)
dat = read.dta("data_lab9_v12.dta")
dat = expandRows(dat, "count") #this expands the counts into one row per observation

# Fit multinomial model
mod.1 = multinom(outcome ~ treatment, data = dat)
summary(mod.1)

# A function that will help us summarize multinomial models 
# Code credit: BIO233 - Methods II
summ.MNfit <- function(fit, digits=3){
  s <- summary(fit)
  for(i in 2:length(fit$lev))
  {
    ##
    cat("\nLevel", fit$lev[i], "vs. Level", fit$lev[1], "\n")
    ##
    betaHat <- s$coefficients[(i-1),]
    se <- s$standard.errors[(i-1),]
    zStat <- betaHat / se
    pval <- 2 * pnorm(abs(zStat), lower.tail=FALSE)
    ##
    RRR <- exp(betaHat)
    RRR.lo <- exp(betaHat - qnorm(0.975)*se)
    RRR.up <- exp(betaHat + qnorm(0.975)*se)
    ##
    results <- cbind(betaHat, se, pval, RRR, RRR.lo, RRR.up)
    print(round(results, digits=digits))
  }
}

summ.MNfit(mod.1)

# Look at how fitted values compare to observed proportions
table(dat$outcome, dat$treatment)
prop.table(table(dat$outcome, dat$treatment), margin = 2)
# Penicillin fitted probs.
fitted(mod.1)[dat$treatment == "Penicillin",][1,]
# Spect-Low fitted probs.
fitted(mod.1)[dat$treatment == "Spect-Low",][1,]
# Spect-High fitted probs.
fitted(mod.1)[dat$treatment == "Spect-Hight",][1,]

# Note, by default, R uses lowest outcome as baseline

######################
# Ordinal Regression #
######################
library(VGAM)
# NOTE:
# We will get the negative of the coefficients and same "intercept" as in Stata
# if we set reverse = FALSE(default), because when reverse = FALSE, 
# the model is P(Y<=j)/P(Y>j) = aj + b1X1 + ... + bpXp;
# when reverse = TRUE, the model is P(Y>=j)/P(Y<j) = aj + b1X1 + ... + bpXp
# Stata fits the model P(Y<=j)/P(Y>j) = aj - (b1X1 + ... + bpXp) 

mod.2 = vglm(outcome ~ treatment,
             cumulative(parallel=TRUE, reverse=FALSE), data=dat)
summary(mod.2)

mod.2.2 = vglm(outcome ~ treatment,
     cumulative(parallel=TRUE, reverse=TRUE), data=dat)
summary(mod.2.2)

# Look at how fitted values compare to observed proportions
table(dat$outcome, dat$treatment)
prop.table(table(dat$outcome, dat$treatment), margin = 2)
# Penicillin fitted probs.
fitted(mod.2)[dat$treatment == "Penicillin",][1,]
# Spect-Low fitted probs.
fitted(mod.2)[dat$treatment == "Spect-Low",][1,]
# Spect-High fitted probs.
fitted(mod.2)[dat$treatment == "Spect-Hight",][1,]

#############################
# Using a different package #
#############################
library(MASS)
mod.2b = polr(as.factor(outcome) ~ treatment, data = dat)
summary(mod.2b)

###################################
# Test of Proportionality of Odds #
###################################
# These are slighly different than the Stata tests
# Here you are using the vglm function to fit two models
# One with the proportional odds assumptions (fit.po) 
# and one without (fit.npo) [change parallel to F]
# We then compare them with a "likelihood ratio" test
# Should give roughly the same conclusion as Stata

# Testing proportional odds
fit.po = vglm(outcome ~ treatment,
              cumulative(parallel=TRUE, reverse=T), data=dat)
fit.npo = vglm(outcome ~ treatment,
               cumulative(parallel=FALSE, reverse=T), data=dat)
pchisq(deviance(fit.po)-deviance(fit.npo),
       df=df.residual(fit.po)-df.residual(fit.npo),lower.tail=F)
