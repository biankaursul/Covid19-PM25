#------------------------#
#   BST210 LAB 06        #
# October 10-11, 2019    #
#------------------------#

# Loading packages
library(foreign)
library(rms)
library(sandwich)

# Reading in the Framingham data
framingham <- read.dta(file="framingham.dta")
framingham <- framingham[complete.cases(framingham), ]

#-------------------------
# Testing Hypotheses
#-------------------------

# Fitting regression model 1
lm.1 <- lm(totchol ~ age + sex + diabp + cigpday + bmi, data=framingham)
summary(lm.1)

# Performing an F-test (testing all betas = 0)
mean.mod <- lm(totchol ~ 1, data=framingham); summary(mean.mod)
anova(mean.mod, lm.1)

# Performing an F-test (testing beta_bmi equal to 0)
lm.2 <- lm(totchol ~ age + sex + diabp + cigpday, data=framingham); summary(lm.2)
anova(lm.2, lm.1)

# Making a cubic spline of bmi with 4 knots
lm.3 <- lm(totchol ~ age + sex + diabp + cigpday + bmi + rcs(bmi), data=framingham)
summary(lm.3)

# Performing an F-test (testing whether the slopes of the spline terms are equal to 0)
anova(lm.1, lm.3)

#-------------------------
# Robust Variances
#-------------------------

# Creating residual plots for Model 1
plot(lm.1, 2) # qqplot
plot(lm.1, 1) # residuals vs. fitted values

# Getting the entire variance-covariance matrix using robust variance estimation
sandwich.vcov <- vcovHC(lm.1, type="HC")
sandwich.vcov

# Finding the robust standard errors for each covariate
sandwich.se <- diag(sandwich.vcov)^(0.5)
sandwich.se

#-------------------------
# Introduction to Logistic Regression
#-------------------------

# Reading in the smoker.dta file
lab6 <- read.dta("smoker.dta")
View(lab6)

# Creating a contingency table for the relationship between student smoking and parental smoking
#   If we had unique observations for every individual, we could also use the command table()
xtabs(freq~psmoke+ssmoke, data=lab6)
table(framingham$sex, framingham$cvd)

# Fitting the logistic regression
#   The family="binomial" option tells R we're working with binary data. This is an important
#   option that you need to include in order to get the correct answer!
lm.4 <- glm(ssmoke ~ psmoke, family="binomial", weights=freq, data=lab6)
summary(lm.4)

lm.4.wrong <- glm(ssmoke ~ psmoke, data = lab6)
summary(lm.4.wrong)