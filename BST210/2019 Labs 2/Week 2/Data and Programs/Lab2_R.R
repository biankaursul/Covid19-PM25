#---------------------------------#
# BST 210: Lab Week 2             #
# September 7-8, 2017             #
#---------------------------------#

#----------------------------------
# Simple Linear Regression
#----------------------------------

# Importing the File
lab <- read.csv(file="lab2.csv")

# Creating a Scatterplot for the Relationship Between SBP and Quet
plot(lab$sbp ~ lab$quet, main="Relationship between SBP and quet", xlab="quet", 
     ylab="Systolic Blood Pressure")

# Running Simple Linear Regression
sbp.quet <- lm(sbp ~ quet, data=lab)
summary(sbp.quet) # shows coefficients and associated p-values

# Regression Diagnostics
#    We can use the functions resid() and fitted() to find the residuals and fitted values,
#    respectively, of our model, and then plot them to assess our assumptions
plot(resid(sbp.quet) ~ fitted(sbp.quet)) # assesses linearity and equal variance
abline(h=0, col="red") # adds the zero line for reference
qqnorm(resid(sbp.quet)) # assesses normality
qqline(resid(sbp.quet)) # adds reference line
hist(resid(sbp.quet)) # alternative method to assess normality

# Assessing Model Fit
#   R^2, adjusted R^2, and the root MSE are all reported in the summary() output for a linear
#   regression model. Note that R^2 is referred to as "multiple R-squared", and that the root
#   MSE is reported as the "residual standard error"

# Hypothesis Testing
#   The test statistic and associated p-value for the hypothesis tests corresponding to each
#   regression coefficient are given in the summary() table

# Confidence Interval for the Slope
#    Note that we could also take the standard error from the summary() output and calculate the CI
#    by hand.
confint(sbp.quet, level=0.95) # allows us to change the level of the CI
coef(sbp.quet)[2] + c(-1, 1)*qt(0.975, 30)*sqrt(vcov(sbp.quet)[2, 2]) # calculates the CI by hand

#----------------------------------
# Multiple Linear Regression
#----------------------------------

# Multiple Linear Regression with Quet and Age: Confounding
mult.conf <- lm(sbp ~ quet + age, data=lab)
summary(mult.conf)

# Multiple Linear Regression with Interaction Term: Effect Modification
mult.em <- lm(sbp ~ quet*smk, data=lab)
summary(mult.em)

# Plotting the Fitted Regression Lines (from the Interaction Model)
plot(lab$sbp ~ lab$quet, xlab="Quetelet Index", ylab="Systolic Blood Pressure (mmHg)")
lines(lab$quet[which(lab$smk == 1)], fitted(mult.em)[which(lab$smk == 1)], col="red") # adds fitted line for smokers
lines(lab$quet[which(lab$smk == 0)], fitted(mult.em)[which(lab$smk == 0)], col="blue") # adds fitted line for non-smokers

#-------------------------------------------------------------
# Additional Topic: Confidence Intervals for Predicted Values
#-------------------------------------------------------------

# Confidence Interval for the Predicted Subpopulation Mean
newData <- data.frame(quet=3.5)
predict(sbp.quet, newData, interval="confidence")

# Prediction Interval for an Individual
predict(sbp.quet, newData, interval="prediction")

# Plotting Regression Line, Confidence Bands, Prediction Bands
quetVals <- data.frame(quet=sort(lab$quet))  # sort to ease plotting
confBand <- predict(sbp.quet, quetVals, interval="confidence")
predBand <- predict(sbp.quet, quetVals, interval="prediction")
plot(lab$quet, lab$sbp, main="Systolic Blood Pressure vs. Quet", ylab="SBP (mmHg)",
     xlab="Quetelet Index", pch=20)
abline(reg=sbp.quet, col="red")
lines(quetVals$quet, predBand[,"lwr"], col="green4", type="l")
lines(quetVals$quet, predBand[,"upr"], col="green4", type="l")
lines(quetVals$quet, confBand[,"lwr"], col="blue", type="l")
lines(quetVals$quet, confBand[,"upr"], col="blue", type="l")

# Adding a Legend
legend("topleft", col=c("red", "blue", "green4"), lty=1,
       legend=c("regression line","confidence band", "prediction band"), cex=.8)

