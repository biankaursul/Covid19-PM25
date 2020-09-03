#-------------------------#
#   BIO210 LAB 11         #
#   November 8-9, 2017    #
#-------------------------#

#----------------------------------
# Poisson Regression
#----------------------------------

# Reading in the dataset
melanoma <- read.csv(file=file.choose())
View(melanoma)
# Fitting a Poisson regression model for the relationship between latitude and melanoma incidence
melanoma$latitude <- factor(melanoma$latitude, levels=unique(melanoma$latitude)[c(3, 1, 2)])
lat1 <- glm(inccases ~ as.factor(latitude), offset=log(persyrs), data=melanoma, family=poisson())
summary(lat1)

# Performing an LRT for significance of latitude
intercept.mod <- glm(inccases ~ 1, offset=log(persyrs), data=melanoma, family=poisson())
summary(intercept.mod)
anova(intercept.mod, lat1, test="Chisq")

# Comparing Northern and Middle latitudes
exp(coef(lat1)[2] - coef(lat1)[3]) # IRR estimate
var.diff <- vcov(lat1)[2,2] + vcov(lat1)[3,3] - 2*vcov(lat1)[2,3] # variance of (Northern)-(Middle)
exp(coef(lat1)[2] - coef(lat1)[3] + c(-1, 1)*1.96*sqrt(var.diff)) # 95% CI
test.stat <- (coef(lat1)[2] - coef(lat1)[3])^2/var.diff # Chi-square test statistic (reported by SAS)
1-pchisq(test.stat, 1) # p-value for IRR

#----------------------------------
# Confounding & Effect Modification
#----------------------------------

# Age Group as a confounder
melanoma$ageg <- factor(melanoma$ageg, levels=unique(melanoma$ageg)[c(6, 1, 2, 3, 4, 5)])
lat2 <- glm(inccases ~ as.factor(latitude) + as.factor(ageg), offset=log(persyrs), data=melanoma, family=poisson())
summary(lat2)

# Age as an independent predictor
anova(lat1, lat2, test="Chisq")

# Age an an effect modifier
lat3 <- glm(inccases ~ as.factor(latitude) + as.factor(ageg) + as.factor(latitude)*as.factor(ageg), offset=log(persyrs), data=melanoma, family=poisson())
summary(lat3)
anova(lat2, lat3, test="Chisq")

#----------------------------------
# Overdispersion
#----------------------------------

# Checking for Overdispersion (Latitude-only model)
deviance(lat1)/lat1$df.residual
pearson.stat1 <- sum((melanoma$inccases - fitted(lat1))^2/fitted(lat1))
pearson.stat1/lat1$df.residual

# Checking for Overdispersion (Interaction model)
deviance(lat3)/lat3$df.residual
pearson.stat3 <- sum((melanoma$inccases - fitted(lat3))^2/fitted(lat3))
pearson.stat3/lat3$df.residual

# Negative Binomial Regression
library(MASS)
lat.negbinom <- glm.nb(inccases ~ as.factor(latitude) + offset(log(persyrs)), data=melanoma, link=log)
summary(lat.negbinom)

# Robust Variance Estimation
#   Note that the following code does something slightly different than what Stata and SAS are
#   doing. It is correcting for overdispersion and inflating the variance, but the mechanism
#   by which it's doing that is differenct: it's using something called a quasi-likelihood
#   rather than a robust variance estimator. I've included a function that you can use to 
#   calculate the robust variance below (code taken from BST 233 materials).
lat.robust <- glm(inccases ~ as.factor(latitude), offset=log(persyrs), data=melanoma, family=quasipoisson())
summary(lat.robust)

# Code for Robust Standard Errors
#   This returns output that appears similar to what we would get from a standard summary()
#   call, but that includes robust standard errors (with the original standard errors 
#   provided as a point of comparison).
#   Code credit: BST 233 Methods II.
robustSE <- function(fit, digits=3) {
  Xmat <- model.matrix(terms(fit), model.frame(fit))
  Umat <- residuals(fit, type="working") * fit$weights * Xmat
  modelV <- summary(fit)$cov.unscaled
  robustV <- modelV %*% t(Umat) %*% Umat %*% modelV
  value <- cbind(fit$coef, sqrt(diag(modelV)), sqrt(diag(robustV)),
                 sqrt(diag(robustV))/sqrt(diag(modelV)))
  colnames(value) <- c("Estimate", "Model SE", "Robust SE", " Ratio")
  return(round(value, digits=digits))
}
robustSE(lat1)


#----------------------------------
# Zero-Inflated Poisson Regression
#----------------------------------

# Reading in the dataset & loading required packages
school <- read.csv(file=file.choose())
install.packages("pscl")
library(pscl)

# Histogram of days absent
hist(school$daysabs, breaks=seq(0, max(school$daysabs), by=1))

# Zero-inflated Poisson with constant probability of structural zero
zip1 <- zeroinfl(daysabs ~ mathnce + langnce + female | 1, data = school)
summary(zip1)

# Zero-inflated Poisson with structural zero probability depending on math scores
zip2 <- zeroinfl(daysabs ~ mathnce + langnce + female | mathnce, data = school)
summary(zip2)
