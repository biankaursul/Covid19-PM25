#-------------------------#
#   BIO210 LAB 07         #
#   October 13-14, 2016   #
#-------------------------#

# Loading packages
library(foreign)

#----------------------------------
# Connection to Contingency Tables
#----------------------------------

# Reading in the dataset
smoker <- read.dta(file=file.choose())

# Fitting a logistic regression model
smoke.glm <- glm(ssmoke ~ psmoke, family=binomial(), weights=freq, data=smoker)
summary(smoke.glm)

#----------------------------------
# Hypothesis Testing
#----------------------------------

# Reading in the dataset & re-naming covariates in lower case
glow <- read.csv(file=file.choose())
names(glow) <- tolower(names(glow))

#----------------------------------
# Binary Age

# Creating an indicator for Age > 75 years
glow$age.75 <- ifelse(glow$age > 75, 1, 0)

# Contingency table for Age > 75 years and bone bracture
table(glow$age.75, glow$fracture, dnn=c("Age > 75", "Fracture"))

# Fitting a simple logistic regression model
age.binary <- glm(fracture ~ age.75, family=binomial(), data=glow)
summary(age.binary)

# Fitting the reduced model with only the intercept
age.binary.red <- glm(fracture ~ 1, family=binomial(), data=glow)

# Performing the Likelihood Ratio test
anova(age.binary.red, age.binary, test="Chisq")

#----------------------------------
# Categorical Age

# Creating categorical age
glow$age.cat <- rep(NA, nrow(glow))
for (i in 1:nrow(glow)){
  if (glow$age[i] <= 65) {
    glow$age.cat[i] <- 0
  } else if (glow$age[i] <= 75) {
    glow$age.cat[i] <- 1
  } else{
    glow$age.cat[i] <- 2
  }
}

# Fitting a logistic regression model with categorical age
age.category <- glm(fracture ~ as.factor(age.cat), family=binomial(), data=glow)
summary(age.category)

# Fitting a logistic regression model with categorical age (treated as continuous)
age.category2 <- glm(fracture ~ age.cat, family=binomial(), data=glow)
summary(age.category2)

# Conducting the Likelihood Ratio test (comparing categorical models)
anova(age.category2, age.category, test="Chisq")

#----------------------------------
# Continuous Age

# Fitting a logistic regression model with continuous age
age.continuous <- glm(fracture ~ age, family=binomial(), data=glow)
summary(age.continuous)


#----------------------------------
# Confounding
#----------------------------------

# Fitting a logistic regression model without age
priorfrac1 <- glm(fracture ~ priorfrac, family=binomial(), data=glow)
summary(priorfrac1)

# Fitting a logistic regression model with age
priorfrac2 <- glm(fracture ~ priorfrac + age, family=binomial(), data=glow)
summary(priorfrac2)

#----------------------------------
# Effect Measure Modification
#----------------------------------

# Assessing whether prior history of fractures is an effect modifier
priorfrac3 <- glm(fracture ~ priorfrac*age, family=binomial(), data=glow)
summary(priorfrac3)

# Plotting fitted log odds
curve(coef(priorfrac3)[1] + coef(priorfrac3)[3]*x, xlim=c(55, 90), xlab="Age", ylab="Logit(p)", col="dodgerblue")
curve(coef(priorfrac3)[1] + coef(priorfrac3)[2] + (coef(priorfrac3)[3] + coef(priorfrac3)[4])*x, xlim=c(55, 90), col="magenta", add=T)

# Plotting fitted probabilities
curve(exp(coef(priorfrac3)[1] + coef(priorfrac3)[3]*x)/(1+ exp(coef(priorfrac3)[1] + coef(priorfrac3)[3]*x)), xlim=c(55, 90), ylim=c(0,1), xlab="Age", ylab="Risk of Fracture", col="dodgerblue")
curve(exp(coef(priorfrac3)[1] + coef(priorfrac3)[2] + (coef(priorfrac3)[3] + coef(priorfrac3)[4])*x)/(1 + exp(coef(priorfrac3)[1] + coef(priorfrac3)[2] + (coef(priorfrac3)[3] + coef(priorfrac3)[4])*x)), xlim=c(55, 90), col="magenta", add=T)

# Note: using the fitted() function in R produces the fitted *probabilities*
#   Using the lines() function to get a single smooth curve can be a bit difficult, as it
#   requires sorting all of the observations correctly in order of ascending age. The code
#   below will recreate the fitted probababilities plot above, but the code is much more
#   involved and less intuitive.
plot(glow$fracture ~ glow$age, xlab="Age", ylab="Risk of Fracture")
lines(fitted(priorfrac3)[order(glow$age)][which(glow$priorfrac[order(glow$age)]==0)] ~ sort(glow$age)[which(glow$priorfrac[order(glow$age)]==0)], col="pink")
lines(fitted(priorfrac3)[order(glow$age)][which(glow$priorfrac[order(glow$age)]==1)] ~ sort(glow$age)[which(glow$priorfrac[order(glow$age)]==1)], col="green")
