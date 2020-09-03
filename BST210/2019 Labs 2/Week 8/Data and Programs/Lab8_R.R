#---------------------------------#
# BST 210: Lab Week 8             #
# October 19-20, 2017             #
#---------------------------------#

install.packages("ResourceSelection")
install.packages("LogisticDx")
library(ResourceSelection)
library(LogisticDx)

# Reading in the Data
glow <- read.csv("glow.csv")
names(glow) <- tolower(names(glow))

#----------------------------------
# Model Building
#----------------------------------

# Running a forward selection procedure (note that this uses AIC)
mod_forw <- step(glm(fracture ~ 1, data = glow, family=binomial), ~fracscore+raterisk+smoke+armassist+momfrac+premeno+bmi+height+age+priorfrac, direction = "forward")
summary(mod_forw)

# Running a backward selection procedure (using AIC)
full.mod <- glm(fracture ~ fracscore+raterisk+smoke+armassist+momfrac+premeno+bmi+height+age+priorfrac, data=glow, family=binomial)
mod_back <- step(full.mod, direction = "backward")
summary(mod_back)

# Running a stepwise selection procedure (using AIC)
mod_step <- step(glm(fracture ~ 1, data = glow, family=binomial), ~fracscore+raterisk+smoke+armassist+momfrac+premeno+bmi+height+age+priorfrac, direction = "both")
summary(mod_step)

# I couldn't immediately find a best subset selection procedure in R: the 
# one I was able to identify in Lab 5 relies on the Adjusted R^2 :/

#----------------------------------
# Calibration 
#----------------------------------

# Pearson Chi-Square Test
by(cbind(glow$priorfrac, glow$armassist), glow$fracture, FUN=function(x)table(x[,1], x[,2]))

# There doesn't seem to be an easy way to perform a Chi-Square test for 
# contingency tables that consider three variables (as we do here: fracture,
# armassist, and priorfrac). You can always compute the statistic by hand
# using the formula seen in class, and then compare that to a chi-square
# distribution with the appropriate degrees of freedom.

# Hosmer-Lemeshow Test for the Forward Selection Model
hoslem.test(glow$fracture,fitted(mod_forw),g=10) 

# Hosmer-Lemeshow Test for the Backward Elimination Model
hoslem.test(glow$fracture, fitted(mod_back), g=10)

#----------------------------------
# Discrimination
#----------------------------------

# AUC for Forward Selection Model
gof(mod_forw)

# AUC for Backward Elimination Model
gof(mod_back)

