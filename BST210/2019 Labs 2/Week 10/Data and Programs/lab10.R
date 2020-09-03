# Set up the working directory and input our data
path <- ("C:/Users/reede/Documents/17-XX (Harvard)/Teaching Materials/BST 210 (Applied Regression)/Labs/Input/")

###################################
# Conditional Logistic Regression #
###################################

whs = read.table(paste0(path,"whsmtch.dat"))
colnames(whs) = c("id", "crpq", "sbp", "dm", "age", "smok", "chdlq", "caco")

# Drop an observation with missing sbp and create a new outcome
whs = whs[whs$id != whs[whs$sbp == ".",]$id,]
whs$sbp = as.numeric(levels(whs$sbp))[whs$sbp]
whs$high_sbp = as.numeric(whs$sbp >= 140)


library(survival)
library(ResourceSelection)
library(LogisticDx)
fit0 <- clogit(caco ~ high_sbp + strata(id), data=whs)
summary(fit0)

# Create categorical sbp
whs$sbpcat = 1
whs$sbpcat = whs$sbpcat + (whs$sbp >119) + (whs$sbp >139) + (whs$sbp >159)

# Fit categorical model
fit1 = clogit(caco ~ as.factor(sbpcat) + strata(id), data = whs)
summary(fit1)

# Fit ordinal model
fit2 = clogit(caco ~ sbpcat + strata(id), data = whs)
summary(fit2)

# Compare using a likelihood ratio test
anova(fit2,fit1)


#############################
# Generalized Linear Models #
#############################

# Load in the data and summarize some of the covariates
load("WCGS_data.dat")
apply(wcgs[,1:7], 2, FUN=summary)

# remove a strangely high value and remove missingness
wcgs$chol[wcgs$chol > 500] = NA 
wcgs = na.omit(wcgs)

# create smoker variable
wcgs$smoker = as.numeric(wcgs$ncigs > 0)

# Fit our normal logistic model
fit.logistic = glm(chd ~ behave, family = binomial(), data = wcgs)
summary(fit.logistic)

# Fit an adjusted model
fit.logistic.adj = glm(chd ~ behave + age + wt + sbp + chol + smoker,
                       family = binomial(), data = wcgs)
summary(fit.logistic.adj)

# Test with a likelihood ratio test if we need adjustment terms
anova(fit.logistic, fit.logistic.adj)

