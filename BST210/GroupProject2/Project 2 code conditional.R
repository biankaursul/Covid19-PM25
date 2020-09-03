library(haven)
library(tidyr)
library(dplyr)
framingham <- read_dta(file=file.choose())
View(framingham)
framingham = framingham[(framingham$prevap==0 & framingham$prevchd==0 & framingham$prevmi==0 & framingham$prevstrk==0),]
framingham = framingham[c(1:12, 17:18, 34:35)] 
framingham = framingham %>% na.omit()
framingham$sex = framingham$sex-1
#framingham[(framingham$death == 1) & (framingham$agecat == 4) & (framingham$sex == 0),]
#framingham[(framingham$death == 1) & (framingham$agecat == 4) & (framingham$sex == 1),]
#framingham[(framingham$death == 0) & (framingham$agecat == 4) & (framingham$sex == 0),]
#framingham[(framingham$death == 0) & (framingham$agecat == 4) & (framingham$sex == 1),]
all.death = framingham[framingham$death == 1,]
alive = framingham[framingham$death == 0,]

all.death$stratum = all.death$sex * 4 + all.death$agecat
  # (all.death$sex == 0 & all.death$agecat == 1) *1 + 
  # (all.death$sex == 0 & all.death$agecat == 2) *2 +
  # (all.death$sex == 0 & all.death$agecat == 3) *3 +
  # (all.death$sex == 0 & all.death$agecat == 4) *4 +
  # (all.death$sex == 1 & all.death$agecat == 1) *5 +
  # (all.death$sex == 1 & all.death$agecat == 2) *6 +
  # (all.death$sex == 1 & all.death$agecat == 3) *7 +
  # (all.death$sex == 1 & all.death$agecat == 4) *8
sample.control = data.frame()
sample.death = data.frame()
for (i in 1:8){
  group = all.death[all.death$stratum == i,]
  group.sex = as.numeric(group[1,"sex"])
  group.agecat = as.numeric(group[1,"agecat"])
  pot.control = alive[(alive$sex == group.sex) & (alive$agecat == group.agecat),]
  if (nrow(group) > nrow(pot.control)){
    group = sample_n(group, nrow(pot.control))
  }
  set.seed(i+1)
  group.sample = sample_n(pot.control, nrow(group))
  group.sample$stratum = i
  sample.control = rbind(sample.control, group.sample)
  sample.death = rbind(sample.death, group)
}

case.control = rbind(sample.control, sample.death)
# View(case.control)

# Data cleaning for case.control dataset
case.control = case.control[-c(16)]

library(MASS)
library(survival)
# dropping bpmeds because it's colinear with prevhyp
full.cond.model = clogit(death ~ .-stratum + strata(stratum), data = case.control)
summary(full.cond.model)

hyp.cond.model = clogit(death ~ prevhyp + strata(stratum), data = case.control)
summary(hyp.cond.model)

step.cond.model = stepAIC(full.cond.model, direction = "both", trace = FALSE)
summary(step.cond.model)

# Prevhyp as the main variable of interest: want to examine the effect of previous hypertention on the incidence of death
# Consider potential confounders according to the conventional definition of confounder: cigpday, bmi, (diabetes), (glucose)

hyp.cond.model2 = clogit(death ~ prevhyp + cigpday + strata(stratum), data = case.control)
summary(hyp.cond.model)
summary(hyp.cond.model2)
# cigpday not confounder

hyp.cond.model3 = clogit(death ~ prevhyp + prevhyp*cigpday + strata(stratum), data = case.control)
summary(hyp.cond.model3)
# cigpday not effect modifier

hyp.cond.model4 = clogit(death ~ prevhyp + bmi + strata(stratum), data = case.control)
summary(hyp.cond.model4)

hyp.cond.model5 = clogit(death ~ prevhyp + prevhyp*bmi + strata(stratum), data = case.control)
summary(hyp.cond.model5)
# bmi not confounder or effect modifier

hyp.cond.model6 = clogit(death ~ prevhyp + diabetes + strata(stratum), data = case.control)
summary(hyp.cond.model6)

hyp.cond.model7 = clogit(death ~ prevhyp + prevhyp*diabetes + strata(stratum), data = case.control)
summary(hyp.cond.model7)
# diabetes not significant

hyp.cond.model8 = clogit(death ~ prevhyp + glucose + strata(stratum), data = case.control)
summary(hyp.cond.model8)

hyp.cond.model9 = clogit(death ~ prevhyp + glucose* prevhyp + strata(stratum), data = case.control)
summary(hyp.cond.model9)
# glucose not significant

# It is not sensible to use clogit model to give predictions because clogit model does not give the intercept term as they're not interpretable.
# Therefore, clogit model is only useful when considering the OR of death between individuals having previous hypertension, and those without hypertension. 

hyp.cond.model10 = clogit(death ~ prevhyp + agecat*prevhyp + strata(stratum), data = case.control)
summary(hyp.cond.model10)

hyp.cond.model11 = clogit(death ~ prevhyp + sex* prevhyp + strata(stratum), data = case.control)
summary(hyp.cond.model11)
# Conclusion: no other variables has confounding effect on the effect of prev hyp on incidence of death. 
# The odds of death among individuals with previous hypertension is estimated to be 2.16 times of the odds of death among those without hypertension.
hyp.cond.model12 = clogit(death ~ prevhyp + heartrte + strata(stratum), data = case.control)
summary(hyp.cond.model12)

hyp.cond.model.final = clogit(death ~ prevhyp + strata(stratum), data = case.control)
summary(hyp.cond.model.final)
confint((hyp.cond.model.final))

model1  = clogit(death ~ prevhyp + totchol + strata(stratum), data = case.control)
summary(model1)

dim(case.control)
case.control[case.control$stratum == 1,]
case.control[case.control$stratum == 2,]
case.control[case.control$stratum == 3,]











