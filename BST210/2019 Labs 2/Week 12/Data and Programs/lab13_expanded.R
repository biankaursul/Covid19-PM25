### PART 1: Survival Analysis
srt <- read.csv("srt.csv")
head(srt)

srt$sex = factor(srt$sex)
srt$sorb = factor(srt$sorb)

#Set up survival analysis
library(survival)
library(survminer)
survobj = Surv(srt$fup,srt$status)
print(survobj)

#Find Kaplan-Meier estimate
model1 = survfit(survobj~sorb+sex,data=srt)
summary(model1)

#Plot Kaplan-Meier estimate
ggsurvplot(model1)

### PART 2: Log-Rank Test
survdiff(survobj~sorb,data=srt) # Test equality of treatment and placebo survival curves
survdiff(survobj~sorb+strata(sex),data=srt) # Test equality, stratified by sex
survdiff(survobj~sorb+sex,data=srt) # Test equality of all four curves

### PART 3: Parametric Models
model2 = survreg(survobj ~ sorb+tgh+dur+sex, dist="exponential", data=srt)
summary(model2)
exp(-1*coef(model2))

#Plot curves for treated men and untreated men, 
# evaluate other covariates at mean (for continuous covariates) or mode (for categorical covariates)
#You can also get estimates for parameters from the model, 
# and plot with "curve()"
plot(predict(model2, newdata=list(sex=factor(1), sorb=factor(0), tgh=11.82, dur=6.875),
             type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col="red", type = 'l')
lines(predict(model2, newdata=list(sex=factor(1), sorb=factor(1), tgh=11.82, dur=6.875),
              type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col="blue")

library(SurvRegCensCov)
model3 = ConvertWeibull(survreg(survobj ~ sorb+tgh+dur+sex, dist="weibull", data=srt))
print(model3$vars)
print(model3$HR)

plot(predict(survreg(survobj ~ sorb+tgh+dur+sex, dist="weibull", data=srt), 
             newdata=list(sex=factor(1), sorb=factor(0), 
                                  tgh=11.82, dur=6.875),type="quantile",
             p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col="red", type = 'l')
lines(predict(survreg(survobj ~ sorb+tgh+dur+sex, dist="weibull", data=srt), 
              newdata=list(sex=factor(1), sorb=factor(1), 
                                   tgh=11.82, dur=6.875),type="quantile",
              p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col="blue")


### PART 4: Cox Proportional Hazards Models
model4 = coxph(survobj~sorb+tgh+dur+sex,data=srt)
summary(model4)
exp(coef(model4))

ggsurvplot(survfit(model4,newdata = with(srt,
                               data.frame(sex = factor(c(1, 1)), sorb = factor(c(0,1)),
                                          tgh = rep(mean(tgh, na.rm = TRUE), 2),
                                          dur = rep(mean(dur, na.rm = TRUE), 2)
                               ))),data=srt)
