srt <- read.csv("srt.csv")

#Part 1: Log-Rank Tests
srt$sex = factor(srt$sex)
srt$sorb = factor(srt$sorb)
#Summarize by treatment group
summary(srt[srt$sorb==0,])
summary(srt[srt$sorb==1,])

#Set up survival analysis
library(survival)
survobj = Surv(srt$fup,srt$status)
model1 = survfit(survobj~sorb+sex,data=srt)

#Plot Kaplan-Meier estimate and test using Log-Rank test
plot(model1,col=c(2,3)) #I would suggest you add a legend, title, axes etc.

survdiff(survobj~sorb+sex,data=srt)

#Part 2: Parametric models
model2 = survreg(survobj ~ sorb+tgh+dur+sex, dist="exponential", data=srt)
#Plot curves for male and female separately, 
# evaluate other covariates at mean (for continuous covariates) or mode (for categorical covariates)
#You can also get estimates for parameters from the model, 
# and plot with "curve()"
plot(predict(model2, newdata=list(sex=factor(1), sorb=factor(0), tgh=11.82, dur=6.875),
             type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col="red", type = 'l')
lines(predict(model2, newdata=list(sex=factor(2), sorb=factor(0), tgh=11.82, dur=6.875),
              type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col="blue")

model3 = survreg(survobj ~ sorb+tgh+dur+sex, dist="weibull", data=srt)
plot(predict(model3, newdata=list(sex=factor(1), sorb=factor(0), tgh=11.82, dur=6.875),type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col="red", type = 'l')
lines(predict(model3, newdata=list(sex=factor(2), sorb=factor(0), tgh=11.82, dur=6.875),type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col="blue")


#Part 3: Cox Proportional Hazards Models
model4 = coxph(survobj~sorb+tgh+dur+sex,data=srt)
plot(survfit(model4))
