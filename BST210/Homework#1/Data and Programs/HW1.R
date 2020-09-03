
dat <- read.csv("SCCS2_v12.csv")
View(dat)

#1(a)
par(mfrow = c(1,1))
summary(dat)
age.tc = lm(tc~age, data = dat)
summary(age.tc)
confint(age.tc)
plot(dat$tc~dat$age, xlab = "age", ylab = "total cholesterol", type = "p", main="Relationship between Total Cholesterol and Age")
# simple linear regression model: 
# intercept: 4.376, p-value < 2e-16, 95% conf interval: (4.109, 4.643)
# slope: 0.026, p-value < 2e-16, 95% conf interval: (0.020, 0.032)
# For a change of an age decade, the mean total cholesterol level increases by 2.6 units. The p-value is smaller than 0.05, indicating the statistical significance of the slope value.
abline(age.tc, col = "blue")
cor.test(dat$age, dat$tc) # Pearson's correlation test
# p-value < 2.2e-16, 95% confidence interval: (0.2836,0.4324), sample estimates: 0.36, the result from pearson's correlation test indicates that there is a weak positive correlation between age and total cholesterol(r = 0.36)
# R-squared value from linear regression model: 0.1298, which means that only 13% of the observed total cholesterol data can be explained by the linear regression of total cholesterol on age, indicating that the linear model is not a good fit for the date. The Pearson's correlation coefficient corroborates this suggestion, suggesting the data is not pretty linearly associated. 
par(las = 0)
#1(b)
par(mfrow = c(2,2))
plot(resid(age.tc)~fitted(age.tc))
abline(h = 0, col = "red")
qqnorm(resid(age.tc))
qqline(resid(age.tc))
hist(resid(age.tc),prob = TRUE)
m = mean(resid(age.tc))
std = sqrt(var(resid(age.tc)))
curve(dnorm(x,mean = m, sd = std),add = TRUE, col = "red")
# No, the residuals are not normally distributed according to the normal Q-Q plot. The right tail of the sample quantile deviates from the reference line.
# Also, according to the histogram of the residual, the residual data is skewed to the right.
par(las = 0)

#1(c)
par(mfrow = c(1,1))
plot(dat$age,dat$tc, xlab = "age", ylab = "Total Cholesterol", main = "Lowess smoother")
lines(loess.smooth(dat$age,dat$tc), col = 'red',lty = 1, lwd = 2)
# scatter.smooth(dat$age,dat$tc,xlab = "age", ylab = "Total Cholesterol")
par(mfrow = c(1,1))
age_2.tc = lm(tc~ age + I(age^2),data = dat)
summary(age_2.tc)

# The adjusted regression model doesn't give an appreciably different result than the original one, with R^2 = 0.1472, still not a good fit.

# 2(a)
t.test(dat$tc ~ dat$gender, var.equal = T)
# The two sample t-test t value is -0.63874, which gives a p-value of 0.5 => We fail to reject null hypothesis that there's no difference in mean total cholesterol between male and female.

gender.tc = lm(tc~gender, data= dat)
summary(gender.tc)
# The multiple R-squared value equals 0.0007765, suggesting that the linear regression on gender is not a good predictor is not a good predictor of total cholesterol, which corresponds to the t-test result that there is no significant difference in mean total cholesterol among female and male.
# We can observe from the linear regression summary the coefficients for gender is 0.05870, which is the mean difference of tc between female and male, and the y intercept value 5.49 is equivalent to tc mean of gender group 0.

# 2(b)
age_2_gender.tc = lm(tc~age + I(age^2) + gender, data = dat)
summary(age_2_gender.tc)
# The coefficients of age and age-square change from 0.09203 to 0.0909455, and from -0.0007389 to -0.0007241, respectively, while the adjusted analysis does not give a significant different result than the regression model using both linear and quadratic age. Therefore, we do not consider gender as a confounder of the effect of linear and quadratic age on tc.
# Gender is not an independent predictor because we've seen in 2(a), R^2 value for gender alone as a predictor variable is too small(R^2 = 0.0007765) to be considered as an independent predictor of tc.
fitted(age_2_gender.tc)
#2(c)
par(mfrow = c(1,1))

color <- function(gender){
  color = c(0, length(gender))
  for (i in 1:length(gender)){
    if (gender[i] == 0) {
      color[i] = "red"
      }
    else {
      color[i] = "blue"
    }
  }
  return (color)
}  

plot(dat$age, dat$tc, xlab = "age", ylab = "total cholesterol", col = color(dat$gender))
lines(loess.smooth(dat$age[which(dat$gender == 0)], fitted(age_2_gender.tc)[which(dat$gender == 0)]), col = "red", lwd = 2)
lines(loess.smooth(dat$age[which(dat$gender == 1)], fitted(age_2_gender.tc)[which(dat$gender == 1)]), col = "blue", lwd =2)
legend("topright",c("fitted regression line for females", "fitted regression line for males"),col = c("red","blue"),lty = 1:1)

#2(d)
full_int = lm(tc ~ age + I(age^2) + gender + age*gender + I(age^2)*gender, data = dat)
summary(full_int)
# P-value for interaction between gender and age is 0.04572, and for interaction between gender and quadratic age is 0.00935, both of which < 0.05, indicating that the interactions are statistically significant.
# Thus gender is an effect modifier of both the effect of linear and quadratic age on total cholesterol
plot(dat$age, dat$tc, xlab = "age", ylab = "total cholesterol", col = color(dat$gender))
lines(loess.smooth(dat$age[which(dat$gender == 0)], fitted(full_int)[which(dat$gender == 0)]), col = "red", lwd = 2)
lines(loess.smooth(dat$age[which(dat$gender == 1)], fitted(full_int)[which(dat$gender == 1)]), col = "blue", lwd =2)
legend("topright",c("fitted regression line for females", "fitted regression line for males"),col = c("red","blue"),lty = 1:1)
# The curves for females and males are no longer parallel. After the age of 50, the difference of predicted total cholesterol between females and males enlarges as age increases.

#2(e)
# R^2 value for the linear regression models
r.square = c( full = summary(full_int)$r.squared, 
              age = summary(age.tc)$r.squared, 
              age_2 = summary(age_2.tc)$r.squared, 
              age.gender = summary(age_2_gender.tc)$r.squared, 
              gender = summary(gender.tc)$r.squared)
r.square
# full interaction model has the highest r-squared value
adj.r.square = c (full = summary(full_int)$adj.r.squared,
                  age = summary(age.tc)$adj.r.squared,
                  age_2 = summary(age_2.tc)$adj.r.squared,
                  age.gender = summary(age_2_gender.tc)$adj.r.squared,
                  gender = summary(gender.tc)$adj.r.squared)
adj.r.square
# full interaction model has the highest adjusted r-squared value


# 2(f)
dat_woman = dat[dat$gender == 0, ]
dat_man = dat[dat$gender == 1, ]
age_2_woman = lm(tc~age + I(age^2), data = dat_woman)
age_2_man = lm(tc~age + I(age^2), data = dat_man)
par(mfrow = c(1,2))

# plot regression for women
plot(dat_woman$age, dat_woman$tc, xlab = "age", ylab = "total cholesterol", main = "Total Cholesterol vs. Age for WOMEN")
lines(loess.smooth(dat_woman$age, fitted(age_2_woman)), col= "red",lwd = 2)
lines(loess.smooth(dat$age[which(dat$gender == 0)], fitted(full_int)[which(dat$gender == 0)]), col = "blue", lwd = 2)
legend("topright", c("Fitted regression line:age + age^2", "Fitted regression line: full"), col = c("red","blue"), lwd = 1:1, cex = 0.7)

# plot regression for men
plot(dat_man$age, dat_man$tc, xlab = "age", ylab = "total cholesterol", main = "Total Cholesterol vs. Age for MEN")
lines(loess.smooth(dat_man$age, fitted(age_2_man)), col= "red",lwd = 2)
lines(loess.smooth(dat$age[which(dat$gender == 1)], fitted(full_int)[which(dat$gender == 1)]), col = "blue", lwd = 2)
legend("topright", c("Fitted regression line:age + age^2", "Fitted regression line: full"), col = c("red","blue"), lwd = 1:1, cex = 0.7)

# test
plot(dat$age, dat$tc)
age_2_woman_test = lm(tc~age +I(age^2), data = dat[which(dat$gender == 0),])
age_2_man_test = lm(tc~age +I(age^2), data = dat[which(dat$gender == 1),])
plot(dat$age, dat$tc)
lines(loess.smooth(dat$age[which(dat$gender == 0)], fitted(age_2_woman_test)))
lines(loess.smooth(dat$age[which(dat$gender == 1)], fitted(age_2_man_test)))
# I would choose the full interaction model. In this case, there are only two categories in gender variable, so we only need to separately generate two linear regression models for males and females. 
# But in other cases, the categorical variable may have more categories, which make it harder to generate a separate linear regression model for each category.
# The full interaction model incorporates all possible values for the categorical variable and generate a more comprehensive model.

# 3(a)
dat$BMI = dat$weight / (dat$height/100)^2
dat$BMI_categorical[dat$BMI < 18.5] = "underweight"
dat$BMI_categorical[(dat$BMI >= 18.5) & (dat$BMI < 25)] = "normal weight"
dat$BMI_categorical[(dat$BMI >= 25) & (dat$BMI < 30)] = "overweight"
dat$BMI_categorical[dat$BMI >= 30] = "obese"
summary(dat$BMI_categorical)

newdat = dat[order(dat$BMI),]
newdat
BMI_cont.tc = lm(tc~BMI, data = newdat)
BMI_cat.tc = lm(tc~BMI_categorical, data = newdat)

summary(BMI_cont.tc)
summary(BMI_cat.tc)
# Comparing to continuous BMI model, categorical BMI model has larger residual standard error, which indicates a higher residual value.
par(mfrow = c(1,1))
plot(newdat$BMI, newdat$tc, main = "Total Cholesterol vs. BMI", xlab = "BMI", ylab = "Total Cholesterol")
lines(newdat$BMI, fitted(BMI_cont.tc), col = "red", lwd = 1.5)
lines(newdat$BMI, fitted(BMI_cat.tc), col = "blue", lwd = 1.5)
legend("topright", c("Fitted regression line for continuous BMI", "Fitted regression line for categorical BMI"), col = c("red", "blue"), lwd = 1)
# I prefer continuous BMI model because it has smaller residual value which means a relatively better fit. When transforming a continuous variable to a categorical variable, there will be loss of information which will leads to the inprecision of the prediction.

# 3(b)
BMI_quad = lm(tc~BMI + I(BMI^2), data = newdat)
summary(BMI_quad)
# Adding a quadratic BMI term makes the model slightly better, R^2 improves from 0.05685 to 0.06691

# 3(c)
BMI_multi = lm(tc ~ age + I(age^2) + gender + BMI + age*gender + I(age^2)*gender + age*BMI + I(age^2)*BMI + gender*BMI + I(BMI^2), data = dat)
summary(BMI_multi)

# R^2 improves by adding the effects of BMI 
plot(resid(BMI_multi) ~ fitted(BMI_multi))
abline(h = 0, col = "red")
qqnorm(resid(BMI_multi))
qqline(resid(BMI_multi))
hist(resid(BMI_multi),prob = TRUE)
m = mean(resid(BMI_multi))
std = sqrt(var(resid(BMI_multi))^2)
curve(dnorm(x,m,std), add = TRUE, col = "red")
# Residuals not normally distributed according to normal Q-Q plot. A few points in the right tail deviate up from reference line, indicating the residuals are slightly right-skewed.
# According to histogram, the residuals are normally distributed except for a few outliers.

# 3(d)
h = hat(model.matrix(BMI_multi))
plot(h, col="blue",
     main="Index plot of leverage", 
     ylab="leverages", xlab="observation number", pch=20)  
abline(h=2*(11)/nrow(dat), col="red", lwd=2)  
length(dat[h > ((2*(11))/nrow(dat)),])

cook <- cooks.distance(BMI_multi)
plot(cook, col="blue",
     main="Index plot of Cook's Distances", 
     ylab="Cook's distances", xlab="observation number", pch=20)
abline(h=(4/(nrow(dat)-2)), col="red", lwd=2)
nrow(dat)
length(dat[cook > 4/(nrow(dat)-2),])
dat[cook > 4/(nrow(dat)-2) & h > ((2*(11))/nrow(dat)),]

newdat2 = dat[!(cook > 4/(nrow(dat)-2) & h > ((2*(11))/nrow(dat))),]
BMI_multi_adj = lm(tc ~ age + I(age^2) + gender + BMI + age*gender + I(age^2)*gender + age*BMI + I(age^2)*BMI + gender*BMI + I(BMI^2), data = newdat2)
summary(BMI_multi_adj)
summary(BMI_multi)
# 23 points have high leverage, and 23 points have high cook's distance. After dropping these observations, the model is improved slightly from a R^2 value of 0.1895 to 0.1908
