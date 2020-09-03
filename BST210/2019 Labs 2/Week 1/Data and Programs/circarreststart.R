# set working directory

setwd("C:/Users/glius/Dropbox/BST 210/Labs (Fall 2017)/Week 1")
dat = read.csv("circarrest.csv",header = T)

# view our data set
View(dat)

# check summary statistics
summary(dat)


# histogram of pdi
# option: prob, number of bins
hist(dat$pdi,prob=T)
# add on a normal overlay - requires a mean/standard deviation
pdi_mean = mean(dat$pdi, na.rm = T)
pdi_std  = sqrt(var(dat$pdi, na.rm = T))
curve(dnorm(x, mean=pdi_mean, sd=pdi_std), 
      col="blue",add=T)


hist(dat$pdi,seq(min(dat$pdi,na.rm = T),max(dat$pdi,na.rm = T),length.out=10))


boxplot(dat$minutes ~ factor(dat$dhca, levels = c(0,1),labels = c("Low Flow", "DHCA")), na.rm = TRUE, 
        xlab = "Procedure",
        ylab = "Minutes of Circulatory Arrest")

# scatter plot 
plot(dat$minutes, dat$pdi, col = dat$dhca + 1)
# loess curve
scatter.smooth(dat$minutes, dat$pdi, col = dat$dhca + 1)

# t-test 
t.test(dat$pdi, mu = 110)
t.test(dat$pdi, mu = 110, alternative = "greater")
t.test(dat$pdi, mu = 110, alternative = "less")

# Pearson's correlation test
cor.test(dat$pdi, dat$minutes)

# two-sample t-test, default in R is to assume unequal variance
t.test(dat$pdi ~ dat$dhca, var.equal = T)

# Linear Regression
mod1 = lm(pdi ~ minutes, data = dat)
summary(mod1)
confint(mod1)

# scatter plot with fitted values
plot(dat$minutes, dat$pdi, col = dat$dhca + 1)
abline(mod1)
