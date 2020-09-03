############################
#   BST210 LAB 03          #
#   September 19-20, 2019  #
############################

ds1 <- read.csv("lab03_outliers.csv")


# scatter plots, systolic blood pressure (sbp) versus individual covariates
## for smokers
plot(ds1$quet[ds1$smk==1], ds1$sbp[ds1$smk==1], 
     main="Systolic Blood Pressure Versus Body Index (smokers)", 
     ylab="systolic blood pressure (SBP)", xlab="body size (units)", pch=20)
plot(ds1$age[ds1$smk==1], ds1$sbp[ds1$smk==1], 
     main="Systolic Blood Pressure Versus Age (smokers)", 
     ylab="systolic blood pressure (SBP)", xlab="age (years)", pch=20)  
     
## for nonsmokers
plot(ds1$quet[ds1$smk==0], ds1$sbp[ds1$smk==0], 
     main="Systolic Blood Pressure Versus Body Index (nonsmokers)", 
     ylab="systolic blood pressure (SBP)", xlab="body size (units)", pch=20)
plot(ds1$age[ds1$smk==0], ds1$sbp[ds1$smk==0], 
     main="Systolic Blood Pressure Versus Age (nonsmokers)", 
     ylab="systolic blood pressure (SBP)", xlab="age (years)", pch=20)  

# regress sbp on covariates
model1 <- lm(sbp ~ smk + quet + age, data=ds1)
summary(model1)

# generate a new variable named `res', that contains the residuals
res <- residuals(model1)

# generate a new variable named `sbp_hat', that contains the predicted means
sbp_hat <- predict(model1)

# generate diagnostic plots using the residuals
hist(res, col="pink", xlim=c(-20,20), main="Residuals", xlab="residuals")
boxplot(res, col="cornflowerblue", ylab="residual values", main="Residuals")
plot(sbp_hat, res, col="blue",
     main="Residuals versus Predicted Outcomes", 
     ylab="residual value", xlab="predicted outcome (mmHg)", pch=20)  
abline(h=0, col="cornflowerblue", lwd=2)

# generate a new variable named `exStu', that contains externally studentized/jackknife residuals
exStu <- rstudent(model1)

# generate a new variable named `inStu', that contains internally studentized residuals
inStu <- rstandard(model1)

# get MSE for standardized residuals
mse = sum(model1$residuals^2)/model1$df.residual

par(mfrow = c(2, 2))

# create Normal Q-Q plots of the residuals
qqnorm(res, pch=20, col="tomato", main="Unstandardized Residuals")
qqnorm(inStu, pch=20, col = "cornflowerblue", main="Internally Studentized Residuals")
qqnorm(exStu, pch=20, col="turquoise4", main="Externally Studentized Residuals")
qqnorm(res/sqrt(mse), pch=20, col="deeppink", main="Standardized Residuals")

par(mfrow = c(1, 1))

#############################

# generate a new variable named `h', that contains leverage values
h <- hat(model.matrix(model1))
plot(h, col="blue",
     main="Index plot of leverage", 
     ylab="leverages", xlab="observation number", pch=20)  
abline(h=2*(3+1)/nrow(ds1), col="red", lwd=2)  

#list above threshold data points for leverage
ds1[h > ((2*(3+1))/nrow(ds1)),]

################################

# generate a new variable named `cook', that contains Cook's Distance
cook <- cooks.distance(model1)
plot(cook, col="blue",
     main="Index plot of Cook's Distances", 
     ylab="Cook's distances", xlab="observation number", pch=20)
abline(h=(4/(nrow(ds1)-2)), col="red", lwd=2)

#list above threshold data points for cook's d
ds1[cook>(4/(34-2)),]

#################################

# generate a new variable named `dfit', that contains dfits
dfit = dffits(model1)

#list above threshold data points for dffits
ds1[abs(dfit)>(2*sqrt((3+1)/34)),]

#remove observations 8, 9, 34 from the data set
ds2 = ds1[-c(8,9,34),]

#regress again
model2 <- lm(sbp ~ smk + quet + age, data=ds2)
summary(model2)
summary(model1)
