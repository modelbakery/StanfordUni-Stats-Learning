library(MASS)
library(ISLR)
### Simple linear regression
names(Boston)
?Boston
plot(medv~lstat,Boston)
fit1=lm(medv~lstat,data=Boston)
fit1
summary(fit1)
abline(fit1,col="red")
names(fit1)
# confidence interval for the fit: here it gives the confidence interval of coefficient 
# and by default, it gives the lower 2.5% and upper 97.5% a balance# for each of those coefficients 
confint(fit1)
# Predict with confidence interval at three points (lstat = c(5,10,15))
predict(fit1,data.frame(lstat=c(5,10,15)),interval="confidence")

predict(fit1,interval="confidence") 

fit2=lm(medv~lstat+age,data=Boston)
summary(fit2)
fit3=lm(medv~.,Boston)
summary(fit3)

par(mfrow=c(2,2))
plot(fit3)
fit4=update(fit3,~.-age-indus)
summary(fit4)
step(fit4)
# Start:  AIC=1585.76

fit4=update(fit3,~.-age-indus +rm*age)
summary(fit4)
step(fit4)
# Step:  AIC=1540.15

### Nonlinear terms and Interactions
fit5=lm(medv~rm*age,Boston)
summary(fit5)
# Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) 36.0885359  1.4698355  24.553  < 2e-16 ***
#   lstat       -1.3921168  0.1674555  -8.313 8.78e-16 ***
#   age         -0.0007209  0.0198792  -0.036   0.9711    
#   lstat:age    0.0041560  0.0018518   2.244   0.0252 *  

# (^2) = quadratic: use I (identity function) to protect!! ---------------------
# The next thing we do here is we fit lstat. And we saw that there was a non-linear looking scatter
# plot between medv and lstat. And so here we explicitly put in a quadratic term. And there's two things going on here.
# The one is we've-- the quadratic we indicate by lstat power two. But power has a meaning in this formula language.
# And so if you want it to mean actually just raise lstat to the power of two, we protect it with this identity function.
# So the formula language doesn't dig inside this identity function.

fit6=lm(medv~lstat +I(lstat^2),Boston); summary(fit6)
attach(Boston)
par(mfrow=c(1,1))
plot(medv~lstat, Boston)
points(Boston$lstat,fitted(fit6),col="red",pch=20)
fit7=lm(medv~poly(lstat,4), Boston)
points(Boston$lstat,fitted(fit7),col="blue",pch=20)
plot(1:20,1:20,pch=1:20,cex=2)

###Qualitative predictors
View(Carseats)
names(Carseats)
summary(Carseats)
fit1=lm(Sales~.+Income:Advertising+Age:Price,Carseats)
summary(fit1)
contrasts(Carseats$ShelveLoc)
###Writing R functions
regplot=function(x,y){
  fit=lm(y~x)
  plot(x,y)
  abline(fit,col="red")
}
attach(Carseats)
regplot(Price,Sales)
regplot=function(x,y,...){
  fit=lm(y~x)
  plot(x,y,...)
  abline(fit,col="red")
}
regplot(Price,Sales,xlab="Price",ylab="Sales",col="blue",pch=20)

