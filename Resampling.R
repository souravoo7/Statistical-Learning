# Validation Set Approach
library(ISLR)
# 2 part validation (50:50 split)
set.seed(1) # seed decides what sample is drawn
train = sample(392, 196) # select random 196 from the total 392 observations (50:50 split)

#fit a linear model
lm.fit = lm(mpg~horsepower, data = Auto, subset = train)
E1 = (mpg - predict(lm.fit, Auto))[-train]
SE = E1^2
MSE_1 = mean(SE) #the mean squared error

#polynomial regression fits
lm.fit2 = lm(mpg~poly(horsepower,2), data = Auto, subset = train)
lm.fit3 = lm(mpg~poly(horsepower,3), data = Auto, subset = train)

E2 = (mpg - predict(lm.fit2, Auto))[-train]
SE = E2^2
MSE_2 = mean(SE) #the mean squared error

E3 = (mpg - predict(lm.fit3, Auto))[-train]
SE = E3^2
MSE_3 = mean(SE) #the mean squared error

#comapring the MSE of the 3 different order fits
MSE_1
MSE_2
MSE_3


#LOO CV 
library(boot)

glm.fit = glm(mpg~horsepower, data = Auto)
cv.err = cv.glm(Auto, glm.fit)
CV_ERROR = cv.err$delta[1]

cv.error = rep(0,5)
for (i in 1:5){
  glm.fit = glm(mpg~poly(horsepower, i), data = Auto)
  cv.error[i] =cv.glm(Auto, glm.fit)$delta[1]
}
cv.error

#k-FOLD CV
set.seed(17)
cv.error.10 = rep(0,10)
for (i in 1:10){
  glm.fit = glm(mpg~poly(horsepower, i), data= Auto)
  cv.error.10[i] = cv.glm(Auto, glm.fit, K=10)$delta[1]
}
cv.error.10

# Bootstrap Resampling
# Description
# Generate R bootstrap replicates of a statistic applied to data. Both parametric and nonparametric resampling are possible. For the nonparametric bootstrap, possible resampling methods are the ordinary bootstrap, the balanced bootstrap, antithetic resampling, and permutation. For nonparametric multi-sample problems stratified resampling is used: this is specified by including a vector of strata in the call to boot. Importance resampling weights may be specified.
# 
# Usage
# boot(data, statistic, R, sim = "ordinary", stype = c("i", "f", "w"), 
#      strata = rep(1,n), L = NULL, m = 0, weights = NULL, 
#      ran.gen = function(d, p) d, mle = NULL, simple = FALSE, ...,
#      parallel = c("no", "multicore", "snow"),
#      ncpus = getOption("boot.ncpus", 1L), cl = NULL)

attach(Portfolio)
#calculating the concerened statistic
alpha.fn = function(data, index){
  X = data$X[index]
  Y = data$Y[index]
  return((var(Y)-cor(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

fx = alpha.fn(Portfolio, 1:100)
fx

set.seed(1)
fx = alpha.fn(Portfolio, sample(100,100, replace = T))
fx

#using the boot
bootstrap = boot(data = Portfolio, statistic = alpha.fn, R = 1000)
bootstrap


#Bootstraping in Linear Regrssion
#Y = b0 + b1*X
#b0, b1 are the parameters estimated using linear regression
#calculate the SE of bo and b1
#function to calculate the parameters of a LR using the data
boot.fn = function(data, index){
  param = coef(lm(mpg~horsepower, data = data, subset = index))
  return (param)
}

set.seed(1)
bootstrap = boot(Auto, #data set 
                 statistic = boot.fn,#the statistic to be computed from the sample
                 R = 1000)#number of samples
bootstrap

#Std error obtained via boot is more realistic that the lm() summary as that uses normality assumptions and fixed sigma value

#comparing the co-effcient std errors using the lm()

summary = summary(lm(mpg~horsepower, data = Auto))
summary


#A more complex model
#------------------------------------------------------------------------------
#function to calculate the parameters of a LR using the data
boot.fn = function(data, index){
  param = coef(lm(mpg~horsepower+I(horsepower^2), data = data, subset = index))
  return (param)
}

set.seed(1)
bootstrap = boot(Auto, #data set 
                 statistic = boot.fn,#the statistic to be computed from the sample
                 R = 1000)#number of samples
bootstrap

#comparing the co-effcient std errors using the lm()
summary = summary(lm(mpg~horsepower+I(horsepower^2), data = Auto))
summary