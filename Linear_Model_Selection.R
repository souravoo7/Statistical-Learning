# Baseball Data
# Description
# Major League Baseball Data from the 1986 and 1987 seasons.
# 
# Usage
# Hitters
# Format
# A data frame with 322 observations of major league players on the following 20 variables.
# 
# AtBat
# Number of times at bat in 1986
# 
# Hits
# Number of hits in 1986
# 
# HmRun
# Number of home runs in 1986
# 
# Runs
# Number of runs in 1986
# 
# RBI
# Number of runs batted in in 1986
# 
# Walks
# Number of walks in 1986
# 
# Years
# Number of years in the major leagues
# 
# CAtBat
# Number of times at bat during his career
# 
# CHits
# Number of hits during his career
# 
# CHmRun
# Number of home runs during his career
# 
# CRuns
# Number of runs during his career
# 
# CRBI
# Number of runs batted in during his career
# 
# CWalks
# Number of walks during his career
# 
# League
# A factor with levels A and N indicating player's league at the end of 1986
# 
# Division
# A factor with levels E and W indicating player's division at the end of 1986
# 
# PutOuts
# Number of put outs in 1986
# 
# Assists
# Number of assists in 1986
# 
# Errors
# Number of errors in 1986
# 
# Salary
# 1987 annual salary on opening day in thousands of dollars
# 
# NewLeague
# A factor with levels A and N indicating player's league at the beginning of 1987
# 
# Source
# This dataset was taken from the StatLib library which is maintained at Carnegie Mellon University. This is part of the data that was used in the 1988 ASA Graphics Section Poster Session. The salary data were originally from Sports Illustrated, April 20, 1987. The 1986 and career statistics were obtained from The 1987 Baseball Encyclopedia Update published by Collier Books, Macmillan Publishing Company, New York.
# 
# References
# James, G., Witten, D., Hastie, T., and Tibshirani, R. (2013) An Introduction to Statistical Learning with applications in R, www.StatLearning.com, Springer-Verlag, New York

library(ISLR)

#read in the data as the ISLR data is faulty
setwd("G:/DATA SC/Statistical_Learning/DATA")
mydata = read.csv("Hitters.csv")
Hitters = mydata
attach(Hitters)
summary(Hitters)

#assess the impact of NA / missing values
na_val = sum(is.na(Hitters$Salary))
Hitters = na.omit(Hitters) #eliminate any row with NA values

Check_na = with(Hitters, sum(is.na(Salary))) # check for NA values post treatment, should be zero

#-------------------------------------------------------------------------------
#Best Subset Regression
#--------------------------------------------------------------------------------
library(leaps)
#default is up to 8 variables
regfit.full =regsubsets(Salary~., data = Hitters)
summary(regfit.full)

#fit up to 19 variables
regfit.full =regsubsets(Salary~., data = Hitters, nvmax = 19)
reg.summary = summary(regfit.full)

names(reg.summary)
plot(reg.summary$cp, xlab="Number of Variables", ylab = "C_p")
which.min(reg.summary$cp) # identify the smallest C-P among the variables
#model selection using the Plot method
plot(regfit.full, scale = "Cp")

#co-efficients of the selected regression model
coef(regfit.full, 10)


#other scales
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "bic")

#Forward Step Wise : Greedy Algorithm
#------------------------------------------------------------------------------------
regfit.fwd =regsubsets(Salary~., data = Hitters, nvmax = 19, method = "forward")
reg.summary = summary(regfit.fwd)
plot(regfit.fwd, scale = "Cp")

plot(reg.summary$cp, xlab="Number of Variables", ylab = "C_p")
which.min(reg.summary$cp) # identify the smallest C-P among the variables

#Backward Step Wise : Greedy Algorithm
#------------------------------------------------------------------------------------
regfit.bck =regsubsets(Salary~., data = Hitters, nvmax = 19, method = "backward")
reg.summary = summary(regfit.bck)
plot(regfit.bck, scale = "Cp")

plot(reg.summary$cp, xlab="Number of Variables", ylab = "C_p")
which.min(reg.summary$cp) # identify the smallest C-P among the variables

#Model Selection by Validation
#-----------------------------------------------------------------------------------

dim(Hitters)
set.seed(1)

train = sample(seq(263), 180, replace = FALSE)

train

regfit.fwd =regsubsets(Salary~., data = Hitters[train,], nvmax = 19, method = "forward")
val.errors = rep(19)

#creating a function for the prediction using regsubsets
#there is no in-built model for prediction using the reg subset
x.test = model.matrix(Salary~., data = Hitters[-train,])

for (i in 1:19){
  coefi = coef(regfit.fwd, id=i) #coefficient for the fit with i variables
  pred = x.test[, names(coefi)]%*%coefi #get the predicted value
  val.errors[i] = mean((Hitters$Salary[-train]-pred)^2) #cal the mean squared error
}

plot(sqrt(val.errors), # Root MSE 
     ylab = "Root MSE", 
     ylim = c(300, 400), 
     pch = 19, #given the solid dot on the plot
     type = "b")
#plot the residual sum of squares
points(sqrt(regfit.fwd$rss[-1]/180), 
       col= "blue",
       pch=19,
       type = "b")

#legend 
legend("topright", 
       legend = c("Training", "Validation"), 
       col = c("blue", "black"),
       pch = 19)

#----------------------------------------------------------------
#function call for doing the prediction using the regsubsets
#----------------------------------------------------------------
predict.regsubsets = function(object, newdata, id, ...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id=id)
  mat[, names(coefi)]%*%coefi
}
#--------------------------------------------------------------
#Model Selection 10 fold Cross Validation
#--------------------------------------------------------------

set.seed(11)
folds = sample(rep(1:10, length= nrow(Hitters)))

folds
table(folds)

cv.errors = matrix(NA, 10, 19)

for (k in 1:10){
  best.fit = regsubsets(Salary ~., 
                        data = Hitters[folds != k, ], 
                        nvmax = 19, 
                        method = "forward")
  for (i in 1:19){
    pred = predict(best.fit, 
                   Hitters[folds == k,], 
                   id=i)
    cv.errors[k,i] = mean( (Hitters$Salary[folds == k] - pred)^2 )
  }
  
}

rmse.cv = sqrt(apply(cv.errors, 2, mean))
plot(rmse.cv, pch=19, type = "b")



#Ridge Regression
#--------------------------------------------------------------

library(glmnet)
#need to set up the x and y here, does not accept the model formulation

x = model.matrix(Salary~.-1, data = Hitters)
y = Hitters$Salary

fit.ridge = glmnet(x,y,alpha = 0) # 0 is ridge, 1 is lasso
plot (fit.ridge, xvar = "lambda", label=TRUE)
#cross validation using 10-folds
cv.ridge = cv.glmnet(x,y,alpha=0)

plot(cv.ridge)

#Lasso Regression
#--------------------------------------------------------------
fit.lasso = glmnet(x,
                   y,
                   alpha = 1) # 0 is ridge, 1 is lasso, default is 1
#lambda is automatically chosed

plot (fit.lasso, xvar = "lambda", label=TRUE)
plot (fit.lasso, xvar = "dev", label=TRUE)
#cross validation
cv.lasso = cv.glmnet(x,y,alpha=1)

plot(cv.lasso)
coef(cv.lasso)

#training data set
lasso.tr  = glmnet(x[train,], y[train])
lasso.tr

pred = predict(lasso.tr, x[-train,])
dim(pred)

rmse = sqrt(apply((y[-train]-pred)^2, 2, mean))
plot(log(lasso.tr$lambda), rmse, type = "b", xlab = "log(lambda)")

#chossing the best model
lam.best = lasso.tr$lambda[order(rmse)[1]]

lam.best
coef(lasso.tr, s=lam.best)


#fit lasso for a given set of lambdas
grid = 10^seq(10, -2, length=100)
fit.lasso = glmnet(x,
                   y,
                   alpha = 1,
                   lambda = grid) # 0 is ridge, 1 is lasso, default is 1
#lambda is defined

plot (fit.lasso, xvar = "lambda", label=TRUE)
plot (fit.lasso, xvar = "dev", label=TRUE)
#cross validation
cv.lasso = cv.glmnet(x,y,alpha=1)

plot(cv.lasso)
coef(cv.lasso)
