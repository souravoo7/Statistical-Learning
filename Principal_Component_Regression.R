#Principal Component Regression
#------------------------------------------------------------------------------------

library(pls)

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

#read in the data as the ISLR data is faulty
setwd("G:/DATA SC/Statistical_Learning/DATA")
mydata = read.csv("Hitters.csv")
Hitters = mydata

#assess the impact of NA / missing values
na_val = sum(is.na(Hitters$Salary))
Hitters = na.omit(Hitters) #eliminate any row with NA values

Check_na = with(Hitters, sum(is.na(Salary))) # check for NA values post treatment, should be zero

set.seed(2)

pcr.fit = pcr(Salary~., 
              data = Hitters, 
              scale=TRUE, # standardize every predictor
              validation = "CV")#10-fold cross validation for each value of components, say M components

summary(pcr.fit)

validationplot(pcr.fit) #default will be Root MSE
validationplot(pcr.fit, val.type = "MSEP") #getting MSE plot

#TRAIN - TEST Setup

set.seed(1)
train = sample(seq(263), 180, replace = FALSE)
x = model.matrix(Salary~., data = Hitters)[,-1]
y = Hitters$Salary

pcr.fit = pcr(Salary~., 
              data = Hitters,
              subset = train,
              scale=TRUE, # standardize every predictor
              validation = "CV")#10-fold cross validation for each value of components, say M components
validationplot(pcr.fit, val.type = "MSEP") #getting MSE plot

pcr.pred = predict(pcr.fit,
                   x[-train,], 
                   ncomp = 7)

mean((pcr.pred-y[-train])^2)

#fit the entire data using PCR and components = 7
pcr.fit = pcr(y~x,
              scale =TRUE,
              ncomp = 7)
summary(pcr.fit)


#PARTIAL LEAST SQUARES REGRESSION
#---------------------------------------------------------------------

set.seed(1)
pls.fit = plsr(Salary~., 
               data = Hitters,
               subset = train,
               scale = TRUE,
               validation = "CV")

summary(pls.fit)

validationplot(pls.fit, val.type = "MSEP") #getting MSE plot
#test MSE calculation
pls.pred = predict(pls.fit,
                   x[-train,], 
                   ncomp = 2)

mean((pls.pred-y[-train])^2)

#final pls model fit
pls.fit = plsr(Salary~.,
               data = Hitters,
               scale = TRUE,
               ncomp = 2)
summary(pls.fit)
