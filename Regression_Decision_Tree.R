#DECISION TREE FOR REGRESSION
#predict the value of the Homes
#-----------------------------------------------------------------------------------
library(ISLR)
library(MASS)
#-----------------------------------------------------------------------------------
# Description
# The Boston data frame has 506 rows and 14 columns.
# 
# Usage
# Boston
# Format
# This data frame contains the following columns:
#   
#   crim
# per capita crime rate by town.
# 
# zn
# proportion of residential land zoned for lots over 25,000 sq.ft.
# 
# indus
# proportion of non-retail business acres per town.
# 
# chas
# Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).
# 
# nox
# nitrogen oxides concentration (parts per 10 million).
# 
# rm
# average number of rooms per dwelling.
# 
# age
# proportion of owner-occupied units built prior to 1940.
# 
# dis
# weighted mean of distances to five Boston employment centres.
# 
# rad
# index of accessibility to radial highways.
# 
# tax
# full-value property-tax rate per \$10,000.
# 
# ptratio
# pupil-teacher ratio by town.
# 
# black
# 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town.
# 
# lstat
# lower status of the population (percent).
# 
# medv
# median value of owner-occupied homes in \$1000s.
# 
# Source
# Harrison, D. and Rubinfeld, D.L. (1978) Hedonic prices and the demand for clean air. J. Environ. Economics and Management 5, 81-102.
# 
# Belsley D.A., Kuh, E. and Welsch, R.E. (1980) Regression Diagnostics. Identifying Influential Data and Sources of Collinearity. New York: Wiley.
#-----------------------------------------------------------------------------------

#using SIMPLE TREE with CV and pruning
#-------------------------------------
set.seed(1)
#select a 50:50 training testing sample
train = sample(1:nrow(Boston), nrow(Boston)/2)
#fit the tree
tree.boston=tree(medv~., # model
                 Boston,#data set
                 subset=train) #on the training set
summary(tree.boston)
#view the tree
plot(tree.boston)
text(tree.boston,pretty=0)
#set up cross validation to prune tree to the best length
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size, # size
     cv.boston$dev,#vs RSS
     type='b')
#prune to best fit as per CV
prune.boston=prune.tree(tree.boston,
                        best=7) #using size 7, basically unpruned tree as per CV results
#view (un-)pruned tree
plot(prune.boston)
text(prune.boston,pretty=0)
#evaluate on test data
yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
MSE = mean((yhat-boston.test)^2) #MSE of the model, sqrt of this will give the STD> ERROR of this model

#Bagging & Random Forest
#-----------------------
library(randomForest)
# bagging is simply a special case of a random forest with m = p. 
#Therefore, the randomForest() function cab be used to perform both random forests and bagging.

set.seed(1)

bag.boston=randomForest(medv~., #model
                        data=Boston, #data
                        subset=train, #data subset on training
                        mtry=13,#number of predictors to be considered for each split, here 13 implies all the predictors, hence we will do bagging
                        importance=TRUE)

bag.boston #model
#test model on the test data set
yhat.bag = predict(bag.boston,
                   newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
MSE = mean((yhat.bag-boston.test)^2)

#setting the number of trees
bag.boston=randomForest(medv~.,
                        data=Boston,
                        subset=train,
                        mtry=13,
                        ntree=10)# the number of trees to make
yhat.bag = predict(bag.boston,
                   newdata=Boston[-train,])
MSE_bg = mean((yhat.bag-boston.test)^2)


#RANDOM FOREST
set.seed(1)
rf.boston=randomForest(medv~.,
                       data=Boston,
                       subset=train,
                       mtry=6, # this is smaller
                       importance=TRUE)
#By default, randomForest() uses p/3 variables when building a random forest of regression trees, and sqrt(p) variables when building a random forest of classification trees.

yhat.rf = predict(rf.boston,newdata=Boston[-train,])
MSE_rf = mean((yhat.rf-boston.test)^2)
#measure of importance of the varaiables
importance(rf.boston)
varImpPlot(rf.boston)

#BOOSTING
#---------------------------------------------------------------------------------
library(gbm)

set.seed(1)
boost.boston=gbm(medv~.,
                 data=Boston[train,],
                 distribution="gaussian",
                 n.trees=5000,
                 interaction.depth=4)
#We run gbm() with the option
#distribution="gaussian" since this is a regression problem; if it were a binary classification problem, we would use distribution="bernoulli". 
#n.trees=5000 indicates that we want 5000 trees
#interaction.depth=4 limits the depth of each tree.
summary(boost.boston)


plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")

yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
MSE_bo = mean((yhat.boost-boston.test)^2)

boost.boston=gbm(medv~.,
                 data=Boston[train,],
                 distribution="gaussian",#distribution="gaussian" since this is a regression problem
                 n.trees=5000,#number of trees
                 interaction.depth=4,#depth of each tree
                 shrinkage=0.2,#shrinkage parameter, default is 0.001
                 verbose=F)

yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)