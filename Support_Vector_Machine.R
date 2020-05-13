#SUPPORT VECTOR MACHINES
#---------------------------------------------------------------------------------
library(e1071) #contains the SVM function
#---------------------------------------------------------------------------------
#SVM IN A BINANRY CLASSIFICATION SCENARIO
#SVM USING LINEAR KERNELS
#CREATE A RANDOM DATA SET:
#---------------------------------------------------------------------------------

set.seed(1)
x=matrix(rnorm(20*2), ncol=2)
y=c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1
plot(x, col=(3-y))

dat = data.frame(x=x, y=as.factor(y))
svmfit = svm(y~., #y to be classified using others, y ={1, -1}
             data = dat, # data
             kernel = "linear",#linear kernel function
             cost = 10,#the cost parameter, higher cost implies stringent fits
             sacle = FALSE)#scaling of parameters is turned off

plot(svmfit, dat)
SUPPORT_VECTORS = svmfit$index
summary(svmfit)

# > summary(svmfit)
# 
# Call:
#   svm(formula = y ~ ., data = dat, kernel = "linear", cost = 10, 
#       sacle = FALSE)
# 
# 
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  linear 
# cost:  10 
# 
# Number of Support Vectors:  7
# 
# ( 4 3 ) #4 & 3 from each class
# 
# 
# Number of Classes:  2 
# 
# Levels: 
#   -1 1

# Support Vector Classifier with lower cost value: higher number of support vevctors to be found

svmfit = svm(y~.,              #y to be classified using others, y ={1, -1}
             data = dat,       # data
             kernel = "linear",#linear kernel function
             cost = 0.1,       #the cost parameter, higher cost implies stringent fits
             sacle = FALSE)    #scaling of parameters is turned off

plot(svmfit, dat)
SUPPORT_VECTORS = svmfit$index
summary(svmfit)

# the svm() function does not explicitly output the coefficients of
# the linear decision boundary obtained when the support vector classifier is
# fit, nor does it output the width of the margin.

#Cross-Validation to see models of interest

set.seed(1)
tune.out = tune(svm, 
                y~.,
                data=dat,
                kernel = "linear",
                ranges = list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out) #summary of the CV 

#save the best model obtained via cross-validation
bestmod = tune.out$best.model
summary(bestmod)

# > summary(bestmod)
# 
# Call:
#   best.tune(method = svm, train.x = y ~ ., data = dat, 
#             ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 
#                                    100)), kernel = "linear")
# 
# 
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  linear 
# cost:  0.1 #BEST MODEL IS WITH THE COST = 0.1
# 
# Number of Support Vectors:  16
# 
# ( 8 8 )
# 
# 
# Number of Classes:  2 
# 
# Levels: 
#   -1 1

xtest=matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,] + 1
testdat=data.frame(x=xtest, y=as.factor(ytest))
#make prediction using the SVM classifier
ypred=predict(bestmod, # model
              testdat) #data

table(predict=ypred, truth=testdat$y) #table of results

#prediction using a lower cost model: cost = 0.01

svmfit = svm(y~.,              #y to be classified using others, y ={1, -1}
             data = dat,       # data
             kernel = "linear",#linear kernel function
             cost = 0.01,       #the cost parameter, higher cost implies stringent fits
             sacle = FALSE)    #scaling of parameters is turned off
#make prediction using the SVM classifier
ypred=predict(svmfit, # model
              testdat) #data
table(predict=ypred, truth=testdat$y) #table of results

# > table(predict=ypred, truth=testdat$y) #table of results
# truth
# predict -1  1
# -1 11  6
# 1   0  3
# > 14/20
# [1] 0.7 # bad prediction, 70% correct

#CREATE A RANDOM DATA SET: WITH LINEARLY SEPARABLE CLASSES
#---------------------------------------------------------------------------------

x[y==1,]=x[y==1,]+0.5
plot(x, col=(y+5)/2, pch=19)
#HIGH COST SVM
dat=data.frame(x=x,y=as.factor(y))
svmfit=svm(y~., 
           data=dat, 
           kernel="linear", 
           cost=1e5)
summary(svmfit)
plot(svmfit, dat)
#LOW COST SVM
svmfit=svm(y~., 
           data=dat, 
           kernel="linear",
           cost=1)
summary(svmfit)
plot(svmfit,dat)

# Using cost=1, we misclassify a training observation, but we also obtain
# a much wider margin and make use of seven support vectors. It seems
# likely that this model will perform better on test data than the model with
# cost=1e5. 
#HIGHER COST WILL LEAD TO DATA OVERFITTING

#SVM USING NON-LINEAR KERNELS
#CREATE A RANDOM DATA SET:
#---------------------------------------------------------------------------------

set.seed(1)
x=matrix(rnorm(200*2), ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
dat=data.frame(x=x,y=as.factor(y))
plot(x, col=y) # the classification can be done using a radial kernel

train=sample(200,100)# split into random training and testing groups
#fit a radial kernel classification
svmfit = svm(y~.,              #model
             data=dat[train,], #data
             kernel = "radial",#radial kernel
             gamma  = 1,       #kernel paramter
             cost   = 1)       #cost parameter

plot(svmfit, dat[train,])

#fit a radial kernel classification with a higer cost
svmfit = svm(y~.,              #model
             data=dat[train,], #data
             kernel = "radial",#radial kernel
             gamma  = 1,       #kernel paramter
             cost   = 1e5)     #cost parameter (OVERFITTING)

plot(svmfit, dat[train,])

#Cross-Validation to see models of interest

set.seed(1)
tune.out=tune(svm, 
              y~., 
              data=dat[train,], 
              kernel="radial", 
              ranges=list(cost=c(0.1,1,10,100,1000),
              gamma=c(0.5,1,2,3,4)))

summary(tune.out)

table(true=dat[-train,"y"], 
      pred=predict(tune.out$best.model,
                   newdata=dat[-train,]))
#--------------------------------------------------------------------------------
#ROC CURVES
#--------------------------------------------------------------------------------
library(ROCR)

#writing a function to get the prediction and performance
rocplot=function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf,...)}
#SVM FIT USING TRAINING DATA
svmfit.opt=svm(y~.,               #MODEL 
               data=dat[train,],  #DATA
               kernel="radial",   #KERNEL SELECT
               gamma=2,           #GAMMA PARAMETER FOR RADIAL KERNEL
               cost=1,            #COST
               decision.values=T) #***THE FITTED VALUES FROM THE SVM***

#GET THE DESICION VALUES FROM THE SVM
#----------------------------------------------------------------
fitted=attributes(predict(svmfit.opt,
                          dat[train,],
                          decision.values=TRUE))$decision.values
#----------------------------------------------------------------
# par(mfrow=c(1,2))
#PLOT THE ROC CURVE
rocplot(fitted,
        dat[train,"y"],
        main="Training Data")

#fit a more flexible radial kernel gamma = 50
svmfit.flex=svm(y~., 
                data=dat[train,], 
                kernel="radial",
                gamma=50, 
                cost=1, 
                decision.values=T)

fitted=attributes(predict(svmfit.flex,dat[train,],decision.values=T))$decision.values
rocplot(fitted,dat[train,"y"],add=T,col="red")
#ROC PLOT OF gamma = 2 on test data
fitted=attributes(predict(svmfit.opt,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],main="Test Data")
#ROC PLOT OF gamma = 50 on test data
fitted=attributes(predict(svmfit.flex,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],add=T,col="red")


#--------------------------------------------------------------------------------
#SVM IN A MULTI-CLASSIFICATION SCENARIO:
# If the response is a factor containing more than two levels, then the svm()
# function will perform multi-class classification using the one-versus-one approach.

# IF Y VECTOR IS A NUMERIC VECTOR svm() WILL PERFORM SUPPORT VECTOR REGRESSION
#--------------------------------------------------------------------------------

#CREATE A RANDOM DATA SET:
#---------------------------------------------------------------------------------
set.seed(1)
x=rbind(x, matrix(rnorm(50*2), ncol=2))
y=c(y, rep(0,50))
x[y==0,2]=x[y==0,2]+2
dat=data.frame(x=x, y=as.factor(y))
plot(x,col=(y+1))

#SVM FIT USING TRAINING DATA
svmfit=svm(y~.,               #MODEL 
               data=dat,  #DATA
               kernel="radial",   #KERNEL SELECT
               gamma=1,           #GAMMA PARAMETER FOR RADIAL KERNEL
               cost=10,            #COST
               decision.values=T) #***THE FITTED VALUES FROM THE SVM***
plot(svmfit, dat)

#APPLICATION TO GENE EXPRESSION DATA
#----------------------------------------------------------------------------------
library(ISLR)

# Description
# The data consists of a number of tissue samples corresponding to four distinct types of small round blue cell tumors. For each tissue sample, 2308 gene expression measurements are available.
# 
# Usage
# Khan
# Format
# The format is a list containing four components: xtrain, xtest, ytrain, and ytest. xtrain contains the 2308 gene expression values for 63 subjects and ytrain records the corresponding tumor type. ytrain and ytest contain the corresponding testing sample information for a further 20 subjects.
# 
# Source
# This data were originally reported in:
#   
#   Khan J, Wei J, Ringner M, Saal L, Ladanyi M, Westermann F, Berthold F, Schwab M, Antonescu C, Peterson C, and Meltzer P. Classification and diagnostic prediction of cancers using gene expression profiling and artificial neural networks. Nature Medicine, v.7, pp.673-679, 2001.
# 
# The data were also used in:
#   
#   Tibshirani RJ, Hastie T, Narasimhan B, and G. Chu. Diagnosis of Multiple Cancer Types by Shrunken Centroids of Gene Expression. Proceedings of the National Academy of Sciences of the United States of America, v.99(10), pp.6567-6572, May 14, 2002.
# 
# References
# James, G., Witten, D., Hastie, T., and Tibshirani, R. (2013) An Introduction to Statistical Learning with applications in R, www.StatLearning.com, Springer-Verlag, New York
#----------------------------------------------------------------------------------

#WHAT TO DO?
#We will use a support vector approach to predict cancer subtype using gene
# expression measurements. In this data set, there are a very large number
# of features relative to the number of observations. This suggests that we
# should use a linear kernel, because the additional flexibility that will result
# from using a polynomial or radial kernel is unnecessary.
#---------------------------------------------------------------------------------

khan_train_data = data.frame(x=Khan$xtrain, 
                             y=as.factor(Khan$ytrain)) # create the data frame with the feature class value
dim.data.frame(khan_train_data) # structure of the data-set

svm_khan =svm(y~., 
              data=khan_train_data, 
              kernel="linear",
              cost=10)
summary(svm_khan)
# > summary(svm_khan)
# 
# Call:
#   svm(formula = y ~ ., data = khan_train_data, kernel = "linear", 
#       cost = 10)
# 
# 
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  linear 
# cost:  10 
# 
# Number of Support Vectors:  58
# 
# ( 20 20 11 7 )
# 
# 
# Number of Classes:  4 
# 
# Levels: 
#   1 2 3 4

table(svm_khan$fitted, khan_train_data$y) # prediction table

#run the fit on the TEST DATA
khan_test_data=data.frame(x=Khan$xtest, 
                          y=as.factor(Khan$ytest))

pred.te=predict(svm_khan, # using the previous fitted svm kernel
                newdata=khan_test_data) # running classification on test data

table(pred.te, khan_test_data$y) # prediction table