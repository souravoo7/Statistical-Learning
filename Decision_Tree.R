
#DECISION TREE FOR CLASSIFICATION

#Fitting a classification tree on the carseats data set
#trying to check if the sales is high or not given other conditions
#--------------------------------------------------------------------------------
library(tree)
library(ISLR)
#--------------------------------------------------------------------------------
# Description
# A simulated data set containing sales of child car seats at 400 different stores.
# 
# Usage
# Carseats
# Format
# A data frame with 400 observations on the following 11 variables.
# 
# Sales
# Unit sales (in thousands) at each location
# 
# CompPrice
# Price charged by competitor at each location
# 
# Income
# Community income level (in thousands of dollars)
# 
# Advertising
# Local advertising budget for company at each location (in thousands of dollars)
# 
# Population
# Population size in region (in thousands)
# 
# Price
# Price company charges for car seats at each site
# 
# ShelveLoc
# A factor with levels Bad, Good and Medium indicating the quality of the shelving location for the car seats at each site
# 
# Age
# Average age of the local population
# 
# Education
# Education level at each location
# 
# Urban
# A factor with levels No and Yes to indicate whether the store is in an urban or rural location
# 
# US
# A factor with levels No and Yes to indicate whether the store is in the US or not
# 
# Source
# Simulated data
# 
# References
# James, G., Witten, D., Hastie, T., and Tibshirani, R. (2013) An Introduction to Statistical Learning with applications in R, www.StatLearning.com, Springer-Verlag, New York

#DATA SET UP:
#-----------------------------------------------------------------------------------

#sales is a continuous varaiable, generating a categorical variable out if it
High = as.factor(ifelse(Carseats$Sales<=8, "Yes", "No"))#this is import for the tree to fit, without as.factor() the classification error cannot be calculated, this is not laid out in the text

Carseats = data.frame(Carseats, High) #merge it back to the main data
#do nnot re-run the merger coomad as new rows will be added
summary(Carseats)
#assess the impact of NA / missing values
na_val = sum(is.na(Carseats$High))
Carseats = na.omit(Carseats) #eliminate any row with NA values
Check_na = with(Carseats, sum(is.na(Carseats$High))) # check for NA values post treatment, should be zero

#TREE FITTING:
#-----------------------------------------------------------------------------------
#use a basic tree model to link High with others
tree.carseats = tree(High ~.-Sales, # model definition 
                     Carseats) #data
summary(tree.carseats)
#plot the tree to view it
plot(tree.carseats)
text(tree.carseats, pretty = 0)

#test-error of the tree
set.seed(2)
train = sample(1:nrow(Carseats), 200)
Carseats.test = Carseats[-train,]
High.test = High[-train]

tree.carseats = tree(High ~.-Sales, # model definition 
                     Carseats,
                     subset = train) #data
tree.pred = predict(tree.carseats, #model 
                    Carseats.test, #test data
                    type ="class" )#type = class for classification problem
#classification results
table(tree.pred, High.test)

#Cross Validation to identify the tree complexity: PRUNING

# We use
# the argument FUN=prune.misclass in order to indicate that we want the
# classification error rate to guide the cross-validation and pruning process,
# rather than the default for the cv.tree() function, which is deviance.


tree.carseats = tree(High ~.-Sales, # model definition 
                     Carseats) #data
set.seed(3)
cv.carseats = cv.tree(tree.carseats, #model object
                      FUN=prune.misclass)#cross validation guide using misclassification guide
cv.carseats

plot(cv.carseats$size, #tree size 
     cv.carseats$dev, #CV error
     type = "b")


plot(cv.carseats$k, #pruning parameter, alpha 
     cv.carseats$dev, #CV error
     type = "b")
#prune the tree
prune.carseats = prune.misclass(tree.carseats, best = 12)
plot(prune.carseats)#view the pruned tree
text(prune.carseats, pretty=0)
#use the pruned tree to make predictions
tree.pred = predict(prune.carseats, #model 
                    Carseats.test, #test data
                    type ="class" )#type = class for classification problem
#classification results
table(tree.pred, High.test)

