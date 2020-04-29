#The classification problem (binary)

library(ISLR)
library(MASS)
names(Smarket)

#?Smarket
# #S&P Stock Market Data
# Description
# Daily percentage returns for the S&P 500 stock index between 2001 and 2005.
# 
# Usage
# Smarket
# Format
# A data frame with 1250 observations on the following 9 variables.
# 
# Year
# The year that the observation was recorded
# 
# Lag1
# Percentage return for previous day
# 
# Lag2
# Percentage return for 2 days previous
# 
# Lag3
# Percentage return for 3 days previous
# 
# Lag4
# Percentage return for 4 days previous
# 
# Lag5
# Percentage return for 5 days previous
# 
# Volume
# Volume of shares traded (number of daily shares traded in billions)
# 
# Today
# Percentage return for today
# 
# Direction
# A factor with levels Down and Up indicating whether the market had a positive or negative return on a given day


#Data Understanding
summary(Smarket)
cor_mat = cor(Smarket[,-9]) #look at the correlation matrix
attach(Smarket)
plot(Volume)

#Problem to predict Direction as Up or Down (a Binary Classification problem)
#Using the logistic regression
# Preidictors: 1. Lag 1 to Lag 5
#              2. Volumes
# Other variables ignored

glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
              data = Smarket,
              family = binomial)

summary(glm.fit)
coef(glm.fit) #access the co-efficients

#prediction using the logit model

glm.probs = predict(glm.fit, type = "response")
glm.probs[1:10]
plot(glm.probs)

contrasts(Direction) # looks the dummy variable function- value corresponding to 1 is the reported probability, in this case "up" is encpded as 1, hence we get the probability of going up

#-------------------------------------------------------------------------------
#set the threshold here for the classification
glm.pred = rep("Down", 1250)
glm.pred[glm.probs>0.5] = "Up" #0.5 is the default point here

#confusion matrix
table(glm.pred, Direction)
print( mean(glm.pred == Direction)) #logical operator to check for the percentage of correct predictions

#validation/hold-out data set

train = (Year<2005)
Smarket.2005 = Smarket[!train, ] #exclude observations prior to 2005
Direction.2005 = Direction[!train]
dim(Smarket.2005)
dim(Direction.2005)

glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
              data = Smarket,
              family = binomial,
              subset = train)
glm.probs = predict(glm.fit, Smarket.2005, type = "response")
glm.pred = rep("Down", dim(Smarket.2005) [1])
glm.pred[glm.probs>0.5] = "Up" #0.5 is the default point here
table(glm.pred, Direction.2005)
print( mean(glm.pred == Direction.2005)) #logical operator to check for the percentage of correct predictions

#modify the model by removing the unrelated variables

glm.fit = glm(Direction~Lag1+Lag2,
              data = Smarket,
              family = binomial,
              subset = train)
glm.probs = predict(glm.fit, Smarket.2005, type = "response")
glm.pred = rep("Down", dim(Smarket.2005) [1])
glm.pred[glm.probs>0.5] = "Up" #0.5 is the default point here
table(glm.pred, Direction.2005)
print( mean(glm.pred != Direction.2005)) #logical operator to check for the percentage of correct predictions

#using the LDA set up

lda.fit = lda(Direction~Lag1+Lag2,
              data = Smarket,
              subset = train)
summary(lda.fit)
print(lda.fit) # the LDA coefficents K = A*Lag1+B*Lag2 is used for classification

lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class = lda.pred$class
table(lda.class, Direction.2005)
print( mean(glm.pred != Direction.2005))

decrease = sum(lda.pred$posterior[,1]>=0.5) #probabilities calculated are decrease probabilities
increase = sum(lda.pred$posterior[,1]<0.5) # the threshold is 50%


#QDA:Quadratic Discriminant Analysis
qda.fit = qda(Direction~Lag1+Lag2,
              data = Smarket,
              subset = train)

print(qda.fit)
qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
print( mean(qda.class != Direction.2005))#error rate



#USING KNN (K-Nearest Neighbours)

library(class)

train.X = cbind(Lag1, Lag2)[train,]
test.X  = cbind(Lag1, Lag2)[!train,]

train.Direction = Direction[train]

#using KNN with K=1

set.seed(1)
knn.pred = knn(train.X,
               test.X,
               train.Direction,
               k=1)
table(knn.pred, Direction.2005)
error_rate_k1 = mean(knn.pred!=Direction.2005)

#using KNN with K=3
knn.pred = knn(train.X,
               test.X,
               train.Direction,
               k=3)
table(knn.pred, Direction.2005)

error_rate_k3 = mean(knn.pred!=Direction.2005)