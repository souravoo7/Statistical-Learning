library(ISLR)

#Data set Caravan:Data Description
names(Caravan)

# Description
# The data contains 5822 real customer records. Each record consists of 86 variables, containing sociodemographic data (variables 1-43) and product ownership (variables 44-86). The sociodemographic data is derived from zip codes. All customers living in areas with the same zip code have the same sociodemographic attributes. Variable 86 (Purchase) indicates whether the customer purchased a caravan insurance policy. Further information on the individual variables can be obtained at http://www.liacs.nl/~putten/library/cc2000/data.html

dim(Caravan)
summary(Caravan$Purchase)
#standardization/regularization of data (bring at the data to a comaprable scale)
standardized.X = scale(Caravan[, -86])
#create the test and training data sets

test = 1:1000
train.X = standardized.X[-test,]
test.X = standardized.X[test,]
train.Y = Caravan$Purchase[-test]
test.Y = Caravan$Purchase[test]

#k=1

set.seed(1)
knn.pred = knn(train.X,
               test.X,
               train.Y,
               k=1)

error_rate = mean(test.Y!=knn.pred)
print(100*error_rate)
table(knn.pred, test.Y)


#k=3

set.seed(1)
knn.pred = knn(train.X,
               test.X,
               train.Y,
               k=3)

error_rate = mean(test.Y!=knn.pred)
print(100*error_rate)
table(knn.pred, test.Y)


#using logistic regression for the same (Yes/No prediction)


glm.fit = glm(Purchase~., data = Caravan, family = binomial, subset = -test)
glm.probs = predict(glm.fit, Caravan[test,], type = "response")

#comparing with 0.5 threshold
glm.pred = rep("No", 1000)
glm.pred[glm.probs>0.5] = "Yes"

table(glm.pred, test.Y)

#comparing with 0.25 threshold
glm.pred = rep("No", 1000)
glm.pred[glm.probs>0.25] = "Yes"

table(glm.pred, test.Y)

