#linear regression
#library(MASS)
#library(ISLR)


Load_lib = function(){
  library(ISLR)
  library(MASS)
  print("Libraries have been loaded...")
}
Load_lib

#fix(Boston)
names(Boston)
#find out more about the data using ?Boston

lm.fit = lm( medv~lstat,
             data= Boston)
#model output
summary(lm.fit)
estimates = coef(lm.fit) #the parameter estimates
CI = confint(lm.fit) # the confidence interval estimates
# model prediction

p = predict(lm.fit,
        data.frame (lstat = c(5,10,15)),
        interval = "prediction")
#view plots
par(mfrow = c(2,2))
plot(lm.fit)
abline(lm.fit)

#look at the residuals separately
par(mfrow = c(1,2))
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
#calculate the levarage of the data points
plot(hatvalues(lm.fit)) 
max_levarage = which.max(hatvalues(lm.fit))

#multiple linear regression
#adding more variables
lm.fit = lm(medv~lstat+age, 
            data = Boston)

summary(lm.fit)

#Add all the variables

lm.fit = lm(medv~., 
            data = Boston)
summary (lm.fit)
plot(lm.fit)

r_squared = summary(lm.fit)$r.sq
RSE = summary(lm.fit)$sigma

#VIF??

#introduction of interaction terms
inter_model = summary(lm(medv~lstat*age, 
                         data = Boston))
#non-linear transformations in to the model

nonl_model = summary(lm(medv~lstat+I(lstat^2), 
                        data = Boston))
#comparing 2 models:ANOVA
lm.fit_a = lm(medv~lstat, 
              data = Boston)
lm.fit_b = lm(medv~lstat+I(lstat^2), 
              data = Boston)
model_comp = anova(lm.fit_a, lm.fit_b)


#Qualitative predictors
#R automatically Codes the qualitative variable
#fix(Carseats)
lm.fit_car = lm(Sales~.+Income:Advertising+Price:Age,
                data = Carseats)
summary(lm.fit_car)
