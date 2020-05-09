#NON-LINEAR MODELLING
#-----------------------------------------------------------------------------------------

library(ISLR)

# Mid-Atlantic Wage Data
# Description
# Wage and other data for a group of 3000 male workers in the Mid-Atlantic region.
# 
# Usage
# Wage
# Format
# A data frame with 3000 observations on the following 11 variables.
# 
# year
# Year that wage information was recorded
# 
# age
# Age of worker
# 
# maritl
# A factor with levels 1. Never Married 2. Married 3. Widowed 4. Divorced and 5. Separated indicating marital status
# 
# race
# A factor with levels 1. White 2. Black 3. Asian and 4. Other indicating race
# 
# education
# A factor with levels 1. < HS Grad 2. HS Grad 3. Some College 4. College Grad and 5. Advanced Degree indicating education level
# 
# region
# Region of the country (mid-atlantic only)
# 
# jobclass
# A factor with levels 1. Industrial and 2. Information indicating type of job
# 
# health
# A factor with levels 1. <=Good and 2. >=Very Good indicating health level of worker
# 
# health_ins
# A factor with levels 1. Yes and 2. No indicating whether worker has health insurance
# 
# logwage
# Log of workers wage
# 
# wage
# Workers raw wage

#------------------------------------------------------------------------------------
#Polynomial Regression:
#---------------------
#general orthogonal polynomial fit
fit = lm(wage~poly(age, 4), data = Wage)
summary(fit)

agelims = range(Wage$age) # get the range of age values
age.grid = seq(from = agelims[1], to= agelims[2]) # create a sequence of age values from low to hig limit

preds = predict(fit, #the model to use for prediction
                newdata = list(age = age.grid),#data for prediction
                se = TRUE)#get the standard errors
se.bands = cbind(preds$fit+2*preds$se.fit, #+2SE 
                 preds$fit-2*preds$se.fit )#-2SE

#second model with the raw model parameters fitted
fit2 = lm(wage~poly(age, 4, raw = T), data = Wage)
summary(fit2)

preds2 = predict(fit2, #the model to use for prediction
                newdata = list(age = age.grid),#data for prediction
                se = TRUE)#get the standard errors
se.bands2 = cbind(preds2$fit+2*preds2$se.fit, #+2SE 
                 preds2$fit-2*preds2$se.fit )#-2SE

#plot the predictions and the standard errors
#plot the first model
par(mfrow = c(1,2), 
    mar=c(4.5, 4.5,1,1), 
    oma=c(0,0,4,0))
plot(Wage$age, 
     Wage$wage, 
     xlim = agelims, 
     cex = 0.5, 
     col='darkgrey')
title("Degree-4 Polynomial", outer = T)
lines(age.grid,
      preds$fit,
      lwd=2,
      col="blue")
matlines(age.grid,
      se.bands,
      lwd=1,
      col="blue",
      lty=3)
#plot the second model
plot(Wage$age, 
     Wage$wage, 
     xlim = agelims, 
     cex = 0.5, 
     col='darkgrey')
lines(age.grid,
      preds2$fit,
      lwd=2,
      col="blue")#
matlines(age.grid,
         se.bands2,
         lwd=1,
         col="blue",
         lty=3)
#decide the degree of the polynomial
#compare the 5 degree polynomials from 1-5
fit.1 = lm(wage~age, data = Wage)
fit.2 = lm(wage~poly(age,2), data = Wage)
fit.3 = lm(wage~poly(age,3), data = Wage)
fit.4 = lm(wage~poly(age,4), data = Wage)
fit.5 = lm(wage~poly(age,5), data = Wage)
#ANOVA TO COMPARE THE FITS
anova(fit.1, fit.2, fit.3, fit.4, fit.5)
#the models are nested, ie model 2 contains all of model 1 and so on

#Polynomial Logistic Regression:
#------------------------------
fit=glm(I(wage>250)~poly(age, 4),
        data = Wage,
        family = 'binomial')

preds = predict(fit, #the model to use for prediction
                newdata = list(age = age.grid),#data for prediction
                se = TRUE)#get the standard errors
#need to transform the prediction to logit
pfit = exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit = cbind(preds$fit+2*preds$se.fit, #+2SE 
                 preds$fit-2*preds$se.fit )#-2SE
se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))

#plot the results
plot(Wage$age,I(Wage$wage>250),xlim=agelims,type="n",ylim=c(0,.2))
points(jitter(Wage$age), I((Wage$wage>250)/5),cex=.5,pch="|",col="darkgrey")
lines(age.grid,pfit,lwd=2, col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)

#Step function fitting
table(cut(Wage$age,4))
fit=lm(wage~cut(age,4),data=Wage)
coef(summary(fit))
#------------------------------------------------------------------------------------
#Splines 
#--------------------------
library(splines)
#using basis functions
fit=lm(wage~bs(age,knots=c(25,40,60)),#using the basis function with specified knots
       data=Wage)
#by default cubic splines are produced
#pre specified 3 knots 
#leading to K+4 = 7 degrees of freedom
#1 interecept + 6 polynomials
pred=predict(fit,newdata=list(age=age.grid),se=T)
plot(Wage$age, Wage$wage, col='gray')
lines(age.grid,pred$fit,lwd=2)
lines(age.grid,pred$fit+2*pred$se,lty="dashed")
lines(age.grid,pred$fit-2*pred$se,lty="dashed")

#using the basis function with automatic knots as per percentiles
fit=lm(wage~bs(age,df=6), #df = 6 for 6 basis functions
       data=Wage)
#by default cubic splines are produced
#pre specified 3 knots 
#leading to K+4 = 7 degrees of freedom
#1 interecept + 6 polynomials
pred=predict(fit,newdata=list(age=age.grid),se=T)
plot(Wage$age, Wage$wage, col='gray')
lines(age.grid,pred$fit,lwd=2)
lines(age.grid,pred$fit+2*pred$se,lty="dashed")
lines(age.grid,pred$fit-2*pred$se,lty="dashed")

#fitting a natural spline
fit2=lm(wage~ns(age,df=4), # natural spline function
        data=Wage)
pred2=predict(fit2,newdata=list(age=age.grid),se=T)
lines(age.grid, pred2$fit,col="red",lwd=2)

#Smoothing SPLINE
#------------------------------------------------------------------------------------
plot(Wage$age, 
     Wage$wage, 
     xlim = agelims, 
     cex = 0.5, 
     col='darkgrey')

title("Smoothing Spline")

fit=smooth.spline(Wage$age,Wage$wage,df=16) # the DF = 16 is pre-specified
fit2=smooth.spline(Wage$age,Wage$wage,cv=TRUE) # using LOOCV to determine the DF
#plot both on the data to vizualize
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend("topright",legend=c("16 DF","6.8 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)

#Local Regression
#------------------------------------------------------------------------------------
plot(Wage$age, 
     Wage$wage, 
     xlim = agelims, 
     cex = 0.5, 
     col='darkgrey')

title("Local Regression")
#use the loess function to get the local regression fit
fit = loess(wage~age, # model 
            span = 0.2, #span parameter: 0.2 ~20% of the observations
            data= Wage)
fit2 = loess(wage~age, span = 0.5, data= Wage)

#plot is not saimple as in case of splines
lines(age.grid,
      predict(fit,data.frame(age=age.grid)),
      col="red",
      lwd=2)
lines(age.grid,predict(fit2,data.frame(age=age.grid)),col="blue",lwd=2)
legend("topright",
       legend=c("Span=0.2","Span=0.5"),
       col=c("red","blue"),
       lty=1,lwd=2,cex=.8)
#------------------------------------------------------------------------------------
#GAM:General Additive Models
#-----------------------------------------------------------------------------------
#natural splines
#smoothing splines
#linear variables
#categorical variables
#local regression
gam1 = lm(wage~ns(year, 4)+ns(age, 5)+ education, #ns function for natural spline with k degress of freedom 
          data = Wage)

library(gam)# generalized additive models
gam.m3 = gam(wage~s(year, 4)+s(age, 5)+education, #s function for smoothing spline with k degrees of freedom 
             data=Wage)
plot(gam.m3, se = TRUE, col='blue')
#plot.gam(gam1, se = TRUE, col='red')

gam.m1=gam(wage~s(age,5)+education,data=Wage)
gam.m2=gam(wage~year+s(age,5)+education,data=Wage)
#compare the 3 models using the ANOVA
anova(gam.m1,gam.m2,gam.m3,test="F")
summary(gam.m3)

#prediction using GAM
preds = predict(gam.m2, newdata = Wage)
#adding local regrssion terms in GAM
gam.lo = gam(wage~s(year, df=4)+lo(age, span = 0.7)+education, 
             data=Wage)
summary(gam.lo)

library(akima) #for plotting 2-D surfaces

gam.lo.i = gam(wage~lo(year, age, span = 0.5)+education, # local regression using an interaction of year & age 
               data = Wage)
plot(gam.lo.i)


#Logistic Regression using GAM
#----------------------------------------------------------------------------------

attach(Wage)
gam.lr = gam(I(wage>250)~year+s(age, df= 4)+education, # model definition 
             data=Wage,
             family = binomial)

plot(gam.lr,se=T,col="green")
table(education,I(wage>250))

gam.lr.s=gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage,subset=(education!="1. < HS Grad"))
plot(gam.lr.s,se=T,col="green")

summary(gam.lr.s)