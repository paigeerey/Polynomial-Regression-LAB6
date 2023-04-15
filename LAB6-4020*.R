# Paige Reynolds
# MATH 4020
# Lab 6

rm(list= ls())
library(ISLR)

# POLYNOMIAL REGRESSION # 

# Fit polynomial to the wage data
fit <- lm(wage ~ poly(age, 4), data= Wage)
summary(fit)

# Explicit polynomial
coef(lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data= Wage))

# Plot the data, fit and pointwise 95% confidence band
# Makes a grid over the range of age values of length 100
age.grid <- seq(from= range(Wage$age)[1], to= range(Wage$age)[2],
                length= 100) 
preds <- predict(fit, newdata= list(age= age.grid), se= TRUE)
# se= true spits out the standard errors, se(yhat), along with yhat 

se.bands <- cbind(preds$fit + 2*preds$se, preds$fit - 2*preds$se)
par(mar= c(4.5, 4.5, 1, 1), oma= c(0, 0, 4, 0)) # Creates

plot(wage ~ age, data= Wage, xlim= c(min(age.grid), max(age.grid)),
     xlab= "Age", ylab= "Wage", cex= 0.5, col= "darkgrey")
title("Degree-4 Polynomial Fit", outer= TRUE)
lines(age.grid, preds$fit, lwd= 2, col= "blue")
matlines(age.grid, se.bands, lwd= 1, col= "blue", lty= 2)

# CHOOSING THE POLYNOMIAL DEGREE #

# Tests of nested models to find the appropriate polynomial
# create a list to store the lm objects
p.mods <- vector("list", length= 5)
for (i in 1:5){
  p.mods[[i]] <- lm(wage ~ poly(age, i), data= Wage)
} 
anova(p.mods[[1]], p.mods[[2]], p.mods[[3]], p.mods[[4]], p.mods[[5]])

# NONLINEAR LOGISTIC REGRESSION #

(log.fit <- glm(I(wage > 250) ~ poly(age, 4), data= Wage, family= binomial))

logit.preds <- predict(log.fit, newdata= list(age= age.grid), se= TRUE)
# Transform to the scale we want
pfit <- exp(logit.preds$fit)/(1 + exp(logit.preds$fit))
se.bands.logit <- cbind(logit.preds$fit + 2*logit.preds$se.fit,
                        logit.preds$fit - 2*logit.preds$se.fit)
se.bands <- exp(se.bands.logit)/(1+exp(se.bands.logit))

# Get the prob. scale se's directly
preds <- predict(log.fit, newdata= list(age= age.grid), type= "response",
                 se= TRUE)

# Plot the result
plot(x= Wage$age, y= I(Wage$wage > 250), xlim= c(min(age.grid), max(age.grid)),
     type= "n", ylim= c(0, 0.2), xlab= "Age", ylab= "Wage > 250K")
points(jitter(Wage$age), I((Wage$wage > 250)/5), cex= 0.5, pch= "|",
       col= "darkgrey")
lines(age.grid, preds$fit, lwd= 2, col= "blue")
matlines(age.grid, se.bands, lwd= 1, col= "blue", lty= 3)

# STEP FUNCTIONS #

# Fit a step function to the data
table(cut(Wage$age, 4))

cut.fit <- lm(wage ~ cut(age, 4), data= Wage)
coef(summary(cut.fit))

cut.pred <- predict(cut.fit, newdata= list(age= age.grid),
                    se= TRUE)
cut.ses <- cbind(cut.pred$fit + 2*cut.pred$se, cut.pred$fit - 2*cut.pred$se)
par(mar= c(4.5, 4.5, 1, 1), oma= c(0, 0, 4, 0)) # Creates
# 2 panels in 1 row x 2 cols, mar() sets the inner margin and oma sets
# outer margin
plot(wage ~ age, data= Wage, xlim= c(min(age.grid), max(age.grid)),
     xlab= "Age", ylab= "Wage", cex= 0.5, col= "darkgrey")
title("Step Function Fit", outer= TRUE)
lines(age.grid, cut.pred$fit, lwd= 2, col= "blue")
matlines(age.grid, cut.ses, lwd= 1, col= "blue", lty= 2)

# FIT SPLINES #

library(splines)
fit <- lm(wage ~ bs(age ,knots=c(25,40,60) ),data=Wage)
pred <- predict(fit ,newdata =list(age=age.grid),se=T)
plot(Wage$age , Wage$wage ,col="gray")
lines(age.grid ,pred$fit ,lwd=2)
lines(age.grid ,pred$fit +2*pred$se ,lty="dashed")
lines(age.grid ,pred$fit -2*pred$se ,lty="dashed")

dim(bs(Wage$age, knots= c(25, 40, 60)))
dim(bs(Wage$age, df= 6))
attr(bs(Wage$age, df= 6), "knots")

fit2 <- lm(wage ~ ns(age ,df=4),data=Wage)
pred2 <- predict(fit2 ,newdata=list(age=age.grid),se=T)
lines(age.grid, pred2$fit,col="red",lwd=2)

plot(Wage$age, Wage$wage ,xlim= Wage$agelims ,cex =.5,col=" darkgrey ")
title("Smoothing Spline ")
fit <- smooth.spline(Wage$age, Wage$wage ,df=16)
fit2 <- smooth.spline(Wage$age, Wage$wage ,cv=TRUE)
fit2$df

lines(fit, col= "red", lwd= 2)

lines(fit2 ,col= "blue",lwd=2)
legend ("topright", legend=c("16 DF" ,"6.8 DF"),
          col=c("red", "blue"),lty= 1,lwd= 2, cex = .8)

plot(Wage$age, Wage$wage ,xlim= Wage$agelims ,cex =.5,col= "darkgrey")
title("Local Regression")
fit <- loess(wage~age,span= .2,data= Wage)
fit2 <- loess(wage~age, span=.5, data= Wage)
lines(age.grid ,predict(fit, data.frame(age= age.grid)),
        col= "red", lwd= 2)
lines(age.grid, predict(fit2,data.frame(age=age.grid)),
        col="blue",lwd=2)
legend ("topright",legend=c("Span=0.2"," Span=0.5"),
          col=c("red","blue"),lty= 1,lwd= 2, cex = .8)

