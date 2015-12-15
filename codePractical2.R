load('cars.RData')
#install.packages("xtable");
#?install.packages('car')
require(car) 
library(xtable)

# the response y : 100 / City MPG
hundredOverMPG <- rep(100,82) / cars$CityMPG

# the variables
weights <- cars$Weight
HPOverWeight <- cars$Horsepower / cars$Weight

# Look if there is a correlation between our y and the variable we are going to use
#pdf(file="ScatterWeights.pdf", width=6,height=4)
#plot(weights, hundredOverMPG, xlab='Weights', ylab='Gallons per hundred city miles',pch='*')
#dev.off()

#pdf(file="ScatterPower.pdf", width=6,height=4)
#plot(HPOverWeight, hundredOverMPG, xlab='HorsePowerPerWeight', ylab='Gallons per hundred city miles',pch='*')
#dev.off()

# correlations
cor(weights, hundredOverMPG)#0.9007547
cor(HPOverWeight, hundredOverMPG)#0.5976514

# Fit first model in order: Y = b0 + X1*b1 + X2*b2
fit1 = lm(hundredOverMPG ~ weights + HPOverWeight)
fit1_w = lm(hundredOverMPG ~ weights)
# Fit second model in order: Y = b0 + X2*b2 + X1*b1
fit2 = lm(hundredOverMPG ~ HPOverWeight + weights)
fit2_hp = lm(hundredOverMPG ~ HPOverWeight)
# ANOVA tables
anova(fit1)
anova(fit1_w, fit1)
anova(fit2)
anova(fit2_hp, fit2)
#pairs(~FuelCapacity+Length+Wheelbase+Width+Uturn, data=cars, pch='*')

print(xtable(anova(fit1)))
print(xtable(anova(fit2)))

cor(cars$Weight,cars$FuelCapacity)#0.9001302
cor(cars$FuelCapacity,cars$Wheelbase)#0.7934098
cor(cars$FuelCapacity,cars$Uturn)#0.6741421
cor(cars$FuelCapacity,cars$Width)#0.7717577
cor(cars$FuelCapacity,cars$Length)#0.794308

# ------------------- PART 2 : Model Selection --------------------

# On veut maintenant utiliser les variables de 11 à 26 pour créer notre model
newData <- cars[ ,11:26]

FullFit <- lm(hundredOverMPG ~ ., data = newData)
summary(FullFit)

# Latex table
#summaryFullFit <- xtable(summary(FullFit))
#print(summaryFullFit)

# VIF of the full model
library(car)
vif(FullFit)

# ---Model Construction : Backward/Forward selection using AIC/BIC

# AIC, Backward
f1 <- lm(hundredOverMPG ~ ., data = newData)
f1.backward <- step(f1, direction = "backward")
summary(f1.backward)
vif(f1.backward)
# AIC, Forward
f2 <- lm(hundredOverMPG ~ 1, data = newData)
my.scope <- formula(data.frame(hundredOverMPG, newData))
f2.forward <- step(f2, scope = my.scope, direction = "forward", data = newData)
summary(f2.forward)
vif(f2.forward)

# BIC, Backward
f3 <- lm(hundredOverMPG ~ ., data = newData) # Fitting model using all the covariates
f3.backward <- step(f3, direction = "backward", k=log(nrow(newData)))
summary(f3.backward)
vif(f3.backward)
# BIC, Forward
f4 <- lm(hundredOverMPG ~ 1, data = newData) # Fitting the model with only one varialbe, the 1 column
my.scope <- formula(data.frame(hundredOverMPG, newData))
f4.forward <- step(f4, scope = my.scope, direction = "forward", data = newData, k=log(nrow(newData)))
summary(f4.forward)
vif(f4.forward)


# --------------------------------------------------------------------
# ----Model without predictors showing a sign of multicollinearity----

# Scatterplots of variables that are strongly correlated
pairs(~Weight+FuelCapacity+Length+Wheelbase+Width+Uturn, data=cars)

# Nouvelle régression sans ces variables
newData2 = cars[,-c(1,2,3,4,5,6,7,8,9,10,17,19,20,21,22)]
newFit <- lm(hundredOverMPG ~ ., data = newData2)
summary(newFit)

# Latex table
#summaryNewFit <- xtable(summary(newFit))
#print(summaryNewFit)

# VIF of the full model
library(car)
vif(newFit)


# ------ Model Construction : Backward and Forward selection using AIC and BIC

# AIC, Backward
f1 <- lm(hundredOverMPG ~ ., data = newData2)
f1.backward <- step(f1, direction = "backward")
summary(f1.backward)
vif(f1.backward)
# AIC, Forward
f2 <- lm(hundredOverMPG ~ 1, data = newData2)
my.scope <- formula(data.frame(hundredOverMPG, newData2))
f2.forward <- step(f2, scope = my.scope, direction = "forward", data = newData2)
summary(f2.forward)
vif(f2.forward)

# BIC, Backward
f3 <- lm(hundredOverMPG ~ ., data = newData2) # Fitting model using all the covariates
f3.backward <- step(f3, direction = "backward", k=log(nrow(newData2)))
summary(f3.backward)
vif(f3.backward)
# BIC, Forward
f4 <- lm(hundredOverMPG ~ 1, data = newData2) # Fitting the model with only one varialbe, the 1 column
my.scope <- formula(data.frame(hundredOverMPG, newData2))
f4.forward <- step(f4, scope = my.scope, direction = "forward", data = newData2, k=log(nrow(newData2)))
summary(f4.forward)
vif(f4.forward)

# We see that we have two models, one with the BIC ( backward and forward give the same model)
# and one with the AIC

# ---------------------------- DIAGNOSTICS FOR THE TWO MODELS ------------------------------
modelB <- f4.forward

# Confidence interval at 95%
confint(modelB,level=0.95)

# Plot fitted values against standardised residual
pdf(file="fitted.pdf", width=6,height=4)
plot(modelB$fitted.values, rstandard(modelB), xlab="Fitted Values"
     , ylab="Standardised residuals", pch='*')
dev.off()

# QQ plot
pdf(file="qq.pdf", width=6,height=4)
qqnorm(rstandard(modelB),pch='*')
qqline(rstandard(modelB))
dev.off()

# Cook Distance

pdf(file="cook.pdf", width=6,height=4)
plot(cooks.distance(modelB), xlab='Observations', ylab="Cook's distance",
     ylim=c(0,0.2), pch='*')
p <- dim(model.matrix(modelB))[2]
n <- dim(model.matrix(modelB))[1]
abline(8/(n-2*p),0)
dev.off()

pdf(file="hats.pdf", width=6,height=4)
plot(hatvalues(modelB), xlab = "Index", ylab="Hat-values",pch="*")
abline(2*p/n,0)
dev.off()

