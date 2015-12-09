load('cars.RData')
install.packages("xtable");
library(xtable)

# the response y : 100 / City MPG
hundredOverMPG <- rep(100,82) / cars$CityMPG

# the variables
weights <- cars$Weight

HPOverWeight <- cars$Horsepower / cars$Weight

# premier model dans l ordre Y = b0 + X1*b1 + X2*b2
fit1 = lm(hundredOverMPG ~ weights + HPOverWeight)
# deuxième model model dans l ordre Y = b0 + X2*b2 + X1*b1
fit2 = lm(hundredOverMPG ~ HPOverWeight + weights)

# Creation des tables d'ANOVA
anova(fit1)
anova(fit2)

# ------------------- PART 2 : Model Selection --------------------

# On veut maintenant utiliser les variables de 11 à 26 pour créer notre model
newData <- cars[ ,11:26]

FullFit <- lm(hundredOverMPG ~ ., data = newData)
summary(FullFit)

# Latex table
summaryFullFit <- xtable(summary(FullFit))
print(summaryFullFit)

# VIF of the full model
library(car)
vif(FullFit)

# ------ Model Construction : Backward and Forward selection using AIC and BIC

# AIC, Backward
f1 <- lm(hundredOverMPG ~ ., data = newData)
f1.backward <- step(f1, direction = "backward")
summary(f1.backward)
vif(f1.backward)
# AIC, Forward
f2 <- lm(hundredOverMPG ~ 1, data = newData)
my.scope <- formula(newData)
f2.forward <- step(f2, scope = my.scope, direction = "forward", data = newData)
summary(f2.forward)
vif(f2.forward)

# BIC, Backward
f3 <- lm(hundredOverMPG ~ ., data = newData) # Fitting model using all the covariates
f3.backward <- step(f3, direction = "backward", k=log(length(newData)))
summary(f3.backward)
vif(f3.backward)
# BIC, Forward
f4 <- lm(hundredOverMPG ~ 1, data = newData) # Fitting the model with only one varialbe, the 1 column
my.scope <- formula(newData)
f4.forward <- step(f4, scope = my.scope, direction = "forward", data = newData, k=log(length(newData)))
summary(f4.forward)
vif(f4.forward)


# --------------------------------------------------------------------------------------
# -------------- Model sans les prédicteurs montrant un signe de multicollinéarité -----

# Scatterplots des variables fortement corrélées
pairs(~Weight+FuelCapacity+Length+Wheelbase+Width+Uturn, data=cars)

# Nouvelle régression sans ces variables
newData2 = cars[,-c(1,2,3,4,5,6,7,8,9,10,17,19,20,21,22)]
newFit <- lm(hundredOverMPG ~ ., data = newData2)
summary(newFit)

# Latex table
summaryNewFit <- xtable(summary(newFit))
print(summaryNewFit)

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
my.scope <- formula(newData2)
f2.forward <- step(f2, scope = my.scope, direction = "forward", data = newData2)
summary(f2.forward)
vif(f2.forward)

# BIC, Backward
f3 <- lm(hundredOverMPG ~ ., data = newData2) # Fitting model using all the covariates
f3.backward <- step(f3, direction = "backward", k=log(length(newData2)))
summary(f3.backward)
vif(f3.backward)
# BIC, Forward
f4 <- lm(hundredOverMPG ~ 1, data = newData2) # Fitting the model with only one varialbe, the 1 column
my.scope <- formula(newData2)
f4.forward <- step(f4, scope = my.scope, direction = "forward", data = newData2, k=log(length(newData2)))
summary(f4.forward)
vif(f4.forward)




