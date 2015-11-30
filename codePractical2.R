load('cars.RData')

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
