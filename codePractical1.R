# Practical number 1
# Matteo and Joel

load('cars.RData')

# ------------------------- Observation of the DATA --------------------------

# the response y : 100 / City MPG
hundredOverMPG <- rep(100,82) / cars$CityMPG

# the variables
weights <- cars$Weight

HPOverWeight <- cars$Horsepower / cars$Weight

# Look if there is a correlation between our y and the variable we are going to use
plot(weights, hundredOverMPG, xlab='Weights', ylab='100 / City MPG', main = 'Plot of the weights against the fuel efficiency')
plot(HPOverWeight, hundredOverMPG)

# correlations
cor(weights, hundredOverMPG)
cor(HPOverWeight, hundredOverMPG)

# Boxplots of the y and the variables
boxplot(hundredOverMPG, main='Fuel efficiency ( 100 / city MPG )')
boxplot(weights, main='Weights of the cars')
boxplot(HPOverWeight, main='Horsepower over the weight')

# Scatter plot

pairs(cbind(hundredOverMPG, weights, HPOverWeight), main='scatter plot of the y and the variables')


# -------------------------- The Fit -----------------------------

fit = lm(hundredOverMPG ~ weights + HPOverWeight)
summary(fit)
confint(fit)  #  buid confidence intervals

# -------------------------- Fist Analysis -----------------------

#1 --- check for linearity : plot the variables against the standardised residuals
plot(weights, rstandard(fit), xlab='weights', ylab='standardised residuals')
plot(HPOverWeight, rstandard(fit), xlab='Horsepower over weight', ylab='standardised residuals')

#now we plot the fitted y against the standardised residuals
plot(fitted(fit), rstandard(fit), xlab='fitted values', ylab='standardised residuals', ylim=c(-3,3))
# remarque: on voit sur le plot qu il y a une voir 2 valeurs abérantes. Il va falloir les identifier
# homoskedasticity : ok

#2 --- check for normality with QQ plot
qqnorm(rstandard(fit))
qqline(rstandard(fit))

#3 --- Cook Distance, check for outliers and/or leverage points
plot(cooks.distance(fit), xlab='Observations', ylab='Cook distance', main='Cook Distance Plot')
p <- dim(model.matrix(fit))[2]
n <- dim(model.matrix(fit))[1]
abline(8/(n-2*p),0)
identify(cooks.distance(fit),labels=rownames(cars))
# rem: we se that observation 80 and 37 crosses the cook distance

# check for leverage points
plot(hatvalues(fit))
abline(2*p/n,0)
identify(hatvalues(fit), labels=rownames(cars))

plot(weights, hundredOverMPG)
text(weights[37], hundredOverMPG[37], rownames(cars)[37])
text(weights[80], hundredOverMPG[80], rownames(cars)[80])
plot(HPOverWeight, hundredOverMPG)
text(HPOverWeight[37], hundredOverMPG[37], rownames(cars)[37])
text(HPOverWeight[80], hundredOverMPG[80], rownames(cars)[80])

# -------------------------- Second Analysis ( only without one the outliers ) ------------

cars3 <- cars[-c(37),]
hundredOverMPG <- rep(100,81) / cars3$CityMPG
HPOverWeight <- cars3$Horsepower / cars3$Weight
weights <- cars3$Weight

fit3 = lm(hundredOverMPG ~ weights + HPOverWeight)
summary(fit3)
confint(fit3)

plot(fitted(fit3), rstandard(fit3), xlab='fitted values', ylab='standardised residuals', ylim=c(-3,3))

qqnorm(rstandard(fit3))
qqline(rstandard(fit3))

# ------------------

cars4 <- cars[-c(80),]
hundredOverMPG <- rep(100,81) / cars4$CityMPG
HPOverWeight <- cars4$Horsepower / cars4$Weight
weights <- cars4$Weight

fit4 = lm(hundredOverMPG ~ weights + HPOverWeight)
summary(fit4)
confint(fit4)

plot(fitted(fit4), rstandard(fit4), xlab='fitted values', ylab='standardised residuals', ylim=c(-3,3))

qqnorm(rstandard(fit4))
qqline(rstandard(fit4))

# -------------------------- Second Analysis ------------------------

# we now take away the observation 37 and 80
cars2 <- cars[-c(37,80),]

# the response y : 100 / City MPG
hundredOverMPG <- rep(100,80) / cars2$CityMPG

# the variables
weights <- cars2$Weight

HPOverWeight <- cars2$Horsepower / cars2$Weight


fit = lm(hundredOverMPG ~ weights + HPOverWeight)
summary(fit)
confint(fit)

#1 --- check for linearity : plot the variables against the standardised residuals
plot(weights, rstandard(fit), xlab='weights', ylab='standardised residuals')
plot(HPOverWeight, rstandard(fit), xlab='Horsepower over weight', ylab='standardised residuals')

#now we plot the fitted y against the standardised residuals
plot(fitted(fit), rstandard(fit), xlab='fitted values', ylab='standardised residuals', ylim=c(-3,3))
# remarque: on voit sur le plot qu il y a une voir 2 valeurs abérantes. Il va falloir les identifier
# homoskedasticity : ok

#2 --- check for normality with QQ plot
qqnorm(rstandard(fit))
qqline(rstandard(fit))

#3 --- Cook Distance, check for outliers and/or leverage points
plot(cooks.distance(fit), xlab='Observations', ylab='Cook distance', main='Cook Distance Plot', ylim=c(0,0.2))
p <- dim(model.matrix(fit))[2]
n <- dim(model.matrix(fit))[1]
abline(8/(n-2*p),0)

plot(hatvalues(fit))
abline(2*p/n,0)
 identify(hatvalues(fit), labels=rownames(cars2))




