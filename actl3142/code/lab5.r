### Q1

library(insuranceData)

data("dataCar")
data <- dataCar
View(data)

# a
model <- glm(clm ~ veh_value + I(veh_value^2) + I(veh_value^3),
             data=data, family=binomial())

# b
summary(model)

# c

model1 <- glm(clm ~ veh_value,
             data=data, family=binomial())
model2 <- glm(clm ~ veh_value + I(veh_value^2),
             data=data, family=binomial())
model3 <- glm(clm ~ veh_value + I(veh_value^2) + I(veh_value^3),
             data=data, family=binomial())


AIC(model1)
AIC(model2)
AIC(model3)


### Q2

data <- read.csv('Third_party_claims.csv')
View(data)

# a
par(mfrow=c(2,1))
hist(data$claims)
plot(data$accidents, data$claims)

# b
lin_model <- lm(claims ~ accidents, data=data)
par(mfrow=c(2,2))
plot(lin_model)

# c
pois_model <- glm(claims ~ log(accidents), family=poisson(), data=data)
summary(pois_model)
plot(pois_model)

# d
library(MASS)
nb_model <- glm.nb(claims ~ log(accidents), data=data)
summary(nb_model)
plot(nb_model)

# e
qp_model <- glm(claims ~ log(accidents), family=quasipoisson(),data=data)
plot(qp_model)






