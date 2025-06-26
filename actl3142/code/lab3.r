library(ISLR2)
data <- Carseats

summary(data)
View(data)

# a
init_model <- lm(Sales ~ Price + Urban + US, data=data)
summary(init_model)

improved_model <- lm(Sales ~ Price + US, data=data)
summary(improved_model)

AIC(init_model)
BIC(init_model)
AIC(improved_model)
BIC(improved_model)

confint(improved_model)

par(mfrow=c(2,2))
plot(improved_model)

# Q4
set.seed(1)
X <- rnorm(100, mean=5, sd=3)
noise <- rnorm(100, mean=0, sd=2)

Y <- 3 + 2*X + 8*X^2 + 7*X^3 + noise

library(leaps)


data <- data.frame(
  Y=Y, X1=X^1, X2=X^2, X3=X^3, X4=X^4, X5=X^5,
  X6=X^6, X7=X^7, X8=X^8, X9=X^9, X10=X^10
)

subsets <- regsubsets(Y ~., data=data, nvmax=10, method="exhaustive")
summary <- summary(subsets)

plot(summary$bic)
summary$bic
summary$cp
summary$adjr2
