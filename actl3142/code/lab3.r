### Q1

library(ISLR2)
data = Carseats
# check if types are all working
summary(data)
View(data)

# a
init_model <- lm(Sales ~ Price + Urban + US, data=data)

# b
summary(init_model)

# bonus Q: what is the base case of our model? i.e. sales when all x = 0?

# e
improved_model <- lm(Sales ~ Price + US, data=data)

# f
summary(improved_model)

# AIC and BIC of our models
# recall lower is better
AIC(init_model)
BIC(init_model)
AIC(improved_model)
BIC(improved_model)

# g
# can also be done by hand
confint(improved_model)

# h
par(mfrow = c(2,2))
plot(improved_model)
par(mfrow=c(1,1))

### Q3

set.seed(1)
x1 <- runif(100)
x2 <- 0.5 * x1 + rnorm(100) / 10
y <- 2 + 2 * x1 + 0.3 * x2 + rnorm(100)

# b
cor(x1, x2)
plot(x1, x2)

# c
model1 <- lm(y ~ x1 + x2)
summary(model1)

# the beta params are very different to the true values!

# d
model2 <- lm(y ~ x1)
summary(model2)

# e
model3 <- lm(y ~ x2)
summary(model3)

# g
set.seed(1)
x1 <- c(x1, 0.1)
x2 <- c(x2, 0.8)
y <- c(y, 6)

# model 1 (full model)
new_model1 <- lm(y ~ x1 + x2)
summary(model1)
summary(new_model1)

par(mfrow=c(2,2))
plot(model1)
plot(new_model1)

# model 2 (just x1)
new_model2 <- lm(y ~ x1)
summary(model2)
summary(new_model2)

plot(model2)
plot(new_model2)

# model 3 (just x2)
new_model3 <- lm(y ~ x2)
summary(model3)
summary(new_model3)

plot(model3)
plot(new_model3)

### Q4

# a
set.seed(1)
X <- rnorm(100, mean=10, sd=3)
noise <- rnorm(100, mean=0, sd=3)

# b
Y <- 3 + 2 * X + 7 * X^2 + 5 * X^3 + noise

# c
# run `install.packages("leaps")` if not done already
library(leaps)

data <- data.frame(
  Y = Y, X1 = X, X2=X^2, X3=X^3, X4=X^4, X5=X^5,
  X6=X^6, X7=X^7, X8=X^8, X9=X^9, X10=X^10
)

model <- regsubsets(Y ~ ., data=data, nvmax=10, method="forward")
summary <- summary(model)
summary

summary$adjr2
summary$cp
summary$bic

# d 
# change the method in the regsubset call to do this


# e
Y = 3 + 4 * X^7 + noise
data <- data.frame(
  Y = Y, X1 = X, X2=X^2, X3=X^3, X4=X^4, X5=X^5,
  X6=X^6, X7=X^7, X8=X^8, X9=X^9, X10=X^10
)

model <- regsubsets(Y ~ ., data=data, nvmax=10, method="exhaustive")
summary <- summary(model)
summary

summary$adjr2
summary$cp
summary$bic
