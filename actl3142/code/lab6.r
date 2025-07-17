library(ISLR2)

####### SCHEDULE
### Q4 -> Q3 -> Q1


### Q1

# a

data <- Default 
View(data)

model <- glm(default ~ income + balance, family=binomial(), data=data)

train_probs <- predict(model, type="response")
train_preds <- ifelse(train_probs > 0.5, "Yes", "No")

# confusion matrix
table(predicted = train_preds, actual = data$default)

# training accuracy
mean(train_preds != data$default)

# b
library(caret)
set.seed(1)

train_idx <- createDataPartition(data$default, p = 0.75, list=F)
train <- data[train_idx,]
test <- data[-train_idx, ]

model.val <- glm(default ~ income+balance, family=binomial(), data=train)
val_probs <- predict(model, type="response", newdata=test)
val_preds <- ifelse(val_probs > 0.5, "Yes", "No")

mean(val_preds != test$default)


### Q4

data <- College
View(data)

# a
train_idx <- createDataPartition(data$Grad.Rate, p = 0.66, list=F)
train <- data[train_idx,]
test <- data[-train_idx, ]

# b
init_model <- lm(Grad.Rate ~ ., data=train)
init_preds <- predict(init_model, newdata=test)

# test error
mean( (init_preds - test$Grad.Rate)^2)

# c
library(glmnet)

# a lot of ML/AI libraries require a specific type to be passed
# into the training call
# X includes covariates, remove Grad.Rate
X_train <- model.matrix(Grad.Rate ~ ., data=train)[, -1]
y_train <- as.matrix(train$Grad.Rate)
X_test <- model.matrix(Grad.Rate ~ ., data=test)[, -1]
y_test <- as.matrix(test$Grad.Rate)

fit.rdg <- glmnet(X_train, y_train, alpha=0)
# plots of coefficient fits
plot(fit.rdg)

pred.naive <- predict(fit.rdg, s=10, newx=X_test)
mean((pred.naive - y_test)^2)

# cv to find optimal lambda
cv.rdg <- cv.glmnet(X_train, y_train, alpha = 0)
plot(cv.rdg)

cv.rdg$lambda.min
pred.cv <- predict(cv.rdg, newx=X_test, s= cv.rdg$lambda.min)
mean((pred.cv - y_test)^2)

# rerun above code with alpha = 1 for lasso





### Q3

# a

set.seed(1)
x <- rnorm(100)
y <- x - 2 * x^2 + rnorm(100)

plot(x, y)

data <- data.frame(y=y, x1=x, x2=x^2, x3=x^3, x4=x^4)

# using a package
library(boot)
# use the cv.glm function
# see ?cv.glm

# its probably a good idea to store these somewhere
# as opposed to printing to console...
fit <- glm(y ~ x1, data=data)
cv.glm(data, fit, K=nrow(data))$delta[1]

fit <- glm(y ~ x1 + x2, data=data)
cv.glm(data, fit, K=nrow(data))$delta[1]

fit <- glm(y ~ x1 + x2 + x3, data=data)
cv.glm(data, fit, K=nrow(data))$delta[1]

fit <- glm(y ~ x1 + x2 + x3 + x4, data=data)
cv.glm(data, fit, K=nrow(data))$delta[1]

# doing it ourselves!
my_awesome_cv_function <- function(data, formula, k = 10) {
  folds <- createFolds(data$y, k=k, list=F)
  
  errors <- c() 
  for (i in seq(1, k)) {
    train <- data[folds != i,]
    val <- data[folds == i,]
    
    model <- lm(formula, train)
    preds_val <- predict(model, val)
    error <- mean((preds_val - val$y)^2)
    
    errors[i] <- error
  }
  return(mean(errors))
}

# its the same as cv.glm !!
my_awesome_cv_function(data=data, formula=y~x1, k=nrow(data))

