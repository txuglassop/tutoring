### Q1

library(ISLR2)
library(caret)

data <- Carseats

# a
train_idx <- createDataPartition(data$Sales, p =0.5, list=F)


train <- data[train_idx,]
test <- data[-train_idx, ]

# b
library(tree)
fit <- tree(Sales ~ ., data = train)
plot(fit)
text(fit, pretty = 0)

pred <- predict(fit, newdata = test)
(test.mse <- mean((test$Sales - pred)^2))

# a better plot
library(rpart)
library(rpart.plot)
fit.rpart <- rpart(Sales ~ ., data = train)
rpart.plot(fit.rpart)



# c
fit <- tree(Sales ~ ., data = train)
fit.cv <- cv.tree(fit, FUN = prune.tree)
fit.cv


# d
library(randomForest)

# bagging is just a random forest with m=p
bag.sales <- randomForest(Sales ~ .,
                          data = train, 
                          mtry = (ncol(Carseats) - 1), importance = TRUE
)
pred <- predict(bag.sales, newdata = test)
(test.mse <- mean((test$Sales - pred)^2))

importance(bag.sales)

# e
rfTestMSE <- rep(Inf, ncol(Carseats) - 1)
for (i in 1:(ncol(Carseats) - 1)) {
  rf.sales <- randomForest(Sales ~ .,
                           data = train, mtry = i,
                           importance = TRUE
  )
  pred <- predict(rf.sales, newdata = test)
  rfTestMSE[i] <- mean((test$Sales - pred)^2)
}
plot(1:(ncol(Carseats) - 1), rfTestMSE, type = "l")

