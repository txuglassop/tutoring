### Q1

# load in data
library(ISLR2)
data = Weekly
View(data)

# a
summary(data)
plot(data$Year, data$Volume)

# b
model <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
             family = binomial(), data=data)
summary(model)

# c
# make a confusion matrix
# the fitted values of our model are stored in ..
model$fitted.values
# observe that they are still probabilities: we need to pick a threshold
# to actually classify our observations

# to find out how R is encoding the jumps, see $y and compare to data
model$y

# 1 is up, while 0 is down
# pick a threshold, and classify everything above that as "Up", otherwise "Down"
threshold <- 0.5
preds = rep("Down", length(data$Direction))
preds[model$fitted.values > threshold] <- "Up"

table(preds, actual=data$Direction)

# d
train <- data[data$Year <= 2008, ]
test <- data[data$Year > 2008, ]

model1 <- glm(Direction ~ Lag2, family=binomial(), data=train)
# make sure the type is response - otherwise it will give you log odds!
preds1 <- predict(model1, test, type="response")

threshold <- 0.5
preds1[preds1 > threshold] <- "Up"
preds1[preds1 <= threshold] <- "Down"

table(preds1, actual=test$Direction)




### Q2

data <- Auto
View(data)

# a
mpg01 <- rep(0, nrow(data))
mpg01[data$mpg > median(data$mpg)] <- 1
data$mpg01 <- mpg01

# b
boxplot(data$horsepower ~ data$mpg01)
boxplot(data$weight ~ data$mpg01)

# c
library(caret)
# we want to randomly split our data into training and test set
train_indices <- createDataPartition(data$mpg01, p = 0.75, list=FALSE)
train <- data[train_indices, ]
test <- data[-train_indices, ]

# d
model <- glm(mpg01 ~ cylinders + horsepower + weight + acceleration,
             data=train, family=binomial())

preds <- predict(model, test, type="response")
threshold <- 0.5
preds[preds > threshold] <- 1
preds[preds <= threshold] <- 0

table(preds, actual=test$mpg01)



