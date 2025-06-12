# Q1
auto <- read.csv("auto.csv")

# one should make a habit of checking their data is clean before doing any work
View(auto)
summary(auto)
# horsepower is a character??
auto$horsepower <- as.numeric(auto$horsepower)

# a
model <- lm(mpg ~ horsepower, data = auto)
summary(model)

# make a profile of our prediction
profile <- data.frame(horsepower = c(98))
# ?predict.lm
predict(model, profile, interval = "confidence")
predict(model, profile, interval = "prediction")

# b
par(mfrow=c(1,1))
plot(auto$horsepower, auto$mpg)
abline(a = model$coefficients[1], b = model$coefficients[2])

# c
par(mfrow = c(2, 2))
plot(model)













