### Q1

library(ISLR2)
library(boot)

data <- Wage

# a

# cv
max_degree <- 12
cv_errors <- c()

for (i in 1:max_degree) {
  fit <- glm(wage ~ poly(age, i), data=data)
  cv_errors[i] <- cv.glm(data, fit, K=10)$delta[1]
}

plot(1:max_degree, cv_errors, type="l")
which.min(cv_errors)

# anova
model_list <- list()
for (i in 1:max_degree) {
  model_list[[i]] <- glm(wage ~ poly(age, i), data = data)
}

anova_results <- do.call(anova, c(model_list, list(test = "F")))
print(anova_results)

model <- glm(wage ~ poly(age, 4), data=data)
age_grid <- data.frame(age = seq(from = 18, to = 80, length.out = 100))
preds <- predict(model, age_grid)

plot(data$age, data$wage, col = "grey")
lines(age_grid$age, preds, col = "blue")

# b
cv_errors <- c()
max_cuts <- 15
for (i in 2:max_cuts) {
  data$age_cut <- cut(data$age, i) 
  fit <- glm(wage ~ age_cut, data=data)
  cv_errors[i-1] <- cv.glm(data, fit, K=10)$delta[1]
}

plot(2:max_cuts, cv_errors, type="l")
which.min(cv_errors)

# plotting
model <- glm(wage ~ cut(age, 11), data=data)
age_grid <- data.frame(age = seq(from = 18, to = 80, length.out = 100))
preds <- predict(model, age_grid)
plot(data$age, data$wage, col = "grey")
points(age_grid$age, preds, type="l")




##### Q2

data <- Boston

model <- lm(nox ~ poly(dis, 3), data=data)

# plotting
dis_grid <- seq(min(data$dis), max(data$dis), length.out=100)
fitted <- predict(model, newdata= list(dis=dis_grid))
plot(data$dis, data$nox)
lines(dis_grid, fitted, col="red")

# summary
summary(model)


# b
rss <- c()
colours <- c(
  "orange", "red", "lightblue", "green", "brown", "purple",
  "pink", "turquoise", "violet", "magenta", "darkblue"
)

plot(data$dis, data$nox)
for (i in 1:10) {
  model <- lm(nox ~ poly(dis, i), data=data)
  rss[i] <- sum(model$residuals^2)
  fitted <- predict(model, newdata=list(dis=dis_grid))
  lines(dis_grid, fitted, col=colours[i])
}

rss


##skip c, since we did this in Q1

# d
library(splines)

model <- lm(nox ~ bs(dis, df=4), data=data)
fitted <- predict(model, newdata=list(dis=dis_grid))
plot(data$dis, data$nox)
lines(dis_grid, fitted, col="red")


# e
rss <- c()

plot(Boston$dis, Boston$nox)
for (i in 4:11) {
  model <- lm(nox ~ bs(dis, df = i), data = Boston)
  rss <- c(rss, sum(model$residuals^2))
  fitted <- predict(model, newdata = list(dis = dis_grid))
  lines(dis_grid, fitted, col = colours[i])
}

rss


# f
cv_errors <- c()
options(warn=-1)
for (i in 3:11) {
  fit <- glm(nox ~ bs(dis, df = i), data = data)
  cv <- cv.glm(Boston, fit, K = 10)$delta[1]
  cv_errors <- c(cv_errors, cv)
}
options(warn=0)  # reset to default
plot(3:11, cv_errors, type = "l", xlab = "df", lwd=2, col="brown3")
