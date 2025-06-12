### Q1

# a
# ensure working directory is set correctly
college <- read.csv('College.csv')

# b
# make the row names for our df the name of the colleges, which is the first column
rownames(college) <- college[, 1]
# remove the first column, which is now redundant
college <- college[, -1]
View(college)

# c
summary(college)
# from the summary, it looks like private is just a character column!
# change it to a 'factor' type 
college$Private <- as.factor(college$Private)
# our summary should make more sense now
summary(college)

pairs(college[, 1:10])

plot(college$Private, college$Outstate)

# d
Elite <- rep("No", nrow(college))
Elite[college$Top10perc > 50] <- "Yes"
Elite <- as.factor(Elite)
college <- data.frame(college, Elite)

# only rly care about summary for the Elite column
summary(college$Elite)
plot(college$Elite, college$Outstate)

# e
par(mfrow = c(2,3))
hist(college$Apps, breaks = 20)
hist(college$Accept, breaks = 20)
hist(college$Top10perc, breaks = 20)
hist(college$Top25perc, breaks = 20)
hist(college$Books, breaks = 20)
hist(college$Personal, breaks = 20)

### Q2
auto <- read.csv('Auto.csv')
View(auto)

# ? are missing values
auto <- read.csv("Auto.csv", na.strings = "?")
auto <- na.omit(auto) # remove missing values

summary(auto)

# a
# just google the dataset >>ISLR2 auto dataset
auto$origin <- as.factor(auto$origin)

# b
quant.var <- c(
  "mpg", "cylinders", "displacement", "horsepower",
  "weight", "acceleration", "year"
)
ranges.df <- apply(auto[, quant.var], 2, range) # 2 means apply it to cols
rownames(ranges.df) <- c("min", "max")
ranges.df

# OR
summary(auto[, quant.var])

# c
means.df <- apply(auto[, quant.var], 2, mean)
std.df <- apply(auto[, quant.var], 2, sd)
distns.df <- rbind(means.df, std.df)
rownames(distns.df) <- c("mean", "sd.")
t(distns.df)


# d
# skip

# e
pairs(auto[, -9])










