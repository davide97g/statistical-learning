# requirements
install.packages("caTools")
library(caTools)

# read csv
df <- read.csv("./data/performance.csv",encoding = "UTF-8")
print(head(df))

### CLEANING

# filtering NA
df <- df[!is.na(df$age), ]
df <- df[!is.na(df$X19.20.goals), ]
df <- df[!is.na(df$X19.20.minutes), ]

### DISTRIBUTIONS

# extract mu/sigma
mu <- summary(log(df$market.value))[4]
sigma.2 <- var(log(df$market.value))
hist((log(df$market.value)-mu)/sqrt(sigma.2))

# goals distribution
goals <- df$X19.20.goals[df$X19.20.goals != 0 & ! is.na(df$X19.20.goals)]

par(mfrow=c(1,3))

# histogram
hist(log(goals))
# qqplot
qqnorm(log(goals))
qqline(log(goals))
# boxplot with CI's
boxplot(log(goals))

par(mfrow=c(1,1))

# position distribution
plot(df$position)

# full pairwise correlation
# pairs(df)


### MODELS + PREDICTIONS

# train/test split
split = sample.split(df$market.value, SplitRatio = 0.75)
train = subset(df,split==TRUE)
test = subset(df,split==FALSE)

# generate simple glm model
my.model <- glm(market.value~X19.20.goals+age+X19.20.minutes, family="gaussian", data=train)
summary(my.model)

# prediction test
predictTrain = predict(my.model, type="response")

# visualize prediction on train data
par(mfrow=c(1,2))
plot(train$market.value)
plot(predictTrain)
par(mfrow=c(1,2))