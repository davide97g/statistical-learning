install.packages("caTools")
library(caTools)

df <- read.csv("./data/performance.csv",encoding = "UTF-8")
print(head(df))

print(df$marketValue)

df <- df[!is.na(df$age), ]
df <- df[!is.na(df$X19.20.goals), ]
df <- df[!is.na(df$X19.20.minutes), ]

length(df$marketValue)

split = sample.split(df$marketValue, SplitRatio = 0.75)

train = subset(df,split==TRUE)
length(train$marketValue)
test = subset(df,split==FALSE)
length(test$marketValue)

my.model <- glm(marketValue~X19.20.goals+age+X19.20.minutes, family="gaussian", data=train)
summary(my.model)

predictTrain = predict(my.model, type="response")
summary(predictTrain)
length(predictTrain)
print(predictTrain)
length(train$marketValue)

par(mfrow=c(1,2))
plot(train$marketValue)
plot(predictTrain)
par(mfrow=c(1,2))

# redictTest = predict(my.model, type = "response", newdata = test)
# table(test$marketValue,predictTest >= 0.3)



mu <- summary(log(df$marketValue))[4]
sigma.2 <- var(log(df$marketValue))
hist((log(df$marketValue)-mu)/sqrt(sigma.2))

goals <- df$X19.20.goals[df$X19.20.goals != 0 & ! is.na(df$X19.20.goals)]
hist(log(goals))
print(goals)

par(mfrow=c(1,2))
qqnorm(log(goals))
qqline(log(goals))
boxplot(log(goals))
par(mfrow=c(1,1))

pairs(df)
plot(df$position)





