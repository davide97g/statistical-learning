# requirements
install.packages("caTools")
library(caTools)

# read csv
df <- read.csv("./data/performance.csv",encoding = "UTF-8")
print(head(df))

# convert market.value to numeric
df$market.value = as.numeric(as.character(df$market.value))


### MODELS + PREDICTIONS

# train/test split
split = sample.split(df$market.value, SplitRatio = 0.75)
train = subset(df,split==TRUE)
test = subset(df,split==FALSE)

# remove NA
for(col in colnames(df)){
  df <- df[!is.na(df[col]), ]
}

# generate simple lm model
hist(X19.20.minutes, freq = FALSE)
my.model <- lm(log(market.value) ~ log(X20.21.goals+1) + X20.21.minutes +log(X19.20.goals+1)+age+X19.20.minutes
                  +position+log(X18.19.goals+1)+X18.19.minutes+
                   log(X17.18.goals+1)+X17.18.minutes+current.league+contract.expires, data=train)
summary(my.model)

# visualize model results
par(mfrow=c(2,2))
plot(my.model)
par(mfrow=c(1,1))

