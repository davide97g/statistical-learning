##### 2. CLEAN & FILTER DATA ######
df <- read.csv("./data/performance.csv",encoding = "UTF-8")
print(head(df))
attach(df)

df$market.value = as.numeric(as.character(df$market.value))
# filtering NA
for(col in colnames(df)){
  df <- df[!is.na(df[col]), ]
}
df <- df[df$foot!='',]

# some distributions

### numeric
# static attributes
hist(df$age)
hist(df$height)
# typeof(df$height)
df$market.value[1]
hist(df$market.value)
# hist(df$contract.expires) # need to convert to numeric

# season-related attributes 17/18
par(mfrow=c(2,2))
hist(df$X17.18.games, freq = FALSE)
lines(density(df$X17.18.games))
hist(df$X17.18.minutes, freq = FALSE)
lines(density(df$X17.18.minutes))
hist(df$X17.18.goals, freq = FALSE)
lines(density(df$X17.18.goals))
hist(df$X17.18.assists, freq = FALSE)
lines(density(df$X17.18.assists))
par(mfrow=c(1,1))

# season-related attributes 18/19
par(mfrow=c(2,2))
hist(df$X18.19.games, freq = FALSE)
lines(density(df$X18.19.games))
hist(df$X18.19.minutes, freq = FALSE)
lines(density(df$X18.19.minutes))
hist(df$X18.19.goals, freq = FALSE)
lines(density(df$X18.19.goals))
hist(df$X18.19.assists, freq = FALSE)
lines(density(df$X18.19.assists))
par(mfrow=c(1,1))


# season-related attributes 19/20
par(mfrow=c(2,2))
hist(df$X19.20.games, freq = FALSE)
lines(density(df$X19.20.games))
hist(df$X19.20.minutes, freq = FALSE)
lines(density(df$X19.20.minutes))
hist(df$X19.20.goals, freq = FALSE)
lines(density(df$X19.20.goals))
hist(df$X19.20.assists, freq = FALSE)
lines(density(df$X19.20.assists))
par(mfrow=c(1,1))


# season-related attributes 20/21
#par(mfrow=c(2,2))
#hist(df$X20.21.games, freq = FALSE)
#lines(density(df$X20.21.games))
#hist(df$X20.21.minutes, freq = FALSE)
#lines(density(df$X20.21.minutes))
#hist(df$X20.21.goals, freq = FALSE)
#lines(density(df$X20.21.goals))
#hist(df$X20.21.assists, freq = FALSE)
#lines(density(df$X20.21.assists))
#par(mfrow=c(1,1))

# comparing games between seasons
par(mfrow=c(2,3))
hist(df$X17.18.games, freq = FALSE)
hist(df$X18.19.games, freq = FALSE)
hist(df$X19.20.games, freq = FALSE)
qqnorm(df$X17.18.games)
qqline(df$X17.18.games)
qqnorm(df$X18.19.games)
qqline(df$X18.19.games)
qqnorm(df$X19.20.games)
qqline(df$X19.20.games)
par(mfrow=c(1,1))
# NORMAL


# comparing goals between seasons
par(mfrow=c(3,3))
hist(df$X17.18.goals, freq = FALSE)
hist(df$X18.19.goals, freq = FALSE)
hist(df$X19.20.goals, freq = FALSE)
# log transformation
df <-  df[df$X17.18.goals!=0,]
df <-  df[df$X18.19.goals!=0,]
df <-  df[df$X19.20.goals!=0,]
hist(log(df$X17.18.goals), freq = FALSE)
hist(log(df$X18.19.goals), freq = FALSE)
hist(log(df$X19.20.goals), freq = FALSE)
# normality check
qqnorm(log(df$X17.18.goals))
qqline(log(df$X17.18.goals))
qqnorm(log(df$X18.19.goals))
qqline(log(df$X18.19.goals))
qqnorm(log(df$X19.20.goals))
qqline(log(df$X19.20.goals))
par(mfrow=c(1,1))
# NOT SO NORMAL!

# comparing assists between seasons
par(mfrow=c(3,3))
hist(df$X17.18.assists, freq = FALSE)
hist(df$X18.19.assists, freq = FALSE)
hist(df$X19.20.assists, freq = FALSE)
# log transformation
df <-  df[df$X17.18.assists!=0,]
df <-  df[df$X18.19.assists!=0,]
df <-  df[df$X19.20.assists!=0,]
hist(log(df$X17.18.assists), freq = FALSE)
hist(log(df$X18.19.assists), freq = FALSE)
hist(log(df$X19.20.assists), freq = FALSE)
# normality check
qqnorm(log(df$X17.18.assists))
qqline(log(df$X17.18.assists))
qqnorm(log(df$X18.19.assists))
qqline(log(df$X18.19.assists))
qqnorm(log(df$X19.20.assists))
qqline(log(df$X19.20.assists))
par(mfrow=c(1,1))
# NOT SO NORMAL!

# comparing minutes between seasons
par(mfrow=c(2,3))
hist(df$X17.18.minutes, freq = FALSE)
hist(df$X18.19.minutes, freq = FALSE)
hist(df$X19.20.minutes, freq = FALSE)
# normality check
qqnorm(df$X17.18.minutes)
qqline(df$X17.18.minutes)
qqnorm(df$X18.19.minutes)
qqline(df$X18.19.minutes)
qqnorm(df$X19.20.minutes)
qqline(df$X19.20.minutes)
par(mfrow=c(1,1))
# NORMAL!

# yellows
par(mfrow=c(2,3))
df <- df[df$X17.18.yellows!=0,]
df <- df[df$X18.19.yellows!=0,]
df <- df[df$X19.20.yellows!=0,]
hist(log(df$X17.18.yellows), freq=FALSE)
hist(log(df$X18.19.yellows), freq=FALSE)
hist(log(df$X19.20.yellows), freq=FALSE)
qqnorm(log(df$X17.18.yellows))
qqline(log(df$X17.18.yellows))
qqnorm(log(df$X18.19.yellows))
qqline(log(df$X18.19.yellows))
qqnorm(log(df$X19.20.yellows))
qqline(log(df$X19.20.yellows))
par(mfrow=c(1,1))

#  compare left/right foot
right.foot <- df[df$foot=="right",]
left.foot <- df[df$foot=="left",]
hist(log(df$market.value), freq = FALSE)
lines(density(log(df$market.value)))
par(mfrow=c(2,2))
hist(log(right.foot$market.value), freq = FALSE)
lines(density(log(right.foot$market.value)))
hist(log(left.foot$market.value), freq = FALSE)
lines(density(log(left.foot$market.value)))
qqnorm(log(right.foot$market.value))
qqline(log(right.foot$market.value))
qqnorm(log(left.foot$market.value))
qqline(log(left.foot$market.value))
par(mfrow=c(1,1))

# overlap left/right foot market value distributions
hist(log(right.foot$market.value), col=rgb(0,0,1,1/4), freq = FALSE)  # first histogram
hist(log(left.foot$market.value), col=rgb(1,0,0,1/4), add=T, freq = FALSE)  # second
plot(density(log(right.foot$market.value)), col=rgb(0,0,1,1/4))  # first histogram
lines(density(log(left.foot$market.value)), col=rgb(1,0,0,1/4))  # second
# some summaries
summary(right.foot$market.value)
summary(left.foot$market.value)
# hypothesis testing
t.test(right.foot$market.value,left.foot$market.value)
# p-value = 0.3708 > 0.05 ==> we cannot reject the null hypothesis : the two samples are drawn from the same distribution

### categorical
plot(df$position)
plot(df$foot)
plot(df$current.league)

