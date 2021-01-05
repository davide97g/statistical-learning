##### 3. EXPLORE DATA ######

df <- read.csv("./data/performance-clean.csv",encoding = "UTF-8")

# let's study some distributions

### numeric
# static attributes
hist(df$age)
hist(df$height, breaks = 50)

hist(log(df$market.value))
qqnorm(log(df$market.value))
qqline(log(df$market.value))

hist(df$contract.expires, breaks = 5) # not enough density of the data, maybe to convert to categorical 

# season-related attributes 17/18
par(mfrow=c(2,2))
hist(df$games.17.18, freq = FALSE)
lines(density(df$games.17.18))
hist(df$minutes.17.18, freq = FALSE)
lines(density(df$minutes.17.18))
hist(df$goals.17.18, freq = FALSE)
lines(density(df$goals.17.18))
hist(df$assists.17.18, freq = FALSE)
lines(density(df$assists.17.18))
par(mfrow=c(1,1))

# season-related attributes 18/19
par(mfrow=c(2,2))
hist(df$games.17.18, freq = FALSE)
lines(density(df$games.17.18))
hist(df$minutes.17.18, freq = FALSE)
lines(density(df$minutes.17.18))
hist(df$goals.17.18, freq = FALSE)
lines(density(df$goals.17.18))
hist(df$assists.17.18, freq = FALSE)
lines(density(df$assists.17.18))
par(mfrow=c(1,1))

# season-related attributes 19/20
par(mfrow=c(2,2))
hist(df$games.17.18, freq = FALSE)
lines(density(df$games.17.18))
hist(df$minutes.17.18, freq = FALSE)
lines(density(df$minutes.17.18))
hist(df$goals.17.18, freq = FALSE)
lines(density(df$goals.17.18))
hist(df$assists.17.18, freq = FALSE)
lines(density(df$assists.17.18))
par(mfrow=c(1,1))

# season-related attributes 20/21
par(mfrow=c(2,2))
hist(df$games.17.18, freq = FALSE)
lines(density(df$games.17.18))
hist(df$minutes.17.18, freq = FALSE)
lines(density(df$minutes.17.18))
hist(df$goals.17.18, freq = FALSE)
lines(density(df$goals.17.18))
hist(df$assists.17.18, freq = FALSE)
lines(density(df$assists.17.18))
par(mfrow=c(1,1))

###################################
# Same season, different features #
###################################

# comparing games between seasons
par(mfrow=c(2,3))
hist(df$games.17.18, freq = FALSE)
hist(df$games.18.19, freq = FALSE)
hist(df$games.19.20, freq = FALSE)
qqnorm(df$games.17.18)
qqline(df$games.17.18)
qqnorm(df$games.18.19)
qqline(df$games.18.19)
qqnorm(df$games.19.20)
qqline(df$games.19.20)
par(mfrow=c(1,1))
# GAMES => NORMAL


# comparing goals between seasons
par(mfrow=c(3,3))
hist(df$goals.17.18, freq = FALSE)
hist(df$goals.18.19, freq = FALSE)
hist(df$goals.19.20, freq = FALSE)
# log transformation
df <-  df[df$goals.17.18!=0,]
df <-  df[df$goals.18.19!=0,]
df <-  df[df$goals.19.20!=0,]
hist(log(df$goals.17.18), freq = FALSE)
hist(log(df$goals.18.19), freq = FALSE)
hist(log(df$goals.19.20), freq = FALSE)
# normality check
qqnorm(log(df$goals.17.18))
qqline(log(df$goals.17.18))
qqnorm(log(df$goals.18.19))
qqline(log(df$goals.18.19))
qqnorm(log(df$goals.19.20))
qqline(log(df$goals.19.20))
par(mfrow=c(1,1))
# NOT SO NORMAL!

# comparing assists between seasons
par(mfrow=c(3,3))
hist(df$assists.17.18, freq = FALSE)
hist(df$assists.18.19, freq = FALSE)
hist(df$assists.19.20, freq = FALSE)
# log transformation
df <-  df[df$assists.17.18!=0,]
df <-  df[df$assists.18.19!=0,]
df <-  df[df$assists.19.20!=0,]
hist(log(df$assists.17.18), freq = FALSE)
hist(log(df$assists.18.19), freq = FALSE)
hist(log(df$assists.19.20), freq = FALSE)
# normality check
qqnorm(log(df$assists.17.18))
qqline(log(df$assists.17.18))
qqnorm(log(df$assists.18.19))
qqline(log(df$assists.18.19))
qqnorm(log(df$assists.19.20))
qqline(log(df$assists.19.20))
par(mfrow=c(1,1))
# NOT SO NORMAL!

# comparing minutes between seasons
par(mfrow=c(2,3))
hist(df$minutes.17.18, freq = FALSE)
hist(df$minutes.18.19, freq = FALSE)
hist(df$minutes.19.20, freq = FALSE)
# normality check
qqnorm(df$minutes.17.18)
qqline(df$minutes.17.18)
qqnorm(df$minutes.18.19)
qqline(df$minutes.18.19)
qqnorm(df$minutes.19.20)
qqline(df$minutes.19.20)
par(mfrow=c(1,1))
# NORMAL!

# yellows
par(mfrow=c(2,3))
df <- df[df$yellows.17.18!=0,]
df <- df[df$yellows.18.19!=0,]
df <- df[df$yellows.19.20!=0,]
hist(log(df$yellows.17.18), freq=FALSE)
hist(log(df$yellows.18.19), freq=FALSE)
hist(log(df$yellows.19.20), freq=FALSE)
qqnorm(log(df$yellows.17.18))
qqline(log(df$yellows.17.18))
qqnorm(log(df$yellows.18.19))
qqline(log(df$yellows.18.19))
qqnorm(log(df$yellows.19.20))
qqline(log(df$yellows.19.20))
par(mfrow=c(1,1))

#  compare left/right foot
right.foot <- df[df$foot=="right" ,]
left.foot <- df[df$foot=="left",]
both.foot <- df[df$foot=="both",]

hist(log(df$market.value), freq = FALSE)
lines(density(log(df$market.value)))

par(mfrow=c(2,3))
hist(log(right.foot$market.value), freq = FALSE)
lines(density(log(right.foot$market.value)))
hist(log(left.foot$market.value), freq = FALSE)
lines(density(log(left.foot$market.value)))
hist(log(both.foot$market.value), freq = FALSE)
lines(density(log(both.foot$market.value)))
qqnorm(log(right.foot$market.value))
qqline(log(right.foot$market.value))
qqnorm(log(left.foot$market.value))
qqline(log(left.foot$market.value))
qqnorm(log(both.foot$market.value))
qqline(log(both.foot$market.value))
par(mfrow=c(1,1))

# overlap left/right foot market value distributions
# hist(log(right.foot$market.value), col=rgb(0,0,1,1/4), freq = FALSE)  # first histogram
# hist(log(left.foot$market.value), col=rgb(1,0,0,1/4), add=T, freq = FALSE)  # second
# hist(log(both.foot$market.value), col=rgb(1,0,0,2/4), add=T, freq = FALSE)  # second
plot(density(log(right.foot$market.value)), col=rgb(0,0,1,1/4))  # first histogram
lines(density(log(left.foot$market.value)), col=rgb(1,0,0,1/4))  # second
lines(density(log(both.foot$market.value)), col=rgb(1,0,0,2/4))  # third
# some summaries
summary(right.foot$market.value)
summary(left.foot$market.value)
# hypothesis testing
t.test(right.foot$market.value,left.foot$market.value)
t.test(right.foot$market.value,both.foot$market.value)
t.test(left.foot$market.value,both.foot$market.value)
# p-value = 0.3708 > 0.05 ==> we cannot reject the null hypothesis : the two samples are drawn from the same distribution

### categorical
boxplot(log(df$market.value)~df$offensive) # almost equal with the log transform
hist(as.numeric(df$offensive), freq = FALSE)
plot(df$foot)
