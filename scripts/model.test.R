install.packages("ggplot2")
library(ggplot2)

df <- read.csv("./data/features/Commenti/Commenti1/Dati della tabella.csv", encoding = "UTF-8")  
print(df)

# plot(Impressioni,Le.tue.entrate.stimate..EUR., pch=20)
my.model <- glm(Le.tue.entrate.stimate..EUR.~Visualizzazioni+Impressioni, family=gaussian)
summary(my.model)
# plot(my.model)
# abline(my.model)

### boxplot & outliers
par(mfrow=c(1,2))
visualizzazioni = boxplot(df$Visualizzazioni)
impressioni = boxplot(df$Impressioni)
par(mfrow=c(1,1))

# outliers 
visualizzazioni$out
impressioni$out

# confidence intervals
visualizzazioni$conf
impressioni$conf

# remove outliers
high <- min(visualizzazioni$out)
df <- df[df$Visualizzazioni<high,]
par(mfrow=c(2,2))
hist(log(df$Visualizzazioni))
plot(density(log(df$Visualizzazioni)))
hist(log(df$Impressioni))
plot(density(log(df$Impressioni)))
par(mfrow=c(1,1))


# observe distribution

par(mfrow=c(2,2))
hist(log(df$Impressioni))
hist(log(df$Visualizzazioni))
hist(log(df$Le.tue.entrate.stimate..EUR.))
hist(log(df$N..di.Mi.piace))
par(mfrow=c(1,1))



# test simple model

my.model <- lm(df$Visualizzazioni~df$Iscritti)
summary(my.model)
ggplot(df,aes(Iscritti, Visualizzazioni)) +
  geom_point() +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x)
plot(my.model)


