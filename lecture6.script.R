
# THE ADVERTISING DATASET

Advertising <- read.csv("Advertising.csv")
attach(Advertising)

# linear regression
plot(TV, sales, pch=20)

mod.out <- lm(sales~TV)

# residuals plot

plot(TV, residuals(mod.out))
abline(h=0)


# two alternative versions of residuals plot 
par(mfrow=c(1,2))
plot(TV, residuals(mod.out))
abline(h=0)

plot(fitted.values(mod.out), residuals(mod.out))
abline(h=0)
par(mfrow=c(1,1))

# diagnostic plots provided by R

plot(mod.out)

par(mfrow=c(2,2))
plot(mod.out)
par(mfrow=c(1,1))

# log-trasform of response

mod.out <- lm(log(sales)~TV)

plot(mod.out)


# otliers

sales.tmp <- sales
sales.tmp[158] <- 5
plot(TV, sales.tmp, pch=16)

mod.out.tmp <- lm(sales.tmp~TV)
abline(mod.out, col="red")
abline(mod.out.tmp)

par(mfrow=c(2,2))
plot(mod.out.tmp)
par(mfrow=c(1,1))

rstandard(mod.out.tmp)[158]

# high leverage point

sales.tmp <- sales
sales.tmp[1] <- 20
TV.tmp <- TV
TV.tmp[1] <- 500

plot(TV.tmp, sales.tmp, pch=16)

mod.out.tmp <- lm(sales.tmp~TV.tmp)
abline(mod.out, col="red")
abline(mod.out.tmp)

plot(mod.out.tmp)



# sample size
n <- length(sales)
n

# check for heteroscedasticity 

mod.out <- lm(sales~TV)
summary(mod.out)

e <- residuals(mod.out)
stud.e <- rstandard(mod.out)

par(mfrow=c(1,2))
plot(TV, e, pch=16)
plot(TV, stud.e, pch=16)
par(mfrow=c(1,1))

plot(TV, sqrt(abs(stud.e)))
lines(loess.smooth(TV, sqrt(abs(stud.e))), col="red")
plot(mod.out)


par(mfrow=c(2,2))
plot(mod.out)
par(mfrow=c(1,1))





################################
# INTERACTION EFFECTS
################################

mod.out <- lm(sales ~ TV + radio + newspaper)
summary(mod.out)

mod.out <- lm(sales ~ TV + radio)
summary(mod.out)


plot(mod.out)

mod.out <- lm(sales ~ TV + radio + TV:radio)
summary(mod.out)
plot(mod.out)

mod.out <- lm(sales ~ TV + radio + TV:radio)
summary(mod.out)


mod.out <- lm(sales ~  TV*radio)


summary(mod.out)
