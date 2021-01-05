##### 4. MODEL DATA ######

df <- read.csv("./data/performance-clean.csv",encoding = "UTF-8")

head(df)

# convert response variable to log

df$market.value <- log(df$market.value)


################################# REGRESSION #################################

# df <- df[df$offensive==FALSE,]
# df <- df[df$offensive==TRUE,]

# Simple Linear Regression Model
lm.model <- lm(formula= df$market.value ~
                 df$age+df$offensive+
                 df$contract.expires+df$current.league+
                 df$games.17.18+log(df$assists.17.18+1)+
                 df$games.18.19+df$minutes.18.19+
                 df$games.19.20+
                 df$games.20.21+log(df$goals.20.21+1)+df$minutes.20.21+log(df$assists.20.21+1)
               ,data=df)

summary(lm.model) # ~ Adjusted R-squared: 0.6406

par(mfrow=c(2,2))
plot(lm.model)
par(mfrow=c(1,1))

################################# CLASSIFICATION #################################


# create a new feature to store the label called "price class"
summary(df$market.value)

market.value.stats <- summary(df$market.value)
q25 <- market.value.stats[[2]] # 25%
q50 <- market.value.stats[[3]] # 50%
q75 <- market.value.stats[[5]] # 75%

low.price <- df[df$market.value<q25,]
medium.price <- df[(df$market.value<q50)&(df$market.value>=q25) ,]
high.price <- df[(df$market.value<q75)&(df$market.value>=q50) ,]
super.price <- df[df$market.value>=q75,]

par(mfrow=c(2,2))
hist(low.price$market.value, freq=FALSE)
hist(medium.price$market.value, freq=FALSE)
hist(high.price$market.value, freq=FALSE)
hist(super.price$market.value, freq=FALSE)
par(mfrow=c(1,1))
# check # of elements for each class
dim(low.price)[1]
dim(medium.price)[1]
dim(high.price)[1] # very very few
dim(super.price)[1] # too much
# check it gives back the total
dim(low.price)[1]+dim(medium.price)[1]+dim(high.price)[1]+dim(super.price)[1] - dim(df)[1] # should be 0 (zero)
# create a new column to store the "price.class" feature
df[df$market.value<q25,'price.class']="low"
df[(df$market.value>=q25)&(df$market.value<q50),'price.class']="medium"
df[(df$market.value>=q50)&(df$market.value<q75),'price.class']="high"
df[df$market.value>=q75,'price.class']="super"
# transform to factor 
df$price.class <- as.factor(df$price.class)
plot(df$price.class)
