##### 4. MODEL DATA ######

df <- read.csv("./data/performance-clean.csv",encoding = "UTF-8")

head(df)

# convert response variable to log

df$market.value <- log(df$market.value)



### CREATE TRAIN/TEST SPLIT ###

library(caTools)
library(caret)

set.seed(88)

split = sample.split(df$market.value, SplitRatio = 0.9)

training = subset(df, split == TRUE)
testing = subset(df, split == FALSE)

################################# REGRESSION #################################

# df <- df[df$offensive==FALSE,]
# df <- df[df$offensive==TRUE,]

# Simple Linear Regression Model
lm.model <- lm(formula= market.value ~
                 age+offensive+sub.position+
                 contract.expires+current.league+
                 games.17.18+log(assists.17.18+1)+
                 games.18.19+minutes.18.19+
                 games.19.20+
                 games.20.21+log(goals.20.21+1)+minutes.20.21+log(assists.20.21+1)
               ,data=training)
# lm.model <- lm(formula= market.value ~.-name,data=training)

summary(lm.model) # ~ Adjusted R-squared: 0.6406

par(mfrow=c(2,2))
plot(lm.model)
par(mfrow=c(1,1))

################################# CLASSIFICATION #################################


# create a new feature to store the label called "price class"
summary(training$market.value)

market.value.stats <- summary(training$market.value)
q25 <- market.value.stats[[2]] # 25%
q50 <- market.value.stats[[3]] # 50%
q75 <- market.value.stats[[5]] # 75%

low.price <- training[training$market.value<q25,]
medium.price <- training[(training$market.value<q50)&(training$market.value>=q25) ,]
high.price <- training[(training$market.value<q75)&(training$market.value>=q50) ,]
super.price <- training[training$market.value>=q75,]

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
dim(low.price)[1]+dim(medium.price)[1]+dim(high.price)[1]+dim(super.price)[1] - dim(training)[1] # should be 0 (zero)

# create a new column to store the "price.class" feature
training[training$market.value<q25,'price.class']=0
training[(training$market.value>=q25)&(training$market.value<q50),'price.class']=1
training[(training$market.value>=q50)&(training$market.value<q75),'price.class']=2
training[training$market.value>=q75,'price.class']=3
# transform to factor 
training$price.class <- as.factor(training$price.class)
# visual check 
plot(training$price.class)

## use the predicted values of the model and save a new feature to the model
testing['market.value.predicted'] <- predict(lm.model, newdata = testing)
par(mfrow=c(1,2))
hist(testing$market.value)
hist(testing$market.value.predicted)
par(mfrow=c(1,1))

summary(testing$market.value)
summary(testing$market.value.predicted)

# create the price class feature for the test set
testing[testing$market.value<q25,'price.class']=0
testing[(testing$market.value>=q25)&(testing$market.value<q50),'price.class']=1
testing[(testing$market.value>=q50)&(testing$market.value<q75),'price.class']=2
testing[testing$market.value>=q75,'price.class']=3
# transform to factor 
testing$price.class <- as.factor(testing$price.class)
# visual check 
plot(testing$price.class)

## use the predicted values of the model and save a new price class predicted feature
testing[testing$market.value.predicted<q25,'price.class.predicted']=0
testing[(testing$market.value.predicted>=q25)&(testing$market.value.predicted<q50),'price.class.predicted']=1
testing[(testing$market.value.predicted>=q50)&(testing$market.value.predicted<q75),'price.class.predicted']=2
testing[testing$market.value.predicted>=q75,'price.class.predicted']=3
# transform to factor 
testing$price.class.predicted <- as.factor(testing$price.class.predicted)
# visual check 
plot(testing$price.class)

### build the confusion matrix by hand
price.class <- c(0,1,2,3)
confusion.matrix <- matrix(0,nrow=4,ncol=4)
warnings()
for(p1 in price.class){
  print(paste("p1",p1))
  for(p2 in price.class){
    print(paste("p2",p2))
    x <- dim(testing[(testing$price.class==p1) & (testing$price.class.predicted==p2),])[1]
    print(x)
    confusion.matrix[p1+1,p2+1] <- x
  }
}
confusion.matrix
sum(diag(confusion.matrix))/sum(confusion.matrix)
########## LINEAR DISCRIMINANT ANALISYS

lda.model <- lda(price.class~
                   age+offensive+
                   contract.expires+current.league+
                   games.17.18+log(assists.17.18+1)+
                   games.18.19+minutes.18.19+
                   games.19.20+
                   games.20.21+log(goals.20.21+1)+minutes.20.21+log(assists.20.21+1), 
                 data = training)
lda.model



p1 <- predict(lda.model, training)$class
tab <- table(Predicted = p1, Actual = training$price.class)
tab
sum(diag(tab))/sum(tab)

# Confusion matrix and accuracy - testing data
p2 <- predict(lda.model, testing)$class
tab1 <- table(Predicted = p2, Actual = testing$price.class)
tab1
sum(diag(tab1))/sum(tab1)

