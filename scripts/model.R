##### 4. MODEL DATA ######
library(caTools)
library(caret)

df <- read.csv("./data/performance-clean.csv",encoding = "UTF-8")

# df <- df[df$position=="A",] # enhance the results by dividing dataset in A/D

################################# MODEL TESTS #################################

# # feature selection based on significance levels
# lm.model <- lm(formula= market.value ~
#                  age+position+sub.position+
#                  contract.expires+current.league+
#                  games.17.18+log(assists.17.18+1)+
#                  games.18.19+minutes.18.19+
#                  games.19.20+
#                  games.20.21+log(goals.20.21+1)+minutes.20.21+log(assists.20.21+1)
#                ,data=df)
# summary(lm.model) # ~ Adjusted R-squared: 0.6506
# # visualize model results
# 
# par(mfrow=c(2,2))
# plot(lm.model)
# par(mfrow=c(1,1))

### TEST 1
# model only year 20/21
lm.model <- lm(formula= market.value ~
                 age+offensive+sub.position+
                 contract.expires+current.league+
                 games.20.21+log(goals.20.21+1)+minutes.20.21+log(assists.20.21+1)+yellow.player.20.21+orange.player.20.21+red.player.20.21
               ,data=df)
summary(lm.model) # ~ Adjusted R-squared: 0.568
# visualize model results
par(mfrow=c(2,2))
plot(lm.model)
mtext("Model 1: years 20/21", side = 3, line = -28, outer = TRUE)
par(mfrow=c(1,1))


### TEST 2
# convert response variable to log
df$market.value <- log(df$market.value)

lm.model <- lm(formula= market.value ~
                 age+offensive+sub.position+
                 contract.expires+current.league+
                 games.20.21+log(goals.20.21+1)+minutes.20.21+log(assists.20.21+1)+yellow.player.20.21+orange.player.20.21+red.player.20.21
               ,data=df)
summary(lm.model) # ~ Adjusted R-squared: 0.568
# visualize model results
par(mfrow=c(2,2))
plot(lm.model)
mtext("Model 2: log(market.value) + years 20/21", side = 3, line = -28, outer = TRUE)
par(mfrow=c(1,1))

### TEST 3
# model years 20/21+19/20
lm.model <- lm(formula= market.value ~
                 age+offensive+sub.position+
                 contract.expires+current.league+
                 games.19.20+log(goals.19.20+1)+minutes.19.20+log(assists.19.20+1)+yellow.player.19.20+orange.player.19.20+red.player.19.20+
                 games.20.21+log(goals.20.21+1)+minutes.20.21+log(assists.20.21+1)+yellow.player.20.21+orange.player.20.21+red.player.20.21
               ,data=df)
summary(lm.model) # ~ Adjusted R-squared: 0.613
# visualize model results
par(mfrow=c(2,2))
plot(lm.model)
mtext("Model 3: log(market.value) + years 20/21+19/20", side = 3, line = -28, outer = TRUE)
par(mfrow=c(1,1))

### TEST 4
# years: 20/21 + 19/20 + 18/19
lm.model <- lm(formula= market.value ~
                 age+offensive+sub.position+
                 contract.expires+current.league+
                 games.18.19+log(goals.18.19+1)+minutes.18.19+log(assists.18.19+1)+yellow.player.18.19+orange.player.18.19+red.player.18.19+
                 games.19.20+log(goals.19.20+1)+minutes.19.20+log(assists.19.20+1)+yellow.player.19.20+orange.player.19.20+red.player.19.20+
                 games.20.21+log(goals.20.21+1)+minutes.20.21+log(assists.20.21+1)+yellow.player.20.21+orange.player.20.21+red.player.20.21
               ,data=df)
summary(lm.model) # ~ Adjusted R-squared: 0.6413
# visualize model results
par(mfrow=c(2,2))
plot(lm.model)
mtext("Model 4: log(market.value) + years 20/21+19/20+18/19", side = 3, line = -28, outer = TRUE)
par(mfrow=c(1,1))

### TEST 5
# years: 20/21 + 19/20 + 18/19 + 17/18
lm.model <- lm(formula= market.value ~
                 age+sub.position+
                 contract.expires+current.league+
                 games.17.18+log(goals.17.18+1)+minutes.17.18+log(assists.17.18+1)+yellow.player.17.18+orange.player.17.18+red.player.17.18+
                 games.18.19+log(goals.18.19+1)+minutes.18.19+log(assists.18.19+1)+yellow.player.18.19+orange.player.18.19+red.player.18.19+
                 games.19.20+log(goals.19.20+1)+minutes.19.20+log(assists.19.20+1)+yellow.player.19.20+orange.player.19.20+red.player.19.20+
                 games.20.21+log(goals.20.21+1)+minutes.20.21+log(assists.20.21+1)+yellow.player.20.21+orange.player.20.21+red.player.20.21
               ,data=df)
summary(lm.model) # ~ Adjusted R-squared: 0.6528
# visualize model results
par(mfrow=c(2,2))
plot(lm.model)
mtext("Model 5: log(market.value) + years 20/21+19/20+18/19+17/18", side = 3, line = -28, outer = TRUE)
par(mfrow=c(1,1))

################################# REGRESSION ON TRAINING SET #################################

### CREATE TRAIN/TEST SPLIT ###

set.seed(55)
split = sample.split(df$market.value, SplitRatio = 0.75)

training = subset(df, split == TRUE)
testing = subset(df, split == FALSE)

# Here we use the best model found so far
# years: 20/21 + 19/20 + 18/19 + 17/18
lm.model <- lm(formula= market.value ~
                 age+sub.position+
                 contract.expires+current.league+
                 games.17.18+log(goals.17.18+1)+minutes.17.18+log(assists.17.18+1)+yellow.player.17.18+orange.player.17.18+red.player.17.18+
                 games.18.19+log(goals.18.19+1)+minutes.18.19+log(assists.18.19+1)+yellow.player.18.19+orange.player.18.19+red.player.18.19+
                 games.19.20+log(goals.19.20+1)+minutes.19.20+log(assists.19.20+1)+yellow.player.19.20+orange.player.19.20+red.player.19.20+
                 games.20.21+log(goals.20.21+1)+minutes.20.21+log(assists.20.21+1)+yellow.player.20.21+orange.player.20.21+red.player.20.21
               ,data=training)
summary(lm.model) # ~ Adjusted R-squared: 0.6528

################################# PREDICTION ON TEST SET #################################

## use the predicted values of the model and save a new feature to the model
testing['market.value.predicted'] <- predict(lm.model, newdata = testing)

hist(testing$market.value,breaks = pretty(10:20, n = 10), freq = FALSE, col=(rgb(0,0,255, max = 255, alpha = 100)), main="Market value vs Predicted")
hist(testing$market.value.predicted,breaks = pretty(10:20, n = 10), freq = FALSE, add=T, col=(rgb(255,0,0, max = 255, alpha = 100)))

summary(testing$market.value)-summary(testing$market.value.predicted) # low values => similar around the mean, diverge in min/max

################################# CLASSIFICATION #################################

get.accuracy <- function(training,testing,percentages,price.class.names){
  quantiles <- quantile(training$market.value,percentages)
  print("Percentiles used:")
  print(quantiles)
  N <- length(quantiles)+1
  # create the "price.class" feature for the test set
  for(i in 1:N){
    if(i==1){
      testing[testing$market.value<quantiles[[i]],'price.class']=price.class.names[i]
      testing[testing$market.value.predicted<quantiles[[i]],'price.class.predicted']=price.class.names[i]
    }
    else if (i==N){
      testing[(testing$market.value>=quantiles[[i-1]]),'price.class']=price.class.names[i]
      testing[(testing$market.value.predicted>=quantiles[[i-1]]),'price.class.predicted']=price.class.names[i]
    }
    else{
      testing[(testing$market.value>=quantiles[[i-1]])&(testing$market.value<quantiles[[i]]),'price.class']=price.class.names[i]
      testing[(testing$market.value.predicted>=quantiles[[i-1]])&(testing$market.value.predicted<quantiles[[i]]),'price.class.predicted']=price.class.names[i]
    }
  }
  
  testing$price.class <- as.factor(testing$price.class)
  testing$price.class.predicted <- as.factor(testing$price.class.predicted)
  
  par(mfrow=c(1,2))
  plot(testing$price.class,col=(rgb(0,0,255, max = 255, alpha = 100)), main="Price Class")
  plot(testing$price.class.predicted,col=(rgb(255,0,0, max = 255, alpha = 100)),main="Price Class Predicted")
  par(mfrow=c(1,1))
  
  
  ### build the confusion matrix by hand
  price.class <- price.class.names
  cm <- matrix(0,nrow=N,ncol=N)
  rownames(cm) <- price.class.names
  colnames(cm) <- price.class.names
  for(p1 in price.class){
    for(p2 in price.class){
      cm[p1,p2] <- dim(testing[(testing$price.class==p1) & (testing$price.class.predicted==p2),])[1]
    }
  }
  
  # visualize results
  print(paste("Confusion Matrix (", dim(testing)[1] ,"istances )"))
  print(cm)
  # calculate accuracy
  print(paste("Accuracy =",round(sum(diag(cm))/sum(cm)*10000)/100,"%"))
}

get.accuracy(training,testing,c(.25,.50,.75),c("low","medium","high","super"))
get.accuracy(training,testing,c(.33,.66),c("low","medium","high"))


########## LINEAR DISCRIMINANT ANALISYS
# 
# lda.model <- lda(price.class~
#                    age+offensive+
#                    contract.expires+current.league+
#                    games.17.18+log(assists.17.18+1)+
#                    games.18.19+minutes.18.19+
#                    games.19.20+
#                    games.20.21+log(goals.20.21+1)+minutes.20.21+log(assists.20.21+1), 
#                  data = training)
# lda.model
# 
# 
# 
# p1 <- predict(lda.model, training)$class
# tab <- table(Predicted = p1, Actual = training$price.class)
# tab
# sum(diag(tab))/sum(tab)
# 
# # Confusion matrix and accuracy - testing data
# p2 <- predict(lda.model, testing)$class
# tab1 <- table(Predicted = p2, Actual = testing$price.class)
# tab1
# sum(diag(tab1))/sum(tab1)

