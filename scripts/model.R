##### 4. MODEL DATA ######
library(caTools)
library(caret)

df <- read.csv("./data/performance-clean.csv",encoding = "UTF-8")

head(df)

# convert response variable to log
df$market.value <- log(df$market.value)


### CREATE TRAIN/TEST SPLIT ###

set.seed(100)
split = sample.split(df$market.value, SplitRatio = 0.8)

training = subset(df, split == TRUE)
testing = subset(df, split == FALSE)

################################# REGRESSION #################################

# feature selection based on significance levels
lm.model <- lm(formula= market.value ~
                 age+offensive+sub.position+
                 contract.expires+current.league+
                 games.17.18+log(assists.17.18+1)+
                 games.18.19+minutes.18.19+
                 games.19.20+
                 games.20.21+log(goals.20.21+1)+minutes.20.21+log(assists.20.21+1)
               ,data=training)
summary(lm.model) # ~ Adjusted R-squared: 0.6506

# model only year 20/21
lm.model <- lm(formula= market.value ~
                 age+offensive+sub.position+
                 contract.expires+current.league+
                 games.20.21+log(goals.20.21+1)+minutes.20.21+log(assists.20.21+1)+yellow.player.20.21+orange.player.20.21+red.player.20.21
               ,data=training)
summary(lm.model) # ~ Adjusted R-squared: 0.568

# model years 20/21+19/20
lm.model <- lm(formula= market.value ~
                 age+offensive+sub.position+
                 contract.expires+current.league+
                 games.19.20+log(goals.19.20+1)+minutes.19.20+log(assists.19.20+1)+yellow.player.19.20+orange.player.19.20+red.player.19.20+
                 games.20.21+log(goals.20.21+1)+minutes.20.21+log(assists.20.21+1)+yellow.player.20.21+orange.player.20.21+red.player.20.21
               ,data=training)
summary(lm.model) # ~ Adjusted R-squared: 0.613

# years: 20/21 + 19/20 + 18/19
lm.model <- lm(formula= market.value ~
                 age+offensive+sub.position+
                 contract.expires+current.league+
                 games.18.19+log(goals.18.19+1)+minutes.18.19+log(assists.18.19+1)+yellow.player.18.19+orange.player.18.19+red.player.18.19+
                 games.19.20+log(goals.19.20+1)+minutes.19.20+log(assists.19.20+1)+yellow.player.19.20+orange.player.19.20+red.player.19.20+
                 games.20.21+log(goals.20.21+1)+minutes.20.21+log(assists.20.21+1)+yellow.player.20.21+orange.player.20.21+red.player.20.21
               ,data=training)
summary(lm.model) # ~ Adjusted R-squared: 0.6413

# years: 20/21 + 19/20 + 18/19 + 17/18
lm.model <- lm(formula= market.value ~
                 age+offensive+sub.position+
                 contract.expires+current.league+
                 games.17.18+log(goals.17.18+1)+minutes.17.18+log(assists.17.18+1)+yellow.player.17.18+orange.player.17.18+red.player.17.18+
                 games.18.19+log(goals.18.19+1)+minutes.18.19+log(assists.18.19+1)+yellow.player.18.19+orange.player.18.19+red.player.18.19+
                 games.19.20+log(goals.19.20+1)+minutes.19.20+log(assists.19.20+1)+yellow.player.19.20+orange.player.19.20+red.player.19.20+
                 games.20.21+log(goals.20.21+1)+minutes.20.21+log(assists.20.21+1)+yellow.player.20.21+orange.player.20.21+red.player.20.21
               ,data=training)
summary(lm.model) # ~ Adjusted R-squared: 0.6528

# visualize model results

par(mfrow=c(2,2))
plot(lm.model)
par(mfrow=c(1,1))

################################# PREDICTION #################################

## use the predicted values of the model and save a new feature to the model
testing['market.value.predicted'] <- predict(lm.model, newdata = testing)
par(mfrow=c(1,2))
hist(testing$market.value)
hist(testing$market.value.predicted)
par(mfrow=c(1,1))

summary(testing$market.value)
summary(testing$market.value.predicted)

################################# CLASSIFICATION #################################

get.accuracy <- function(training,testing,percentages,price.class.names){
  quantiles <- quantile(training$market.value,percentages)
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
  plot(testing$price.class)
  plot(testing$price.class.predicted)
  par(mfrow=c(1,1))
  
  
  ### build the confusion matrix by hand
  price.class <- price.class.names
  cm <- matrix(0,nrow=N,ncol=N)
  rownames(cm) <- price.class.names
  colnames(cm) <- price.class.names
  cm
  for(p1 in price.class){
    # print(paste("p1",p1))
    for(p2 in price.class){
      # print(paste("p2",p2))
      # x <- dim(testing[(testing$price.class==p1) & (testing$price.class.predicted==p2),])[1]
      # print(x)
      cm[p1,p2] <- dim(testing[(testing$price.class==p1) & (testing$price.class.predicted==p2),])[1]
    }
  }
  
  # visualize results
  print(cm)
  # calculate accuracy
  return (sum(diag(cm))/sum(cm)*100)
}

get.accuracy(training,testing,c(.30,.70),c("low","medium","high"))


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

