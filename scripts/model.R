##### 4. MODEL DATA ######
library(MASS)
library(caTools)
library(caret)

df <- read.csv("./data/performance-clean.csv",encoding = "UTF-8")

################################# MODEL TESTS #################################

### TEST 1
# player attributes + year 20/21
lm.model <- lm(formula= market.value ~
                 age+height+offensive+foot+
                 contract.expires+current.league+
                 games.20.21+goals.20.21+minutes.20.21+assists.20.21+yellow.player.20.21+orange.player.20.21+red.player.20.21,
               data=df)
summary(lm.model) # ~ Adjusted R-squared: 0.3757
# visualize model results
par(mfrow=c(2,2))
plot(lm.model)
mtext("Model 1: years 20/21", side = 3, line = -28, outer = TRUE)
par(mfrow=c(1,1))


### TEST 1.1
# convert response variable to log
df$market.value <- log(df$market.value)
lm.model <- lm(formula= market.value ~
                 age+height+offensive+foot+
                 contract.expires+current.league+
                 games.20.21+goals.20.21+minutes.20.21+assists.20.21+yellow.player.20.21+orange.player.20.21+red.player.20.21,
               data=df)
summary(lm.model) # ~ Adjusted R-squared: 0.5671
# visualize model results
par(mfrow=c(2,2))
plot(lm.model)
mtext("Model 1.1: log(market.value) ~ years 20/21", side = 3, line = -28, outer = TRUE)
par(mfrow=c(1,1))


### TEST 2
# model years 20/21+19/20
lm.model <- lm(formula= market.value ~
                 age+height+offensive+foot+
                 contract.expires+current.league+
                 games.19.20+goals.19.20+minutes.19.20+assists.19.20+yellow.player.19.20+orange.player.19.20+red.player.19.20+
                 games.20.21+goals.20.21+minutes.20.21+assists.20.21+yellow.player.20.21+orange.player.20.21+red.player.20.21
               ,data=df)
summary(lm.model) # ~ Adjusted R-squared: 0.6086
# visualize model results
par(mfrow=c(2,2))
plot(lm.model)
mtext("Model 2: log(market.value) ~ years 20/21+19/20", side = 3, line = -28, outer = TRUE)
par(mfrow=c(1,1))

### TEST 3
# years: 20/21 + 19/20 + 18/19
lm.model <- lm(formula= market.value ~
                 age+height+offensive+foot+
                 contract.expires+current.league+
                 games.18.19+goals.18.19+minutes.18.19+assists.18.19+yellow.player.18.19+orange.player.18.19+red.player.18.19+
                 games.19.20+goals.19.20+minutes.19.20+assists.19.20+yellow.player.19.20+orange.player.19.20+red.player.19.20+
                 games.20.21+goals.20.21+minutes.20.21+assists.20.21+yellow.player.20.21+orange.player.20.21+red.player.20.21
               ,data=df)
summary(lm.model) # ~ Adjusted R-squared: 0.6338
# visualize model results
par(mfrow=c(2,2))
plot(lm.model)
mtext("Model 3: log(market.value) ~ years 20/21+19/20+18/19", side = 3, line = -28, outer = TRUE)
par(mfrow=c(1,1))

### TEST 4
# years: 20/21 + 19/20 + 18/19 + 17/18
lm.model <- lm(formula= market.value ~
                 age+height+offensive+foot+
                 contract.expires+current.league+
                 games.17.18+goals.17.18+minutes.17.18+assists.17.18+yellow.player.17.18+orange.player.17.18+red.player.17.18+
                 games.18.19+goals.18.19+minutes.18.19+assists.18.19+yellow.player.18.19+orange.player.18.19+red.player.18.19+
                 games.19.20+goals.19.20+minutes.19.20+assists.19.20+yellow.player.19.20+orange.player.19.20+red.player.19.20+
                 games.20.21+goals.20.21+minutes.20.21+assists.20.21+yellow.player.20.21+orange.player.20.21+red.player.20.21
               ,data=df)
summary(lm.model) # ~ Adjusted R-squared: 0.6429
# visualize model results
par(mfrow=c(2,2))
plot(lm.model)
mtext("Model 4: log(market.value) + years 20/21+19/20+18/19+17/18", side = 3, line = -28, outer = TRUE)
par(mfrow=c(1,1))

### WHY WE AGGREGATE 
# inconsistency in p-values in various features of the same years and between years.
# TODO!!!!!!!

### TEST 5
# only sums
lm.model <- lm(formula= market.value ~
                 age+height+offensive+foot+
                 contract.expires+current.league+
                 games+goals+assists+
                 yellow+orange+red,
               data=df)
summary(lm.model) # ~ Adjusted R-squared: 0.6038
# visualize model results
par(mfrow=c(2,2))
plot(lm.model)
mtext("Model 5: log(market.value) + years 20/21+19/20+18/19+17/18", side = 3, line = -28, outer = TRUE)
par(mfrow=c(1,1))



### BACKWARD MODEL SELECTION
### TEST 5.1
lm.model <- step(lm.model,steps=20,trace=0,direction = "backward")
summary(lm.model) # ~ Adjusted R-squared: 0.6039
# visualize model results
par(mfrow=c(2,2))
plot(lm.model)
mtext("Model 5.1: Backward model selection", side = 3, line = -28, outer = TRUE)
par(mfrow=c(1,1))

############### REMOVE HLP (HIGH LEVERAGE POINTS)

# dim(df)[1]-length(hatvalues(lm.model))
# total.length <- dim(df)[1]
# df <- df[hatvalues(lm.model) <= 4 * mean(hatvalues(lm.model)),]
# total.length-dim(df)[1] # removed high leverage points
### !! removing the HLPs doesn't not improve the model

######### BEST MODEL
### TEST 5.2
lm.model <- lm(formula= market.value ~
                 age+offensive+height+
                 games+goals+assists+
                 contract.expires+
                 yellow+
                 current.league,
               data=df)
summary(lm.model) # ~ Adjusted R-squared: 0.6036
# visualize model results
par(mfrow=c(2,2))
plot(lm.model)
mtext("Model 5.2: Final reduced model", side = 3, line = -28, outer = TRUE)
par(mfrow=c(1,1))


################################# REGRESSION ON TRAINING SET #################################

### CREATE TRAIN/TEST SPLIT ###

set.seed(100)
split = sample.split(df$market.value, SplitRatio = 0.75)

training = subset(df, split == TRUE)
testing = subset(df, split == FALSE)

# here we use the reduced model ( last tested ) with the selected features only (after backward model selection)
lm.model <- lm(formula= market.value ~
                 age+offensive+height+
                 games+goals+assists+
                 contract.expires+
                 yellow+
                 current.league,
               data=training)
summary(lm.model) # ~ Adjusted R-squared: 0.6099
# visualize model results
par(mfrow=c(2,2))
plot(lm.model)
par(mfrow=c(1,1))

################################# PREDICTION ON TEST SET #################################

## use the predicted values of the model and save a new feature into the model
testing['market.value.predicted'] <- predict(lm.model, newdata = testing)

# simple visualization of how the two distributions overlap
hist(testing$market.value.predicted,breaks = pretty(10:20, n = 10), freq = FALSE, col=(rgb(255,0,0, max = 255, alpha = 100)), main="Predicted vs Market value")
hist(testing$market.value,breaks = pretty(10:20,n = 10), freq = FALSE,add=T, col=(rgb(0,0,255, max = 255, alpha = 100)))
# as we can see, the prediction is more concentrated around the mean and has lower tails

summary(testing$market.value)-summary(testing$market.value.predicted) # low values => similar around the mean, diverge at the extremes

################################# CLASSIFICATION #################################

get.accuracy <- function(training,testing,percentages,price.class.names){
  quantiles <- quantile(training$market.value,percentages)
  print("Percentiles used:")
  print(quantiles)
  N <- length(quantiles)+1
  # create the "price.class" feature for the test set
  for(i in 1:N){
    if(i==1){
      testing[testing$market.value<=quantiles[[i]],'price.class']=price.class.names[i]
      testing[testing$market.value.predicted<=quantiles[[i]],'price.class.predicted']=price.class.names[i]
    }
    else if (i==N){
      testing[(testing$market.value>quantiles[[i-1]]),'price.class']=price.class.names[i]
      testing[(testing$market.value.predicted>quantiles[[i-1]]),'price.class.predicted']=price.class.names[i]
    }
    else{
      testing[(testing$market.value>quantiles[[i-1]])&(testing$market.value<=quantiles[[i]]),'price.class']=price.class.names[i]
      testing[(testing$market.value.predicted>quantiles[[i-1]])&(testing$market.value.predicted<=quantiles[[i]]),'price.class.predicted']=price.class.names[i]
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

# try with 4 price classes divided using 25°, 50° and 75° percentile of the market.value distribution
get.accuracy(training,testing,c(.25,.50,.75),c("low","medium","high","super")) # Accuracy 51.15%

# with 3 classes divided using 33° and 66° percentile of the market.value distribution
get.accuracy(training,testing,c(.33,.67),c("low","medium","high")) # Accuracy 63.49%


##########################################
### COMPARE WITH LDA BUILT-IN FUNCTION ###
##########################################

thresholds <- c(.33,.67)
class.names <- c("low","medium","high")

q <- quantile(df$market.value,thresholds)

# create price.class feature on entire dataset based on percentile thresholds
df[df$market.value<=q[[1]],'price.class']=class.names[1]
df[(df$market.value>q[[1]])&(df$market.value<=q[[2]]),'price.class']=class.names[2]
df[df$market.value>q[[2]],'price.class']=class.names[3]

df$price.class <- as.factor(df$price.class)

plot(df$price.class, main="Distribution of 'price.class' among entire dataset")

# same seed as before to be consistent with the train/test split
set.seed(100)
split = sample.split(df$market.value, SplitRatio = 0.75)

training = subset(df, split == TRUE)
testing = subset(df, split == FALSE)

# Here we use the same features but the model tries to fit on the "price.class" feature directly
lda.model <- lda(formula=price.class~
                  age+offensive+
                  contract.expires+current.league+
                  games+goals+assists+
                  yellow,
                data=training)

confusionMatrix(testing$price.class, predict(lda.model,newdata = testing)$class) # Accuracy 64.29%
# Confusion Matrix
#           Reference
# Prediction high low medium
# high    251  23     85
# low      13 279     86
# medium   70 128    199
