##### 2. CLEAN & FILTER DATA ######
# install.packages("tidyverse")
library(tidyverse)
df <- read.csv("./data/performance.csv",encoding = "UTF-8")

# visualize dataframe
head(df)

## drop useless columns
df$current.club = NULL

##### rename columns
names(df)

# rename games columns
df <- rename(df,games.17.18=X17.18.games)
df <- rename(df,games.18.19=X18.19.games)
df <- rename(df,games.19.20=X19.20.games)
df <- rename(df,games.20.21=X20.21.games)

# rename goals columns
df <- rename(df,goals.17.18=X17.18.goals)
df <- rename(df,goals.18.19=X18.19.goals)
df <- rename(df,goals.19.20=X19.20.goals)
df <- rename(df,goals.20.21=X20.21.goals)

# rename assists columns
df <- rename(df,assists.17.18=X17.18.assists)
df <- rename(df,assists.18.19=X18.19.assists)
df <- rename(df,assists.19.20=X19.20.assists)
df <- rename(df,assists.20.21=X20.21.assists)

# rename yellows columns
df <- rename(df,yellows.17.18=X17.18.yellows)
df <- rename(df,yellows.18.19=X18.19.yellows)
df <- rename(df,yellows.19.20=X19.20.yellows)
df <- rename(df,yellows.20.21=X20.21.yellows)

# rename second yellows columns
df <- rename(df,second.yellows.17.18=X17.18.second.yellows)
df <- rename(df,second.yellows.18.19=X18.19.second.yellows)
df <- rename(df,second.yellows.19.20=X19.20.second.yellows)
df <- rename(df,second.yellows.20.21=X20.21.second.yellows)

# rename reds columns
df <- rename(df,reds.17.18=X17.18.reds)
df <- rename(df,reds.18.19=X18.19.reds)
df <- rename(df,reds.19.20=X19.20.reds)
df <- rename(df,reds.20.21=X20.21.reds)

# rename minutes columns
df <- rename(df,minutes.17.18=X17.18.minutes)
df <- rename(df,minutes.18.19=X18.19.minutes)
df <- rename(df,minutes.19.20=X19.20.minutes)
df <- rename(df,minutes.20.21=X20.21.minutes)

###############################
### TYPE CHECK & CONVERSION ###
###############################

# type check 

typeof(df$age) # integer
typeof(df$height) # double

df[is.integer(df$name),]
typeof(df$games.17.18) # double
typeof(df$goals.17.18) # double
typeof(df$assists.17.18) # double
typeof(df$yellows.17.18) # double
typeof(df$second.yellows.17.18) # double
typeof(df$reds.17.18) # double
typeof(df$minutes.17.18) # double

### MARKET.VALUE
# need to cast this feature to integer for later development
typeof(df$market.value)
hist(df$market.value) # ! this fails: not recognized as numeric
df$market.value = as.integer(as.character(df$market.value)) # cast to character and then to integer
df <- df[!is.na(df$market.value),] # remove the NA produces by casting to integer the "-" values
typeof(df$market.value) # now I have integer type
hist(log(df$market.value)) # now it works

### CONTRACT.EXPIRES
# convert contract dates to only year number and then to integers
for(x in unique(df$contract.expires)){
  splitted <- str_split(x,", ",simplify = TRUE)
  to.replace <- paste(splitted[1],", ",sep="")
  df$contract.expires <- gsub(to.replace, '', df$contract.expires)
}

df$contract.expires = as.integer(df$contract.expires) # cast to character and then to integer
contract.min <- min(df$contract.expires, na.rm = TRUE) # find minimun excluding NA
df$contract.expires <-  df$contract.expires-contract.min # normalize data
df[is.na(df$contract.expires),'contract.expires']=0 # the NA will become contract.expires == 2021

### YELLOWS, SECOND YELLOWS, REDS

# for every yellows, if NA --> 0
df[is.na(df$yellows.17.18),'yellows.17.18'] = 0
df[is.na(df$yellows.18.19),'yellows.18.19'] = 0
df[is.na(df$yellows.19.20),'yellows.19.20'] = 0
df[is.na(df$yellows.20.21),'yellows.20.21'] = 0

# for every second yellows, if NA --> 0
df[is.na(df$second.yellows.17.18),'second.yellows.17.18'] = 0
df[is.na(df$second.yellows.18.19),'second.yellows.18.19'] = 0
df[is.na(df$second.yellows.19.20),'second.yellows.19.20'] = 0
df[is.na(df$second.yellows.20.21),'second.yellows.20.21'] = 0

# for every reds, if NA --> 0
df[is.na(df$reds.17.18),'reds.17.18'] = 0
df[is.na(df$reds.18.19),'reds.18.19'] = 0
df[is.na(df$reds.19.20),'reds.19.20'] = 0
df[is.na(df$reds.20.21),'reds.20.21'] = 0

###################################
### TODO : Discretization cards ###
###################################

# > 0 and == 0 make up to the total number of samples of our dataset
dim(df[df$yellows.17.18>0,])[1]+dim(df[df$yellows.17.18==0,])[1]-dim(df)[1]

# let's assign a categorical (logical) variable to each player:
# - yellow.player.y1.y2 if yellows.y1.y2 > 0  = TRUE, FALSE otherwise

df[df$yellows.17.18>=3,'yellow.player.17.18']=TRUE
df[df$yellows.17.18<3,'yellow.player.17.18']=FALSE

df[df$yellows.18.19>=3,'yellow.player.18.19']=TRUE
df[df$yellows.18.19<3,'yellow.player.18.19']=FALSE

df[df$yellows.19.20>=3,'yellow.player.19.20']=TRUE
df[df$yellows.19.20<3,'yellow.player.19.20']=FALSE

df[df$yellows.20.21>0,'yellow.player.20.21']=TRUE
df[df$yellows.20.21==0,'yellow.player.20.21']=FALSE

# now for the second.yellows.y1.y2

df[df$second.yellows.17.18>0,'orange.player.17.18']=TRUE
df[df$second.yellows.17.18==0,'orange.player.17.18']=FALSE

df[df$second.yellows.18.19>0,'orange.player.18.19']=TRUE
df[df$second.yellows.18.19==0,'orange.player.18.19']=FALSE

df[df$second.yellows.19.20>0,'orange.player.19.20']=TRUE
df[df$second.yellows.19.20==0,'orange.player.19.20']=FALSE

df[df$second.yellows.20.21>0,'orange.player.20.21']=TRUE
df[df$second.yellows.20.21==0,'orange.player.20.21']=FALSE

# and finally for the reds.y1.y2

df[df$reds.17.18>0,'red.player.17.18']=TRUE
df[df$reds.17.18==0,'red.player.17.18']=FALSE

df[df$reds.18.19>0,'red.player.18.19']=TRUE
df[df$reds.18.19==0,'red.player.18.19']=FALSE

df[df$reds.19.20>0,'red.player.19.20']=TRUE
df[df$reds.19.20==0,'red.player.19.20']=FALSE

df[df$reds.20.21>0,'red.player.20.21']=TRUE
df[df$reds.20.21==0,'red.player.20.21']=FALSE

### LEAGUES

# if we aggregate by league and sum over the market.value we get the following results
plot(aggregate(df$market.value, by=list(Category=df$current.league), FUN=sum))

# As we can see, there is a high difference between major leagues and the "others"
# for this reasons we decided to put the lower in just one class called "other"
# major leagues: bundesliga, la liga, ligue 1, premier league, serie a
# minor leagues: eredivisie, jupiler pro league, liga nos, premier liga, super lig

df$current.league <-as.character(df$current.league) # cast to character to overwrite values

df[(df$current.league=="Eredivisie")|
     (df$current.league=="Jupiler Pro League")|
     (df$current.league=="Liga NOS")|
     (df$current.league=="Premier Liga")|
     (df$current.league=="Süper Lig"),'current.league']= "Other"

df$current.league <- as.factor(df$current.league) # bring the factors back
# Now the "Other" factor is well balanced with the other major leagues in terms of market.value
# and we have reduced the complexity of handling small differences between low leagues
plot(aggregate(df$market.value, by=list(Category=df$current.league), FUN=sum))

### Merge positions A+CF & D+M
# offensive and difensive player are classified with differently, maybe the right path should be to create
# separate models for "A" (attackers/offensive) and "D" (defensive) players.
plot(df$position)
df[(df$position=="A")|(df$position=="CF"),"offensive"]=TRUE
df[(df$position=="D")|(df$position=="M"),"offensive"]=FALSE
hist(as.numeric(df$offensive), freq = FALSE)
#########################################
### FILTER & REMOVE NA & USELESS DATA ###
#########################################

# fill 0's
columns.to.fill = c("games.17.18","games.18.19","games.19.20","games.20.21",
                      "goals.17.18","goals.18.19","goals.19.20","goals.20.21",
                      "assists.17.18","assists.18.19","assists.19.20","assists.20.21",
                      "minutes.17.18","minutes.18.19","minutes.19.20","minutes.20.21")
for(col in columns.to.fill){
  df[is.na(df[col]),col]=0
  print(paste("NA filled with 0's in",col))
}


# filtering NA
# these are the features that can not be NA because
# we can not give them a default value or the missing value compromised the future model
columns.to.filter = c("foot","market.value","height","age")
for(col in columns.to.filter){
  df <- df[!is.na(df[col]), ]
  print(paste("removed NA from",col))
}

# remove empty or useless
# these are the features that can not be "empty" or "invalid" because
# we can not give them a default value or the missing value compromised the future model
columns.useless = c("foot","market.value","height","age")
for(col in columns.useless){
  df <- df[df[col]!='', ] # filter "" (empty)
  df <- df[df[col]!='-', ] # filter "-" (dash)
  print(paste("removed useless data from",col))
}

#####################################
### SAVE CLEAN DATA FRAME TO FILE ###
#####################################

# save the cleaned data frame to file 
write.csv(df, "./data/performance-clean.csv",row.names=FALSE)
