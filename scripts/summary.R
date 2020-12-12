### load dataset

df1 <- read.csv("./data/features/Commenti1/Dati del grafico.csv")
df2 <- read.csv("./data/features/Commenti2/Dati del grafico.csv")
df3 <- read.csv("./data/features/Commenti3/Dati del grafico.csv")
df4 <- read.csv("./data/features/Commenti4/Dati del grafico.csv")
df5 <- read.csv("./data/features/Commenti5/Dati del grafico.csv")
df6 <- read.csv("./data/features/Commenti6/Dati del grafico.csv")

### create list of data frames
df.list <-  list(df1,df2,df3,df4,df5,df6,df.v1)

removeUselessColumns <- function(df, cols){
  for(col in cols){
    df[col] = NULL
  }
  return(df)
}

cleanComments <- function (df){
  df <- df[df$Commenti.aggiunti != 0,]  
  df <- df[df$Commenti.aggiunti < 40,]
  return(df)
}


columns.to.drop = c('Titolo.video','Ora.pubblicazione.video')
columns.to.keep = c('Data','Video','Commenti.aggiunti','Visualizzazioni')

total.df <- data.frame()

### for each data frame
for(df in df.list){
  ### remove rows with 0 comments
  df <- cleanComments(df)
  ### remove useless columns
  df <- removeUselessColumns(df,columns.to.drop)  
  ### bind
  total.df <- rbind(total.df,df)
}

summary(total.df)
par(mfrow=c(1,2))
hist(total.df$Commenti.aggiunti)
plot(density(total.df$Commenti.aggiunti))
par(mfrow=c(1,1))
