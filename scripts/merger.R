################
# Merger : merge every feature in one unique dataset
################

### define feature names
features = c("Commenti","IscrittiConquistati","IscrittiPersi", "MiPiace","NonMiPiace","TempoDiVisualizzazione", "Visualizzazioni")

path = "./data/totals/"

for(feature in features){
  feature.path <- paste("./data/features/",feature, sep="")
  dirs <-  list.dirs(path = feature.path)
  dirs <- dirs[-1] # remove first redundant
  print(feature)
  
  feature.df = data.frame() # data frame for the feature
  
  for(dir in dirs){
    # extract file "Dati del grafico"
    files <- list.files(path=dir, full.names = TRUE)
    file.name <- files[1]
    df <- read.csv(file.name)
    # print(dim(df)[1])
    feature.df <- rbind(feature.df,df)
  }
  print(dim(feature.df)[1])
  print(summary(feature.df))
  write.csv(feature.df,paste(path,feature,"_totals.csv",sep=""),row.names = FALSE)
  print("---")
}

merge <- function(df1,df2){
  print(df1)
  print(df2)
  df <- data.frame()
  return(df)
}

print(typeof(dfs))
