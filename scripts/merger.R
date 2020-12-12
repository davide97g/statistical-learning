################
# Merger : merge every feature in one unique dataset
################

### define feature names
features = c("Commenti","IscrittiConquistati","IscrittiPersi", "MiPiace","NonMiPiace","TempoDiVisualizzazione", "Visualizzazioni")

dfs <-  data.frame()

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
    feature.df <- rbind(feature.df,df)
  }
  print(summary(feature.df))
  print("---")
}
