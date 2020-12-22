### join the totals in one unique super dataset
path = "./data/totals/"

features = c("Commenti","IscrittiConquistati","IscrittiPersi", "MiPiace","NonMiPiace","TempoDiVisualizzazione", "Visualizzazioni")

# df1 <- read.csv(paste(path,features[1],"_totals.csv",sep=""))
# df2 <- read.csv(paste(path,features[2],"_totals.csv",sep=""))


merged <- data.frame()

for(feature in features){
  
  feature.df <- read.csv(paste(path,feature,"_totals.csv",sep=""))  
  
  feature.df$X = NULL
  feature.df$Titolo.video = NULL
  feature.df$Ora.pubblicazione.video = NULL
 
  if(dim(merged)[1]==0) {
    merged <- feature.df
  }
  else{
    merged <- merge(merged,feature.df, all=TRUE, by=c("Video","Data"))
  }
}
write.csv(merged,"./data/dataset.csv", row.names=FALSE)

# df1$X = NULL
# df2$X = NULL

# df1$Titolo.video = NULL
# df2$Titolo.video = NULL

# df1$Ora.pubblicazione.video = NULL
# df2$Ora.pubblicazione.video = NULL

# print(dim(df1))
# print(head(df1))
# print(dim(df2))
# print(head(df2))


# merged <- merge(df1,df2, by=c("Data","Video"))
# print(dim(merged))
# print(merged)

# par(mfrow=c(2,2))
# plot(density(df1$Commenti.aggiunti))
# plot(density(df2$Iscritti.conquistati))
# plot(density(merged$Commenti.aggiunti))
# plot(density(merged$Iscritti.conquistati))
# par(mfrow=c(1,1))

# file.name.merged = paste(features[1],"_",features[2],".csv",sep="")
# path.to.save = paste("./data/joined/",file.name.merged,sep="")
# write.csv(merged,path.to.save, row.names=FALSE)
