### read the csv file
df <- read.csv("./data/test.csv")

### visualize correlation
pairs(df)


### hist

hist(df$Visualizzazioni)
d <- density(df$Visualizzazioni)
plot(d)
