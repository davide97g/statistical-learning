###
# This data has been gathered at two solar power plants in India over a 34 day period.
# It has two pairs of files - each pair has one power generation dataset and one sensor readings dataset. 
# The power generation datasets are gathered at the inverter level - each inverter has multiple lines of solar panels
# attached to it. The sensor data is gathered at a plant level - single array of sensors optimally placed at the plant.

# There are a few areas of concern at the solar power plant -
# 1. Can we predict the power generation for next couple of days? - this allows for better grid management
# 2. Can we identify the need for panel cleaning/maintenance?
# 3. Can we identify faulty or suboptimally performing equipment?

### plants
df.power1 <- read.csv("./data/Plant_1_Generation_Data.csv")
df.power2 <- read.csv("./data/Plant_2_Generation_Data.csv")
df.weather1 <- read.csv("./data/Plant_1_Weather_Sensor_Data.csv")
df.weather2 <- read.csv("./data/Plant_2_Weather_Sensor_Data.csv")

# look at the data
head(df.power1)
head(df.power2)
head(df.weather1)
head(df.weather2)

### CLEAN
# plat_id
df.power1$PLANT_ID <- NULL
df.power2$PLANT_ID <- NULL
df.weather1$PLANT_ID <- NULL
df.weather2$PLANT_ID <- NULL
# source_key
df.power1$SOURCE_KEY <- NULL
df.power2$SOURCE_KEY <- NULL
df.weather1$SOURCE_KEY <- NULL
df.weather2$SOURCE_KEY <- NULL

# cleaned
head(df.power1)
head(df.weather1)

# summary stats
summary(df.power1)
summary(df.weather1)

### POWER PLANT
help(pairs)

# complete overview
## put histograms on the diagonal
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "coral", ...)
}
## put (absolute) correlations on the upper panels,
## with size proportional to the correlations.
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
# power plant 1
pairs(df.power1, diag.panel = panel.hist, upper.panel = panel.cor)
# power plant 2
pairs(df.power2, diag.panel = panel.hist, upper.panel = panel.cor)

# weather 1
pairs(df.weather1, diag.panel = panel.hist, upper.panel = panel.cor)
# weather 2
pairs(df.weather2, diag.panel = panel.hist, upper.panel = panel.cor)

# sizes
dim(df.power1)
dim(df.power2)
dim(df.weather1)
dim(df.weather2)
