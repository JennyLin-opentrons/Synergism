#### If to form a function, input could be WD(working directory), SN(strain name), PN(plate number)

getwd()
setwd('C:/Users/40355/Documents/R')
## to combine raw data with supplementary
library(dplyr)

### Import supplementary data
chemicals <- read.csv("data/chemical-matrix.csv", header=TRUE)

### Raw data formatting
## Name each wells from A1, A2, A3 to H10, H11, H12
rowLetter <- character(0)
colNumber <- numeric(0)
colNumber <- c(colNumber, rep(1:12, times = 8))
for(i in 1:8){
  rowLetter <- c(rowLetter, rep(LETTERS[i], times = 12))
}
a<-0
a <- paste(rowLetter, colNumber, sep="")
## Create a empty data frame
area <- data.frame(well=rep(a[c(14:23,26:35,38:47,50:59,62:71,74:83)], 5), area1=rep(1:60,5), area2=rep(1:60,5), area3=rep(1:60,5), area4=rep(1:60,5))
## Function to transform raw data
AreaUnderSpline <- function(Time, N, minT, maxT){
  spline.area <- stats::integrate(stats::splinefun(Time, N, method = "natural"), 
                                  lower = minT, upper = maxT)$value
  return(spline.area)
}

### Import raw data
count <- c(14:23,26:35,38:47,50:59,62:71,74:83)
## j is the plate number
## Four replicates are written in the same row
SN <- "527"
for (j in 1:20) {
  text <- paste("data/527/isolate-", SN, "-plate-", j, ".txt", sep="")
  data <- read.table(text, sep = "\t", header = TRUE, check.names = FALSE)
  
  i<-0
  number <- 0
  for (i in count) {
    number=number+1
    area[((j-1)%%5)*60+number,(j-1)%/%5+2]<- AreaUnderSpline(data$Time,data[[i+2]], 0, 72)
  }
}
## Input manually plate number (from 1 to 5)
plateIndex <- numeric()
for (rep in 1:5) {
  plateIndex <- c(plateIndex, rep(rep,60))
} 
area$plate <- plateIndex

### Combine raw data with supplementary data
df <- full_join(area, chemicals, by = c("well" = "Well", "plate"="Plate"))
## Isolate negative control for further calculation
residual <- subset(df, Complexity == 0)
residual$area <- rowMeans(residual[,2:5],na.rm=TRUE)
## Transform the data to effect in each well
df$area1 <- df$area1-mean(residual$area)
df$area2 <- df$area2-mean(residual$area)
df$area3 <- df$area3-mean(residual$area)
df$area4 <- df$area4-mean(residual$area)

### Export the data for further analysis
write.csv(df,"C:/Users/40355/Documents/R\\data.csv", row.names = FALSE)
