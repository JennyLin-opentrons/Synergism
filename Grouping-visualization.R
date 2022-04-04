#### Comments are based on 527 data
#### Part1 includes overviews on data points
#### Part2 includes arbitrary grouping on data points
#### Part3 includes tests on in-group and between-group variation

getwd()
setwd('C:/Users/40355/Documents/R')
library(ggplot2)
df <- read.csv("data/data-527.csv", header=TRUE)


### Part1 overviews 
df$area <- rowMeans(df[,2:5],na.rm=TRUE)
## prelimary check of the data
plot(density(df$area))
## Two local maximum is observed
plot(df$area)
## A rough grouping is obersved


cor.test(df$area, df$Complexity)
## AUC is significantly correlated with complexity with p-value < 2.2e-16

df$Amoxicillin <- as.factor(df$Amoxicillin)
df$Oxytetracycline <- as.factor(df$Oxytetracycline)
## The two antibiotics are more effective than other chemicals
GD <- ggplot(df, aes(Complexity, diff, color = area)) +
  geom_point(size = 3) +
  labs(x = "Number of chemicals in the mixture",
       y = "area changed under curve")

GDA <- ggplot(df, aes(well, diff, colour=Amoxicillin)) +
  geom_point(size = 3) +
  labs(title="Amoxicillin",
       x = "well",
       y = "area changed under curve") +
  theme(axis.text.x=element_blank())

GDX <- ggplot(df, aes(well, diff, colour=Oxytetracycline)) +
  geom_point(size = 3) +
  labs(title="Oxytetracycline",
       x = "well",
       y = "area changed under curve") +
  theme(axis.text.x=element_blank()) 

ggp_all <- GD / (GDA + GDX) +   
  plot_annotation(title = "Chemical effect on E.coli") & 
    theme(plot.title = element_text(hjust = 0.5))
ggp_all
                          
### Part2 Grouping
df$group <- NA
row_numbers <- 1:nrow(area)
x <- 0 
for (x in row_numbers){
  
  group <- df$area[x]
  if (group < 10){
    group_type <- "lethal"
  }
  else {
    if (group>mean(residul$area)){
      group_type <- "positive"
    }
    else {
      if (group < 30){
      group_type <- "negative"  
      }
    else
      group_type <- "unknown"
    }
  }
  df$group[x] <- group_type
}

index <- df$Complexity %in% 0
df$group[index] <- "NC"

ggplot(df, aes(Complexity, area, colour=group)) +
  geom_point(size = 3) +
  labs(title="Arbitary grouping on chemical effect",
       x = "Number of chemicals in the mixture",
       y = "area changed under curve") 

ggplot(df, aes(well, area, colour=group)) +
  geom_point(size = 3) +
  labs(title="plate 1-5",
       x = "Number of chemicals in the mixture",
       y = "area under curve") 

groupH <- subset(df, Complexity <= 2)
groupA <- subset(groupH, Amoxicillin == 1)
groupA$Complexity <- as.factor(groupA$Complexity)
ggplot(groupA, aes(well, area, colour=Complexity)) +
  geom_point(size = 5) +
  labs(title="Marked-up effect on Amoxicillin",
       x = "well",
       y = "area under curve")

groupS <- subset(df, Complexity >= 4)
groupS$Oxytetracycline <- as.factor(groupS$Oxytetracycline)
plot1 <- ggplot(groupS, aes(well, area, colour=Oxytetracycline)) +
  geom_point(size = 5) +
  labs(title="with Amoxicillin in high complexity",
       x = "well",
       y = "area under curve") +
  theme(axis.text.x=element_blank()) 

groupSS <- subset(groupS, Amoxicillin == 0)
plot2 <- ggplot(groupSS, aes(well, area, colour=Oxytetracycline)) +
  geom_point(size = 5) +
  labs(title="without Amoxicillin in high complexity",
       x = "well",
       y = "area under curve") +
  theme(axis.text.x=element_blank()) 

grid.arrange(plot1, plot2, ncol=2)

ggplot(area, aes(well, area, colour=group)) +
  geom_point(size = 3) +
  labs(title="plate 1-5",
       x = "well",
       y = "area under curve") 

par(mfcol=c(2,2)) 
part1 <- subset(df, group == "lethal")
part2 <- subset(df, group == "positive")
part3 <- subset(df, group == "negative")
part4 <- subset(df, group == "unknown")
part5 <- subset(df, group == "positive" | group == "unknown")
plot(density(part2$area))
## lethal and the unknown group (part1 and part4) are roughly normally distributed
## positive and negative group (part2 and part3) seems to have two local maximum

### Part3 in-group variation
## Dictionary for chemical combination
ChemicalSH <- c("A","C","D","G","I","M","O","T")
modelist<- as.character()
for(i in 1:8){
  modelist <- c(modelist,combn(ChemicalSH,m=i,FUN=paste0,collapse = ""))
}

## Input shorthand for chemical combination
x <- 0 
chemicals$sum <- NA
row_numbers <- 1:nrow(df)
for (x in row_numbers){
  Present_type <- ""
  for (y in 7:14) {
    present <- df[x,y]
    if (present==1 ){
      Present_type <- paste(Present_type, ChemicalSH[y-6], sep="")
    }
    df$sum[x] <- Present_type
  }
}

## single chemical effect
IE <- subset(df, Complexity == 1) 

## two chemical effect
PE <- subset(df, Complexity == 2)

## randomly sum up
test <- data.frame(ID = c(1:448), area.IE = c(1:448))
number <- 0
row_numbers1<- 1:(nrow(IE)-1)

for (k in row_numbers1){
  row_numbers2<- (k+1):nrow(IE)
  for (l in row_numbers2) {
    for (i in 2:5) {
      for (j in 2:5){
        number <- number+1
        test$area.IE[number]<- (IE[k, i]+IE [l, j])  
        test$ID[number] <- paste(IE$sum[k], IE$sum[l], sep=" ")
      }
    }
  }
}

## sort ID so that it is compatible with the other dataframe
row_number3 <- 1:(nrow(test))
for (i in row_number3){
  array <- test[i, 1]
  arrayS <- strsplit(array, " ")
  arrayS <- sort(arrayS[[1]])
  test[i, 1] <- paste( unlist(arrayS), collapse='')
}

test <- full_join(test, PE[17:18], by = c("ID" = "sum"))

## check t.test p-value for all possible variation
row_number4 <- 1:((nrow(test))%/%16)
for (i in row_number4){
  mu <- test[i*16,3]
  t.test(test[i:(i+16),2], mu=mu)
  arrayR[i] <- t.test(test[1:16,2], mu=mu)$p.value
} 
## maximum is less that 0.05



