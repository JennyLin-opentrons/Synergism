#### Current file only consider two chemicals and can be potentially extended to multiple chemicals
#### Question1: which index to use (a. Substration A+B from AB ;b. Ratio between A+B and AB)?
#### Question2: How to decide the synergism if variation of A+B result in both less and more than AB? 

getwd()
setwd('C:/Users/40355/Documents/R')
library(ggplot2)
test <- read.csv("test-527.csv", header=TRUE)

### Calculate the effect index by substraction
row_number5 <- 1:nrow(test)
for (i in row_number5) {
  if (test[i,2]<0 && test[i, 3]<0) {
    test$test[i] <- 1
    test$diff[i] <- test[i, 2]-test[i,3]
  }
  else{
    if(test[i,2]>0 && test[i, 3]>0) {
      test$test[i] <- 2
      test$diff[i] <- test[i, 3]-test[i,2]
    }
    else{
      if(test[i,2]>0) {
        test$test[i] <- 3
        test$diff[i] <- -(abs(test[i, 3])+abs(test[i,2]))
      }
      else{
        test$test[i] <- 4
        test$diff[i] <- abs(test[i, 3])+abs(test[i,2])
      }
    }
  }
}

### Decide synergism based on substraction
for (i in row_number5) {
  if (test$diff[i]>0) {
    test$synergism[i] <- "S"
  }
  else{
    test$synergism[i] <- "A"
  }
}

### Calculate the effect index based on ratio
for (i in row_number5) {
  if (test[i,2]<0 && test[i, 3]<0 || test[i,2]>0 && test[i, 3]>0) {
    test$test2[i] <- 1
    test$ratio[i] <- test[i, 2]/test[i,3]
  }
  else{
    test$test2[i] <- 2
    test$ratio[i] <- abs(test[i, 3])/abs(test[i,2])
  }
}

### I don't yet know how to define these part of data
index <- test$test2 %in% 2
test$synergism[index] <- "N/A"

row_number4 <- 1:((nrow(test))%/%16)

### A clearer summary on the data
Summary <- data.frame(ID=c(1:28), ratio=c(1:28), synergism=c(1:28))
Summary$ID <- unique(test$ID)
for (i in row_number4){
  a <- str_count(test$synergism[(1+(i-1)*16):(1+i*15)], "A")
  b <- str_count(test$synergism[(1+(i-1)*16):(1+i*15)], "S")
  Summary$ratio[i]<-sum(a)/(sum(b)+sum(a))
  if (Summary$ratio[i]==0){
    Summary$synergism[i] <- "S"
  } 
  else {
    if (Summary$ratio[i]==1){
      Summary$synergism[i] <- "A"
    }
    else {
      Summary$synergism[i] <- "N/A"
    }
  }
} 

write.csv(Summary,"C:/Users/40355/Documents/R\\summary-527.csv", row.names = FALSE)
