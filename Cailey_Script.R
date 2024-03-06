
library(readr)
full <- read_csv("diagnosing_AD_data.csv")
library(dplyr) #needed for the %>% function in the data subset loops
library(ggplot2)

a=1
b=6
c=15
access <- data.frame(matrix(nrow=174))

for (i in 1:25) {
  access <- access %>%
    mutate(full[a:(a+2)], full[b:(b+1)], full[c:(c+1)])
  
  a <- a+18
  b <- b+18
  c <- c+18
  
}

access <- access %>%
  mutate(full[451:452])

access <- subset(access, select = -(matrix.nrow...174.))

d <- 2

for (i in (1:25)) {
  df <- data.frame(access[1], access[d:(d+6)])
  df <- df %>%
    mutate(access[177])
  
  assign( paste("Task", i, sep = "_"), df)
  d <- d+7
}

