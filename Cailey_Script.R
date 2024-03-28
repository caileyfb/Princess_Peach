#packages
library(readr)
full <- read_csv("diagnosing_AD_data.csv")
library(dplyr) #needed for the %>% function in the data subset loops
library(ggplot2)

#accessible data
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

#separating the tasks

d <- 2

for (i in (1:25)) {
  df <- data.frame(access[1], access[d:(d+6)])
  df <- df %>%
    mutate(access[177])
  
  assign( paste("Task", i, sep = "_"), df)
  d <- d+7
}


# playing around with SVM
set.seed(2024)
x = matrix(rnorm(40), 20, 2)
y = rep(c(-1, 1), c(10, 10))
x[y == 1,] = x[y == 1,] + 1
plot(x, col = y + 3, pch = 19)

dat = data.frame(x, y = as.factor(y))
svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE)
print(svmfit)

#SVM test model data, task 1
library(e1071)
set.seed(2024)

