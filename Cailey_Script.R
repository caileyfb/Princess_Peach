
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


# playing around with SVM
set.seed(10111)
x = matrix(rnorm(40), 20, 2)
y = rep(c(-1, 1), c(10, 10))
x[y == 1,] = x[y == 1,] + 1
plot(x, col = y + 3, pch = 19)


dat = data.frame(x, y = as.factor(y))
svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE)
print(svmfit)

plot(svmfit, dat)

make.grid = function(x, n = 75) {
  grange = apply(x, 2, range)
  x1 = seq(from = grange[1,1], to = grange[2,1], length = n)
  x2 = seq(from = grange[1,2], to = grange[2,2], length = n)
  expand.grid(X1 = x1, X2 = x2)
}

xgrid = make.grid(x)
xgrid[1:10,]
