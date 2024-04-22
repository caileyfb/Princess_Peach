#packages
library(readr)
full <- read_csv("diagnosing_AD_data.csv")
library(dplyr) #needed for the %>% function in the data subset loops
library(ggplot2)
library(lattice)

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

svm_task1 <- svm(as.factor(class) ~ ., data = Task_1)
summary(svm_task1)
table(Task_1$class, predict(svm_task1, Task_1, type = "class"))

#SVM with grid search
set.seed(2024)
library(caret)
svm_model <- function(task, kern){
  
  #divide data
  H <- which(task$class == "H") 
  P <- which(task$class == "P")
  
  n_tran_H <- ceiling(0.8 * length(H))
  n_train_P <- ceiling(0.8 * length(P))
  
  train_H <- sample(H, size = n_train_H)
  train_P <- sample(P, size = n_train_P)
  
  train_both <- c(train_H, train_P)
  train_ID <- task[train_both, ]
  test_ID <- task[-train_both, ]
  
  #remove ID
  train <- subset(train_ID, select = -ID)
  test <- subset(test_ID, select = -ID)
  
  #grid search
  costs <- seq(from = 0.5, to = 1.5, by = 0.1)
  
  tune_cost <- tune(svm, as.factor(class) ~ ., data = train, ranges = list(cost = costs), kernel = kern, cross =5)
  
  best_cost <- tune_cost$best.parameters$cost
  
  svm_model <- svm(as.factor(class) ~ ., data = train, kernel = kern, cost = best_cost, gamma = 1)
  
  testing <- predict(svm_task1, test)
  
  confusionMatrix(testing, as.factor(test$class))
  
  summary(svm_task1)
  }

svm_model(Task_1, "linear")
  
#SVM task 1
  
#divide data
H <- which(Task_1$class == "H") 
P <- which(Task_1$class == "P")

n_train_H <- ceiling(0.8 * length(H))
n_train_P <- ceiling(0.8 * length(P))
  
train_H <- sample(H, size = n_train_H)
train_P <- sample(P, size = n_train_P)
  
train_both <- c(train_H, train_P)
train_ID <- Task_1[train_both, ]
test_ID <- Task_1[-train_both, ]
  
  #remove ID
train <- subset(train_ID, select = -ID)
test <- subset(test_ID, select = -ID)
  
  #grid search
costs <- seq(from = 0.5, to = 1.5, by = 0.1)
  
tune_cost <- tune(svm, as.factor(class) ~ ., data = train, ranges = list(cost = costs), kernel = "radial", cross =5)
  
best_cost <- tune_cost$best.parameters$cost

svm_task1 <- svm(as.factor(class) ~ ., data = train, kernel = "radial", cost = best_cost, gamma = 1)

testing <- predict(svm_task1, test)

confusionMatrix(testing, as.factor(test$class))

summary(svm_task1)

