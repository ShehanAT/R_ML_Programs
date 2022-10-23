# Q1
library(caret)
library(dslabs)
library(tidyverse)


models <- c("glm", "lda",  "naive_bayes",  "svmLinear", 
            "gamboost",  "gamLoess", "qda", 
            "knn", "kknn", "loclda", "gam",
            "rf", "ranger",  "wsrf", "Rborist", 
            "avNNet", "mlp", "monmlp", "gbm",
            "svmRadial", "svmRadialCost", "svmRadialSigma")
# "adaboost" omitted as including it runs into dependency issues with "fastAdaboost"
# set.seed(1) # if using R 3.5 or earlier 
set.seed(1, sample.kind = "Rounding")
data("mnist_27")

fits <- lapply(models, function(model){
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
})

names(fits) <- models

# Q2 

predictions <- sapply(fits, function(model){
                predict(model)
                })

dim(predictions)

# Q3

acc <- colMeans(predictions == mnist_27$train$y)
acc
mean(acc)

# Q4

votes <- rowMeans(predictions == 7)
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)

# Q5

ind <- acc > mean(predictions == mnist_27$train$y)
sum(ind)
models[ind]

# Q6

acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy) ) 
mean(acc_hat)

# Q7 
ind <- acc_hat >= 0.8
votes <- rowMeans(predictions[,ind] == 7)
y_hat <- ifelse(votes >= 0.5, 7, 2)
mean(y_hat == mnist_27$test$y)