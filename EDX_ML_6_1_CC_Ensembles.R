# Q1
library(caret)
library(dslabs)
library(tidyverse)
library(adaboost)

models <- c("glm", "lda",  "naive_bayes",  "svmLinear", 
            "gamboost",  "gamLoess", "qda", 
            "knn", "kknn", "loclda", "gam",
            "rf", "ranger",  "wsrf", "Rborist", 
            "avNNet", "mlp", "monmlp",
            "adaboost", "gbm",
            "svmRadial", "svmRadialCost", "svmRadialSigma")

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

cm <- colMeans(predictions == mnist_27$train$y)
cm
mean(cm)