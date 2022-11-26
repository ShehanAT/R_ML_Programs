options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
library(dplyr)
library(ISLR)
library(gam)
data(brca)

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_scaled[test_index,]
test_y <- brca$y[test_index]
train_x <- x_scaled[-test_index,]
train_y <- brca$y[-test_index]


# Q9
mean(train_y == "B")

mean(test_y == "B")


# Q10a

predict_kmeans <- function(x, k) {
  centers <- k$centers # extract cluster centers 
  # calculate distance to cluster centers 
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances)) # select cluster with min distance to center 
}

set.seed(3) # This sets a seed for R's random number generator

#The kMeans Clustering algorithm is an unsupervised non-linear algorithm that cluster data based on similarity or similar groups.
# The function kmeans() return 
k <- kmeans(train_x, centers = 2 )

# The function predict_kmeans(x, k) take a matrix of observations(x) and a kMeans object(k) and assigns each row of x to a cluster from k
predict_result <- predict_kmeans(test_x, k)
predict_result

# Option 1: Convert "B" & "M" to 1 & 2 then use mean() to find the overall accuracy between the prediction and the test data

kmeans_preds_1 <- sapply(test_y, function(i) { 
    ifelse(i == "B", as.integer(1), as.integer(2))
})

kmeans_preds_1

mean(kmeans_preds_1 == predict_result)

# OR 
# Option 2: Convert 1 & 1 to "B" & "M" then use mean() to find the overall accuracy between the prediction and the test data

kmeans_preds_2 <- sapply(predict_kmeans(test_x, k), function(i){
  ifelse(i == 1, "B", "M")
})

kmeans_preds_2
test_y

mean(kmeans_preds_2 == test_y)

# Q10b

# The function sensitvity() calculates the sensitivity, specificity or predictive values of a measurement system compared to a reference result


# In order to find the proportion of benign tumors that are correctly identified in the kmeans_preds_2 with reference to test_y use the following code:
sensitivity(data = as.factor(kmeans_preds_2), reference = test_y, positive = "B")

# In order to find the proportion of malignant tumors that are correctly identified in the kmeans_preds_2 with reference to test_y use the follow code:
sensitivity(data = as.factor(kmeans_preds_2), reference = test_y, positive = "M")

# Q11

set.seed(1, sample.kind = "Rounding")

# In order to fit a logistic regression model on the training set with caret::train() using all predictors, use the following code:
logistic_model <- train(
  train_x, train_y, method = "glm", family = "binomial"
)

logistic_model$results$Accuracy

# To make predictions on the test set using the newly trained logistic regression model, use the following code:
yhat <- predict(logistic_model, test_x)

# To calculate the accuracy of the logistic regression model on the test set, use the following code:
mean(yhat == test_y)

# Q12

set.seed(1, sample.kind = "Rounding")

# In order to train an LDA and QDA model on the training set, use the following code:
lda_model <- train(
  train_x, train_y, method = "lda"
)

qda_model <- train(
  train_x, train_y, method = "qda"
)

# In order to make predictions on the test set using each of the above models:
lda_yhat <- predict(lda_model, test_x)

qda_yhat <- predict(qda_model, test_x)

# To calculate the accuracy of the LDA and QDA models on the test set:
mean(lda_yhat == test_y)

mean(qda_yhat == test_y)


# Q13
# Question text: Set the seed to 5, then fit a loess model on the training set with the caret package. you will need to install the `gam` 
# package if you have not yet done so. Use the default tuning grid. This may take several minutes; ignore warnings. 
# Generate predictions on the test set.
# What is the accuracy of the loess model on the test set?
set.seed(5)

train_y

grid <- data.frame(k = seq(101, 301, 25))

train_loess <- train(train_x, train_y, 
                     method = "gamLoess") # exluding the `data` parameter doesn't produce errors
train_loess
y_hat <- predict(train_loess, test_x)
y_hat
confusionMatrix(data = y_hat, reference = test_y)$overall["Accuracy"]


confusionMatrix(data = predict(train_loess, test_x),
                reference = test_x)$overall["Accuracy"]

