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
logistic_yhat <- predict(logistic_model, test_x)

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
loess_y_hat <- predict(train_loess, test_x)
loess_y_hat
confusionMatrix(data = loess_y_hat, reference = test_y)$overall["Accuracy"]


confusionMatrix(data = predict(train_loess, test_x),
                reference = test_x)$overall["Accuracy"]


# Q14

set.seed(7)

for(x in c(3, 5, 7, 9, 11, 13, 15, 17, 19, 21)){
  knn_fit <- train(train_x, train_y, method = "knn", k = c(3, 5, 7, 9, 11, 13, 15, 17, 19, 21))
  y_hat_knn <- predict(knn_fit, test_x, type="class")
  
}

ctrl <- trainControl(method = "repeatedcv", repeats = 3)
knn_fit <- train(train_x, train_y, method = "knn", trControl = ctrl, preProcess = c("center", "scale"), tuneLength = 20)
knn_fit
y_hat_knn <- predict(knn_fit, test_x)
confusionMatrix(y_hat_knn, test_y)$overall["Accuracy"]


# Q15a 

set.seed(9)

train_rf <- train(train_x, train_y, method = "rf", tuneGrid = data.frame(mtry = c(3, 5, 7, 9)), importance = TRUE)
yhat_rf <- predict(train_rf, test_x)
train_rf
varImp(train_rf) # Use the varImp() function to find the variable importance of each of the features in the train set
confusionMatrix(yhat_rf, test_y)$overall["Accuracy"]

# Q16

# Create an ensemble using the predictions from the 7 models created in the previous exercises: k-means, logistic regression,
# LDA, QDA, loess, k-nearest neighbors, and random forest. Use the ensemble to generate a majority prediction of tumor type
# (if most model suggest the tumor is malignant, predict malignant).
# What is the accuracy of the ensemble prediction?

ensemble <- cbind(kmeans = predict_result == "B", glm = logistic_yhat == "B", lda = lda_yhat == "B", qda = qda_yhat == "B", loess = loess_y_hat == "B", knn = y_hat_knn == "B", rf = yhat_rf == "B")

ensemble_yhat <- ifelse(rowMeans(ensemble) > 0.5, "B", "M")

mean(ensemble_yhat == test_y)

# Q16b 
names(ensemble)
table(ensemble)

models <- c("kmeans", "logistic regression", "LDA", "QDA", "Loess", "KNN", "RandomForest")

model_accuracy <- c(mean(predict_result == test_y),
            mean(logistic_yhat == test_y),
            mean(lda_yhat == test_y),
            mean(qda_yhat == test_y),
            mean(loess_y_hat == test_y),
            mean(y_hat_knn == test_y),
            mean(yhat_rf == test_y))

models_by_acc <- data.frame(model=models, accuracy=model_accuracy)
models_by_acc