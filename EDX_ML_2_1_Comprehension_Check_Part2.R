library(caret)
data(iris)
iris <- iris[-which(iris$Species == 'setosa'),]
y <- iris$Species

set.seed(2, sample.kind="Rounding")
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

# print(train)

most_accurate_feature <- function(x){
  rangedValues <- seq(range(x)[1], range(x)[2], by=0.1)
  sapply(rangedValues, function(i){
    y_hat <- ifelse(x > i, 'virginica', 'versicolor')
    mean(y_hat==train$Species)
  })
}

predictions <- apply(train[,-5], 2, most_accurate_feature)
# print(sapply(predictions, max))

# Q9
predictions <- most_accurate_feature(train[,3])
rv <- seq(min(train[,3]), max(train[,3]), by=0.1) # rv = ranged values 
cutoffs <- rv[which(predictions == max(predictions))]

y_hat <- ifelse(test[,3] > cutoffs[1], 'virginica', 'versicolor')
# print(mean(y_hat == test$Species))

# Q10
most_accurate_feature_test <- function(x){
  rangedValues <- seq(range(x)[1], range(x)[2], by=0.1)
  sapply(rangedValues, function(i){
    y_hat <- ifelse(x > i, 'virginica', 'versicolor')
    mean(y_hat==test$Species)
  })
}

test_predictions <- apply(test[,-5], 2, most_accurate_feature_test)
print(sapply(test_predictions, max))

