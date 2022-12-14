library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species 

plot(iris, pch=21, bg=iris$Species)

set.seed(2, sample.kind = "Rounding")
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

petalLengthRange <- seq(range(train$Petal.Length)[1], range(train$Petal.Length)[2], by=0.1)
petalWidthRange <- seq(range(train$Petal.Width)[1], range(train$Petal.Length)[2], by=0.1)

length_predictions <- sapply(petalLengthRange, function(i){
  y_hat <- ifelse(train$Petal.Length > i, 'virginica', 'versicolor')
  mean(y_hat==train$Species)
})

length_cutoff <- petalLengthRange[which.max(length_predictions)]

width_predictions <- sapply(petalWidthRange, function(i){
  y_hat <- ifelse(train$Petal.Length > i, 'virginica', 'versicolor')
  mean(y_hat == train$Species)
})

width_cutoff <- petalWidthRange[which.max(width_predictions)]

y_hat <- ifelse(test$Petal.Length > length_cutoff | test$Petal.Width > width_cutoff, 'virginica', 'versicolor')
mean(y_hat == test$Species)


