options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
library(dplyr)

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

# Q10

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
predict_result <- predict_kmeans(train_x, k)
predict_result

