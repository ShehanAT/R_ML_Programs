
# Q1 
# We will build 100 linear models using the data above and calculate the mean and standard deviation of the 
# combined models. First, set the seed to 1 again (make sure to use sample.kind="Rounding" if your 
# R is version 3.6 or later). Then, within a replicate() loop, (1) partition the dataset into test 
# and training sets with p = 0.5 and using dat$y to generate your indices, (2) train a linear model 
# predicting y from x, (3) generate predictions on the test set, and (4) calculate the RMSE of that model.
# Then, report the mean and standard deviation (SD) of the RMSEs from all 100 models.
# 
# Report all answers to at least 3 significant digits.
library(tidyverse)
library(caret)
library(magrittr)


# set.seed(1) # if using R 3.5 or earlier 
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later 
n <- 100 
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1, sample.kind="Rounding")
rmse <- replicate(100, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  
  fit <- lm(y ~ x, data=train_set)
  
  y_hat <- predict(fit, newdata=test_set)
  sqrt(mean((y_hat - test_set$y)^2))
})

mean(rmse)

sd(rmse)


# Q2
# Now we will repeat the exercise above but using larger datasets. Write a function that takes a size n, then (1) builds a dataset using the code provided at the top of Q1 but with n observations instead of 100 and without the set.seed(1), (2) runs the replicate() loop that you wrote to answer Q1, which builds 100 linear models and returns a vector of RMSEs, and (3) calculates the mean and standard deviation of the 100 RMSEs.
# 
# Set the seed to 1 (if using R 3.6 or later, use the argument sample.kind="Rounding") and then use sapply() or map() to apply your new function to n <- c(100, 500, 1000, 5000, 10000).
# 
# Note: You only need to set the seed once before running your function; do not set a seed within your function. Also be sure to use sapply() or map() as you will get different answers running the simulations individually due to setting the seed.

# sapply() function takes a list, vector, or data frame as input and gives output in vector or matrix format. It is useful for operations on list objects and returns a list object of same length of original set. Sapply function in R does the same job as lapply() function but returns a vector


# set.seed(1) # if using R 3.5 or earlier 
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later 
n <- c(100, 500, 1000, 5000, 10000)


# set.seed(1, sample.kind="Rounding")

replicator_function <- function(n){
  
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  
  rmse <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    
    fit <- lm(y ~ x, data=train_set)
    
    y_hat <- predict(fit, newdata=test_set)
    sqrt(mean((y_hat - test_set$y)^2))
  })
  
  c(avg = mean(rmse), sd = sd(rmse))
}

result <- sapply(n, replicator_function)

result


# Q4 
# Now repeat the exercise from Q1, this time making the correlation between x and y larger, as in the following code:

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
set.seed(1, sample.kind="Rounding")
rmse <- replicate(100, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  
  fit <- lm(y ~ x, data=train_set)
  
  y_hat <- predict(fit, newdata=test_set)
  sqrt(mean((y_hat - test_set$y)^2))
})

rmse

mean(rmse)

sd(rmse)

# Q6


# set.seed(1) # if using R 3.5 or earlier
# set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

rmse_x_1 <- replicate(1, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  
  fit <- lm(y ~ x_1, data=train_set)
  
  y_hat <- predict(fit, newdata=test_set)
  sqrt(mean((y_hat - test_set$y)^2))
})

print("rmse_x_1: ")
rmse_x_1


rmse_x_2 <- replicate(1, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  
  fit <- lm(y ~ x_2, data=train_set)
  
  y_hat <- predict(fit, newdata=test_set)
  sqrt(mean((y_hat - test_set$y)^2))
})

print("rmse_x_2: ")
rmse_x_2

rmse_x_1_x_2 <- replicate(1, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  
  fit <- lm(y ~ x_1 + x_2, data=train_set)
  
  y_hat <- predict(fit, newdata=test_set)
  sqrt(mean((y_hat - test_set$y)^2))
})

print("rmse_x_1_x_2: ")
rmse_x_1_x_2
