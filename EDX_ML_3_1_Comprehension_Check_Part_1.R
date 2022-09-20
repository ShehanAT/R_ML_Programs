
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


