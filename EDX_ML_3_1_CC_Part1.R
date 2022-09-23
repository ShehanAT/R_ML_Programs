# Q1 
library(tidyverse)
library(caret)

# set.seed(2) # if you are using R 3.5 or earlier 
set.seed(2, sample.kind="Rounding")
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1, sigma_1 = 1){
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x=x, y=as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}

dat <- make_data()

set.seed(1, sample.kind="Rounding")

delta <- seq(0, 3, len=25)

res <- sapply(delta, function(i){
  # test_seq <- seq(0, 3, len=25)
  current_dataset <- make_data(mu_1 = i)
  glm_fit <- current_dataset$train %>% glm(y ~ x, data=., family="binomial")
  y_hat_logit <- ifelse(predict(glm_fit, current_dataset$test) > 0.5, 1, 0) %>% factor(levels = c(0, 1))
  # Factors in R programming are data structures that are implemented to categorize the data or represent 
  # categorical data and store it on multiple levels
  # The factor() function is used to encode a vector as a factor
  # The infix(%>%) works like a pipe. It passes the left hand side of the operator to the first argument of the right hand side of the operator
  # 

  mean(y_hat_logit == current_dataset$test$y)
})

# qplot() is similar to plot() with the additional that it can be used to create and combine different types of plots easily
qplot(delta, res)