p <- 0.9 
n <- length(test_index)

y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prod = c(p, 1-p)) %>%
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)