library(dslabs)
library(caret)
data(mnist_27)
# set.seed(1995) # if R 3.5 or earlier

set.seed(1995, sample.kind="Rounding") # if R 3.6 or later 
indexes <- createResample(mnist_27$train$y, 10)
sum(indexes[[1]] == 3)
sum(indexes[[1]] == 4)
sum(indexes[[1]] == 7)

# Q2
x = sapply(indexes, function(k){
  sum(k == 3)
})
sum(x)

# Q3
set.seed(1)
B <- 1000

q_75 <- replicate(B, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})

mean(q_75)
# sd() is used to find Standard Error
sd(q_75)

# Q4
# set.seed(1) # if R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if R 3.6 or later 

# createResample() creates n number of resamples from the dataset passed to it. 
indexes <- createResample(y, 10000)

q_75_resample <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})

mean(q_75_resample)

sd(q_75_resample)


