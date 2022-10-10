# Q1 
library(rpart)
n <- 1000
sigma <- 0.25
# set.seed(1) # if using R 3.5 or earlier 
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later 
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

fit <- rpart(y ~ ., data = dat)

# Q2 

plot(fit)

# Q3 

dat %>%
  mutate(y_hat = predict(fit)) %>%
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)

# Q4
library(randomForest)
fit <- randomForest(y ~ x, data = dat)
dat %>% 
  mutate(y_hat = predict(fit)) %>%
  ggplot() + 
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

plot(dat)

# Q5

plot(fit)

# Q6 

library(randomForest)
fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
dat %>%
  mutate(y_hat = predict(fit)) %>%
  ggplot() + 
  geom_point(aes(x, y)) + 
  geom_step(aes(x, y_hat), col = "red")



