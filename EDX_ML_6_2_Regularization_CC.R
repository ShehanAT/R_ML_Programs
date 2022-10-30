library(dslabs)
library(tidyverse)
library(caret)


# set.seed(1986) # if using R 3.5 or earlier 
set.seed(1986, sample.kind="Rounding") # if using R 3.6 or later
n <- round(2^rnorm(1000, 8, 1))

set.seed(1, sample.kind="Rounding")
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS", 1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

schools %>% top_n(10, quality) %>% arrange(desc(quality))

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later 
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores 
})

schools <- schools %>% mutate(score = sapply(scores, mean))

# Q1

schools %>% top_n(10, score) %>% arrange(desc(score)) %>% select(id, size, score)

# Q2

median(schools$size)
schools %>% top_n(10, score) %>% .$size %>% median()

# Q3
median(schools$size)
schools %>% top_n(-10, score) %>% .$size %>% median()

# Q4

schools %>% ggplot(aes(size, score)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(data = filter(schools, rank <= 10), col=2)

# We can see that the standard error of the score has larger variability when the school is smaller. 
# This is a basic statistical reality we learned in PH125.3x: Data Science: Probability and PH125.4x: Data Science: Inference and Modeling courses!
# Note also that several of the top 10 schools based on true quality are also in the top 10 schools based on the exam score:
# schools %>% top_n(10, score) %>% arrange(desc(score))

# Q5

overall <- mean(sapply(scores, mean))
alpha <- 25
score_reg <- sapply(scores, function(x){ overall + sum(x - overall) / (length(x) + alpha) })
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

# Q6

alphas <- seq(10, 250)
rmse <- sapply(alphas, function(alpha) {
  score_reg <- sapply(scores, function(x){ overall + sum(x - overall) / (length(x) + alpha) })
  mean((score_reg - schools$quality)^2)
})
plot(alphas, rmse)
alphas[which.min(rmse)]

# Q7

alpha <- alphas[which.min(rmse)]
score_reg <- sapply(scores, function(x) {
  overall+sum(x - overall) / (length(x) + alpha)
})
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

# Q8

alphas <- seq(10, 250)
rmse <- sapply(alphas, function(alpha) {
  score_reg <- sapply(scores, function(x){ sum(x) / (length(x) + alpha) })
  mean((score_reg - schools$quality)^2)
})
plot(alphas, rmse)
alphas[which.min(rmse)]




  




