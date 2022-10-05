library(ggplot2)
library(caTools)
library(ROCR) 
library(sjPlot)
library(magrittr)
library(tidyverse)

# Splitting dataset
split <- sample.split(mtcars, SplitRatio = 0.8)
split

train_reg <- subset(mtcars, split == "TRUE")
test_reg <- subset(mtcars, split == "FALSE")

x_weight <- seq(0, 6, 0.1)



# Training model
logistic_model <- glm(vs ~ wt + disp, 
                      data = train_reg, 
                      family = "binomial")
logistic_model

predict_reg <- predict(logistic_model, 
                       test_reg, type = "response")

plot(mtcars$wt, predict_reg, pch=16, xlab="WEIGHT (g)", ylab="VS")
table(mtcars$vs, predict_reg)

length(mtcars$vs)
length(mtcars$wt)
length(predict_reg)

train_reg %>%
  ggplot(aes(vs, predict_reg)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) 
# +
  # labs(
  #   title = "Logistic Regression Model", 
  #   x = "Plasma Glucose Concentration",
  #   y = "Probability of being diabete-pos"
  # )
