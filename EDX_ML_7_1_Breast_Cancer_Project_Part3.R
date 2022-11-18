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

