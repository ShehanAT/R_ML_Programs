library(tidyverse)
library(caret)
library(dslabs)
data(heights)

mnist <- read_mnist()
x = data.frame(mnist)
print(ncol(x))