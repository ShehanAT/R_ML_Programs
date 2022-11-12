options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
library(dplyr)

data(brca)

# In order to count the number of samples in the dataset use the nrow() function like so:
nrow(brca$x)

# In order to count the number of predictors in a matrix use the ncol() function like so:
ncol(brca$x)

# In order to calculate the proportion of the sample that are of a specifc condition 'M', use the following code:
mean(brca$y == "M")

# In order to calculate the column number with the highest mean and return its index in the dataset, use the following code:
colMean <- apply(brca$x, 2, mean)
which.max(colMean)

colSd <- apply(brca$x, 2, sd)
which.min(colSd)