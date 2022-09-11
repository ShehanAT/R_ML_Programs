library(tidyverse)
library(caret)
library(dslabs)
data(heights)


# define the outcome and predictors
y <- heights$sex 
x <- heights$height 

# generate training and test sets 
set.seed(2, sample.kind = "Rounding") # if using R 3.5 or earlier, remove the sample.kind argument
