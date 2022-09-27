# With high dimentional data, a quick way to compute all the distances at once is to use the function `dist()`, which computes the distance between each row and produces an object of class `dist()`
d <- dist(x)

# To compute the distance between all pairs of predictors, we can transpose the matrix first and then use `dist()`:
d <- dist(t(x))

library(tidyverse)
library(dslabs)

if(!exists("mnist")) mnist <- read_mnist()
set.seed(0) # if using R 3.5 or earlier 
set.seed(0, sample.kind="Rounding") # if using R 3.6 or later 
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)

# The predictors are in x and the labels in y
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]

y[1:3]


x_1 <- x[1,]
x_2 <- x[2,]
x_3 <- x[3,]

# Distance between two numbers
sqrt(sum((x_1 - x_2)^2))
sqrt(sum((x_1 - x_3)^2))
sqrt(sum((x_2 - x_3)^2))

# Compute distance using matrix algebra
sqrt(crossprod(x_1 - x_2))
sqrt(crossprod(x_1 - x_3))
sqrt(crossprod(x_2 - x_3))

# Compute distance between each row 
d <- dist(x)
class(d)
as.matrix(d)[1:3, 1:3]

# Visualize these distances 
image(as.matrix(d))

# Order the distance by labels
image(as.matrix(d)[order(y), order(y)])

# Compute distance between predictors
d <- dist(t(x))
dim(as.matrix(d))

d_492 <- as.matrix(d)[492,]

image(1:28, 1:28, matrix(d_492, 28, 28))

library(dslabs)
data("tissue_gene_expression")

dim(tissue_gene_expression$x)

table(tissue_gene_expression$y)

# Q1: 
# d <- dist(tissue_gene_expression$x, distance='maximum')

# d <- dist(tissue_gene_expression)

d <- dist(tissue_gene_expression$x)

# d <- cor(tissue_gene_expression$x)

# Q2:

ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind, ind]

# Q3: 
image(d)

image(as.matrix(d))

d

