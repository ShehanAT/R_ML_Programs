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

# In order to calculate the proportion of the sample that are of a specific condition 'M', use the following code:
mean(brca$y == "M")

# In order to calculate the column number with the highest mean and return its index in the dataset, use the following code:
colMean <- apply(brca$x, 2, mean)
which.max(colMean)

colSd <- apply(brca$x, 2, sd)
which.min(colSd)

# Q2
class(brca$x)
class(brca_x)

# A matrix is a collection of data sets arranged in a two dimensional rectangular organisation, its m*n array with similar data type. It has a fixed number of rows and columns. Matrices are homogenous. matrix => [x,] for the row numbers
# A dataframe is a collectionof data tables that contains multiple data types in multiple columns called fields. It is a list of vector of equal length. It is a generalized form of matrix. It has variable number of rows and columns. The data stored must be numeric, character or factor type. DataFrames are heterogeneous. dataframe => x for the row numbers

# The function sweep(x) returns an array obtained from an input array by sweeping out a summary statistic
# The function unlist(x) simplifies a list, x, by producing a vector which contains all the atomic components which occur in x
# The below code is used to convert a matrix into a dataframe:
brca_x <- as.data.frame(unlist(brca[["x"]]))
scale_sweep <- function(x){
  (x - mean(x))/sd(x)
  (x - mean(x))/sd(x)
}  
brca_x1 <- brca_x %>% mutate_all(scale_sweep) # brca_x1 is the scaled matrix 

# Use the above code OR the following code:
x_centered <- sweep(brca$x, 2, colMeans(brca$x))
x_scaled <- sweep(x_centered, 2, colSds(brca$x), FUN="/")
sd(x_scaled[,1])
median(x_scaled[,1])

brca_x1[,1]
sd(brca_x1[,1])
median(brca_x1[,1])

# matrix[,x] <- is used to access the xth column of the matrix 
# matrix[x,] <- is used to access the xth row of the matrix 

# Q3


# The function dist() computes and returns the distance matrix computed by using the specified distance measure to compute the distance between the rows of a data matrix

brca_dist <- dist(brca_x1)
brca_matrix <- as.matrix(brca_dist)[1, brca$y == "B"]
brca_matrix
dist_brca_matrix_1st_col <- mean(brca_matrix[2:length(brca_matrix)])
dist_brca_matrix_1st_col

brca_matrix_malignant <- as.matrix(brca_dist)[1, brca$y == "M"]
dist_brca_matrix_malignant <- mean(brca_matrix_malignant[2:length(brca_matrix_malignant)])
dist_brca_matrix_malignant <- mean(brca_matrix_malignant)
dist_brca_matrix_malignant


length(brca_matrix_malignant)

# Q4 

scaled_matrix_features <- dist(t(brca_x1))
heatmap(as.matrix(scaled_matrix_features))

# Option 1 is the correct answer

# Q5 
class(scaled_matrix_features)
names(as.data.frame(scaled_matrix_features))
# In order to count the number of rows in a dist object: convert it into a matrix then use nrow() on that matrix
scaled_matrix <- as.matrix(scaled_matrix_features)
length(names(as.data.frame(scaled_matrix))) # In order to count the number of names in a matrix use the code on the left side
nrow(scaled_matrix)
length(scaled_matrix_features)
scaled_matrix_hclust <- hclust(scaled_matrix_features)
# The function cutree(x) cuts a tree(often a result from hclust()) into several groups either by specifying the desired number(s) of groups or the cut height(s)
scaled_matrix_cutree <- cutree(scaled_matrix_hclust, k = 5)
# The function split(x) divides a vector into groups according to the function's parameters
# The function names(x) prints the names attribute of the dataset x
# The names attribute of a dataset is typically a vector of character strings of the same length as the rows of the dataset
split(names(scaled_matrix_cutree), scaled_matrix_cutree)


# Q6

pc <- prcomp(brca_x1)
pc

# What proportion of variance is explained by the first principal component?
summary(pc)

# How many principal components are required to explain at least 90% of the variance?
# Note when the Cumulative Proportion value exceeds 0.9 and use that PC number as the answer
plot(summary(pc)$importance[3,])

# Answer provided by course:
pca <- prcomp(brca_x1)
summary(pca)
