library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()

class(mnist$train$images)

x <- mnist$train$images[1:1000,]
y <- mnist$train$labels[1:1000]

# Matrix Notation
length(x[,1])
x_1 <- 1:5
x_2 <- 6:10
cbind(x_1, x_2)
dim(x)
dim(x_1)
dim(as.matrix(x_1))
dim(x)

# Use as.matrix() to convert a vector into a matrix
# Use the function dim() to extract the dimensions of a matrix
# Use the function matrix() to convert a vector into a matrix. The matrix is filled in by column, but we can fill row by by using the `byrow` argument. The function t() can be used to directly transpose a matrix.
# Note that the matrix function recycles values in the vector without warning if the product of columns and rows does not match the length of the vector 
my_vector <- 1:15

# fill the matrix by column 
mat <- matrix(my_vector, 5, 3)
mat 

# fill by row 
mat_t <- matrix(my_vector, 3, 5, byrow = TRUE)
mat_t 
identical(t(mat), mat_t)
matrix(my_vector, 5, 5)
grid <- matrix(x[3,], 28, 28)
image(1:28, 1:28, grid)

# flip the image back 
image(1:28, 1:28, grid[,28:1])

# Row and Column Summaries and Apply

# The function rowSums() computes the sum of each row 
# The function rowMeans() computers the average of each row 
# We can compute the column sums and averages using the functions colSums() and colMeans()
# the matrixStats package adds functions that performs operations on each row or column very efficiently, including the functions rowSds() and colSds()
# The apply() function lets you apply any function to a matrix. The first argment is the matrix, the second is the dimension(1 for rows, 2 for columns), and the third is the function

sums <- rowSums(x)
avg <- rowMeans(x)

data_frame(labels = as.factor(y), row_averages = avg) %>%
  qplot(labels, row_averages, data = ., geom = "boxplot")

avgs <- apply(x, 1, mean)
sds <- apply(x, 2, sd)

# To only keep columns for which the standard deviation is above 60 are kept use the following syntax:
new_x <- x[, colSds(x) > 60]

# if you select one column or one row of a matrix, the result is not a matrix but a vector
# However, you can preserve the matrix class by using the argument drop, as shown below:
class(x[, 1, drop=FALSE])

# The operations used to extract columns 
x[, c(351, 352)]

# The operations used to extract rows
x[c(2, 3), ]

# We can also use logical indexes to determine which columns or rows to keep
new_x <- x[, colSds(x) > 60]

# If you select only one column or only one row, the result is no longer a matrix but a vector. We can preserve the matrix class by using the argument `drop=FALSE`
library(matrixStats)

sds <- colSds(x)
qplot(sds, bins="30", color=I("black"))
image(1:28, 1:28, matrix(sds, 28, 28)[, 28:1])

# Extract columns and rows 
x[, c(351, 352)]
x[c(2, 3), ]
new_x <- x[, colSds(x) > 60]
dim(new_x)
class(x[,1])
dim(x[1,])

# Preserve the matrix class 
class(x[, 1, drop=FALSE])
dim(x[, 1, drop=FALSE])

# To turn matrices into vectors use the as.vector()

# In order to use complex logical operations with matrices such as: Make all the values that are between 6 and 12 into 0's run the following code:
mat <- matrix(1:15, 5, 3)
mat[mat > 6 & mat < 12] <- 0
mat

# In order to binarize data in a matrix, for ex: To turn all the values below 255 divided by 2 to 0 and above it to 1, run the following code:
bin_x <- x
bin_x[bin_x < 255/2] <- 0
bin_x[bin_x > 255/2] <- 1

# We can use logical operations with matrices
mat <- matrix(1:15, 5, 3)
mat[mat > 6 & mat < 12] <- 0

# We can also binarize the data using just matrix operations:
bin_x <- x 
bin_x[bin_x < 255/2] <- 0
bin_x[bin_x > 255/2] <- 1

# Index with matrices
mat <- matrix(1:15, 5, 3)
as.vector(mat)
qplot(as.vector(x), bins=30, color=I("black"))
new_x <- x 
new_x[new_x < 50] <- 0

mat <- matrix(1:15, 5, 3)
mat[mat < 3] <- 0
mat 

mat <- matrix(1:15, 5, 3)
mat[mat > 6 & mat < 12] <- 0
mat

# Binarize the data 
bin_x <- x
bin_x[bin_x < 255/2] <- 0
bin_x[bin_x > 255/2] <- 1
bin_X <- (x > 255/2) * 1


# Vectorization for matrices and matrix algebra operations 
# In R, when we subtract a vector from a matrix: the first element of each vector in subtracted from the first row of the matrix. The second element from the vector is subtracted from the second row of the matrix and so on.

# We can scale each row of a matrix using this simple code:
(x - rowMeans(x)) / rowSds(x)

# In order to scale a column of a matrix, we first have to transpose a matrix, subtract the columns, and then transpose it back. The function sweep() works in a similar way to apply because it takes each entry of a vector and subtracts it from the corresponding row or column.
t(t(x) - colMeans(x))

# Matrix multiplication is done with the following operation: %*%
t(x) %*% x
# or use the function crossprod(x) to give us the cross-product:
crossprod(x)

# To compute the inverse of a function: we can use the function solve()
solve(crossprod(x))

# The QR Decomposition is readily available by using the function qr() like so:
qr(x)




