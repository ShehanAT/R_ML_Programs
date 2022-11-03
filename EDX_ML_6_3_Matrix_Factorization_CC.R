library(dslabs)
library(tidyverse)
library(caret)


# Q1

# In the code below we are constructing a dataset that represents grade scores for 100 students in 24 different subjects
# A 0 represents an average grade(C), a 25 is a high grade(A+), and a -25 represents a low grade(F)

set.seed(1987)
# if using R 3.6 or later, use `set.seed(1987, sample.kind="Rounding")` instead 
n <- 100 
k <- 8
Sigma <- 64 * matrix(c(1, 0.75, 0.5, 0.75, 1, 0.5, 0.5, 0.5, 1), 3, 3)
m <- MASS::mvrnorm(n ,rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math", k), 1:k, sep="_"),
                 paste(rep("Science", k), 1:k, sep="_"),
                 paste(rep("Arts", k), 1:k, sep="_"))

my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="", col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)

# Based on the image the above code produces: The students that test well are at the top of the image
# and there seem to be three groupings by subject

# Q2 

my_image(cor(y), zlim = c(-1, 1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

# Based on the graph the above code produces: There is correlation among all tests, but higher if the 
# tests are in science and math and even higher within each subject

# Q3

# The function svd() is used to compute the singular value decomposition(SVD) of y. This function will
# return U, V and the diagonal entries of D 

s <- svd(y)
names(s)

y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))

y_sq <- y * y
# colSums function is used to compute the sums of matrix or array columns
colMean_y <- colSums(y_sq)
ss_y <- sum(colMean_y)

y_svd_sq <- y_svd * y_svd
colMean_yv <- colSums(y_svd_sq)
ss_yv <- sum(colMean_yv)

ss_y

ss_yv

# Q4

plot(ss_y)

plot(ss_yv)

# Based on the plots that the above code produces we can see that the variability of the columns of 
# of YV is decreasing. Furthermore, we see that, relative to the first three, the variability of the 
# columns beyond the third is almost 0.

# Q6
# The operator %*% denotes matrix multiplication 
# The function cumsum() returns a vector whose elements are the cumulative sums, products, minima or maxima of the elements of the argument

YV_t <- y %*% s$v
var_explained <- cumsum(sd(YV_t[,1])^2/sum(var(YV_t))) + cumsum(sd(YV_t[,2])^2/sum(var(YV_t))) + cumsum(sd(YV_t[,3])^2/sum(var(YV_t)))
var_explained

var_explained <- sum(s$d[1:3]^2) / sum(s$d^2)
var_explained

