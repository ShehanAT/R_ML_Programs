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

# Q7

# The function t(x) returns the transpose of matrix x 

identical(t(s$u %*% diag(s$d)), sweep(s$u, 2, s$d, FUN = "*"))

identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))

identical(S$u %*% t(diag(s$d)), sweep(s$u, 2, s$d, FUN = "*"))

identical(s$u %*% diag(s$d), sweep(s$u, 2, s, FUN = "*"))

# Q8
# The notation y[,1] targets the first column of the matrix y in its entirety

y_svd 
y_svd[,1]
y_svd[1]
plot(y_svd[,1], rowMeans(y))

# Q9 

plot(s$v)
my_image(s$v)

# Based on the image plot of s$v we can state that the first column is very close to being a constant,
# which implies that the first column of YV is the sum of the rows of Y multiplied by some constant,
# and is thus proportional to an average 

# Q10 

# The argument ylim sets the y-axis limit for the specified plot
plot(s$u[,1], ylim = c(-0.25, 0.25))

plot(t(s$v[,1]), ylim = c(-0.25, 0.25))

my_image((s$u[, 1, drop=FALSE]*d[1]) %*% (t(s$v[, 1, drop=FALSE]))) 
my_image(y)
# Q11

resid <- y - with(s, (u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE]))
# The function cor(x) is used to produce correlations for the matrix x
my_image(cor(resid), zlim = c(-1, 1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

plot(s$u[,2])
plot(t(s$v[,2]))
# The drop=FALSE argument is used return a matrix instead of a vector
my_image((s$u[, 2, drop=FALSE]*s$d[2]) %*% t(s$v[, 2, drop=FALSE]))
# The function with() evaluates an R expression in an environment constructed from data, 
# possibility modifying the original data(or a copy of the original data)
with(s, my_image((u[, 2, drop=FALSE]*d[2]) %*% t(v[, 2, drop=FALSE])))

# The above with() expression is equivalent to the following R expression:
# my_image((s$u[, 2, drop=FALSE]*s$d[2]) %*% t(s$v[, 2, drop=FALSE]))


# Q12 

plot(s$u[,3], ylim = c(-0.5, 0.5))
plot(s$u[,3], ylim = c(-0.25, 0.25))

plot(t(s$v[,3]), ylim = c(-0.5, 0.5))
plot(t(s$v[,3]), ylim = c(-0.25, 0.25))

my_image((s$u[, 3, drop=FALSE]*s$d[3]) %*% t(s$v[, 3, drop=FALSE]))
my_image(resid)

# Q13

resid <- y - with(s, sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(cor(resid), zlim = c(-1, 1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

range(y)

my_image(y)
my_image(y, zlim = range(y))

y_hat <- with(s, sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(y_hat, zlim = range(y)) # <- Creating an image of the y_hat variable yields the correct answer
my_image(y - y_hat, zlim = range(y))

my_image(((s$u[, 1, drop=FALSE]*s$d[1]) %*% t(s$v[, 1, drop=FALSE])) +
           (s$u[, 2, drop=FALSE]*s$d[2]) %*% t(s$v[, 2, drop=FALSE]) + 
           (s$u[, 3, drop=FALSE]*s$d[3]) %*% t(s$v[, 3, drop=FALSE]))




