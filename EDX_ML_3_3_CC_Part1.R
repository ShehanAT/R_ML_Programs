# Q1

x <- matrix(rnorm(1000), 100, 100)

x <- matrix(rnorm(100*10), 100, 10)
 
x <- matrix(rnorm(100*10), 10, 10)

x <- matrix(rnorm(100*10), 10, 100)

dim(x)


x <- sweep(x, 2, 1:nrow(x), "+")

x <- sweep(x, 1, 1:nrow(x), "+")

# Q3: Which of the following lines of code would add the scalar 1 to row 1, the scalar 2 to row 2 and so on for the matrix `x`?
x <- x + seq(nrow(x))
x <- sweep(x, 1, 1:nrow(x), "+")

# Q4: Which of the following lines of code would add the scalar 1 to column 1, the scalar 2 to column 2 and so on for the matrix `x`?
x <- sweep(x, 2, 1:ncol(x), FUN="+")

rowMeans(x)

# Q6
if(!exists("mnist")) mnist <- read_mnist()
y <- rowMeans(mnist$train$images > 50 & mnist$train$images < 205)
qplot(as.factor(mnist$train$labels), y, geom="boxplot")
mean(y)