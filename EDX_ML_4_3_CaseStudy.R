if(!exist('mnist'))mnist <- read_mnist()

set.seed(3456) # use set.seed(3456, sample.kind="Rounding") in R 3.6 or later 
index_127 <- sample(which(mnist$train$labels %in% c(1,2,7)), 2000)
y <- mnist$train$labels[index_127]
x <- mnist$train$images[index_127,]
index_train <- createDataPartition(y, p=0.8, list=FALSE)

# Get the quadrants
# Temporary object to help figure out the quadrants
row_column <- expand.grid(row=1:28, col=1:28)
upper_left_ind <- which(row_column$col <= 14 & row_column$row <= 14)
lower_right_ind <- which(row_column$col > 14 & row_column$row > 14)

# Binarize the values. Above 200 is ink, below is no ink
x <- x > 200

