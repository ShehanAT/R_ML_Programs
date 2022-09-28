# Q1 

library(caret)
library(dslabs)

set.seed(1)

test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)

train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

k <- seq(1, 101, 3)

F1_scores <- sapply(k, function(k){
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <-  predict(fit, test_set, type="class") %>%
    factor(levels = levels(train_set$sex))
                
  F_meas(data=y_hat, reference=test_set$sex)
})

# F1_scores
max(F1_scores)

k[which.max(F1_scores)]

# Q2

library(caret)
data("tissue_gene_expression")

set.seed(1)

x <- tissue_gene_expression$x 
y <- tissue_gene_expression$y

train_index <- createDataPartition(y, list = FALSE)

k = seq(1, 11, 2)

tissue_types <- sapply(k, function(k){
  fit <- knn3(x[train_index,], y[train_index], k = k)
  y_hat <-  predict(fit, newdata=data.frame(x=x[-train_index,]), type="class") 
  
  mean(y_hat == y[-train_index])
})

tissue_types