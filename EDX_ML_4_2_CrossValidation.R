library(tidyverse)
library(caret)

set.seed(1996, sample.kind="Rounding")
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep=" ")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[, sample(p, 100)]

fit <- train(x_subset, y)
fit$results

fit <- train(x_subset, y, method = "glm")
fit$results 

# fit <- train(y, x_subset, method = "glm")
# fit$results
# 
# fit <- test(x_subset, y, method="glm")
# fit$results

# Q2

install.packages("BiocManager")
BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)

# Q3
ind <- which(pvals <= 0.01)
length(ind)

# Q4 
x_subset <- x[,ind]
fit <- train(x_subset, y, method="glm")
fit$results

# Q5 
fit <- train(x_subset, y, method="knn", tuneGrid=data.frame(k = seq(101, 301, 25)))
ggplot(fit)

fit <- train(x_subset, y, method="knn")
ggplot(fit)

fit <- train(x_subset, y, method="knn", tuneGrid=data.frame(k=seq(103, 301, 25)))
ggplot(fit)

fit <- train(x_subset, y, method="knn", tuneGrid=data.frame(k=seq(101, 301, 5)))
ggplot(fit)

# Q7 
data('tissue_gene_expression')
fit <- with(tissue_gene_expression, train(x, y, method="knn", tuneGrid=data.frame(k = seq(1, 7, 2))))
ggplot(fit)


