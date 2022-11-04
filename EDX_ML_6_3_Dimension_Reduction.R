library(tidyverse)
library(caret)
library(dslabs)

data("tissue_gene_expression")
dim(tissue_gene_expression$x)

d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))
d

tissue_gene_expression$x[1,]
length(tissue_gene_expression$x[1,])
tissue_gene_expression$x[,1]
length(tissue_gene_expression$x[,1])

# The function cbind() takes a sequence of vectors, matrices or data-frames and combines them by columns and rows respectively 
cbind_result <- cbind(tissue_gene_expression$x[,1], tissue_gene_expression$x[,2])
dim(cbind_result)

# The function prcomp(x) performs a principal matrix analysis on the given matrix x and outputs a result of the class `prcomp`
pc_result <- prcomp(tissue_gene_expression$x)
data_frame(pc_1=pc_result$x[,1], pc_2=pc_result$x[,2],
           tissue=tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

# Q2

# tissue_gene_expression$x[,1]
# 
# tissue_mean_value <- sapply(tissue_gene_expression$x[,1], function(value) mean(value))
# 
# tissue_mean_value

# In order to compute the mean value for all values in a given vector use the rowMeans() or colMeans() functions

avgs <- rowMeans(tissue_gene_expression$x)
df_res <- data.frame(pc_1 = pc_result$x[,1], avg = avgs,
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, avg, color = tissue)) +
  geom_point()
# In order to compute the correlation between the average across all predictors and the first PC(principal component)
# we need to utilize the cor() function as shown below:
cor(avgs, pc_result$x[,1])
# The ordering of the arguments don't influence the output
cor(pc_result$x[,1], avgs)



# Q3


# BLANK
x <- with(tissue_gene_expression, sweep(x, 1, mean(x)))

# The function sweep() is used to apply various operations to a matrix's rows or columns
x <- sweep(x, 1, rowMeans(tissue_gene_expression$x))

x <- tissue_gene_expression$x - mean(tissue_gene_expression$x)

x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x))) # This is the correct answer for Q3



pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2],
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

# Q4 

class(pc$x[,1])
class(tissue_gene_expression$y)

for (i in 1:10){
  boxplot(pc$x[,i] ~ tissue_gene_expression$y, xlab="Tissue Type", ylab="Median Difference", main = paste("PC", i))
}

boxplot(pc$x[,7] ~ tissue_gene_expression$y, xlab="Tissue Type", ylab="Median Difference", main = "PC 7")

# Q5

# The summary.imp function returns data that contained substituted data in order to replace any missing
# data
summary(pc)
summary(pc)$imp
df_pc <- data.frame(summary(pc)$imp) 


