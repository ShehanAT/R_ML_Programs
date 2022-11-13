library(tidyverse)
library(caret)
library(dslabs)
data("tissue_gene_expression")

# Q1

d <- dist(tissue_gene_expression$x)

d <- dist(rowMeans(tissue_gene_expression$x))

d <- dist(rowMeans(tissue_gene_expression$y))

d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))
# The above line is the correct answer

d

# Q2


# The function hclust(x) conducts a hierarchical cluster analysis on a set of dissimilarities, x, and methods for analyzing it
h <- hclust(d)
# The cex argument in the plot() function indicates the amount by which plotting text and symbols should be scaled relative to the default. Values over 1 indicate larger, values under 1 indicate smaller 
plot(h, cex=0.75)

# A dendrogram is a diagram that shows the hierarchical relationship between objects. It is mostl commonly created as an output from hierarchical clustering.
# Hierarchical clustering is an algorithm that groups similar objects into groups called clusters

# Q3
library(RColorBrewer)
sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = TRUE)[1:50]
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]

heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = colors)
# The above line is the correct answer 

heatmap(t(tissue_gene_expression$x[,ind]), color = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = rev(colors))

heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = sample(colors))

heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = sample(colors))



