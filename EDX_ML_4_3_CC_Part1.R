# Q1
library(dslabs)
library(caret)
library(tidyverse)
data("tissue_gene_expression")

# set.seed(1993) # if using R 3.5 or earlier 
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later 
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

fit_lda <- train(x, y, method="lda")
fit_lda$results["Accuracy"]

# Q2
t(fit_lda$finalModel$means) %>% data.frame() %>% 
  mutate(predictor_name=rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label=predictor_name)) + 
  geom_point() + 
  geom_text() +
  geom_abline()

# Q3
library(dslabs)
library(caret)
library(tidyverse)
data("tissue_gene_expression")

# set.seed(1993) # if using R 3.5 or earlier 
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later 
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

fit_qda <- train(x, y, method="qda")
fit_qda$results["Accuracy"]

# Q4

t(fit_qda$finalModel$means) %>% data.frame() %>% 
  mutate(predictor_name=rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label=predictor_name)) + 
  geom_point() + 
  geom_text() +
  geom_abline()

# Q5

library(dslabs)
library(caret)
data("tissue_gene_expression")

# set.seed(1993) # if using R 3.5 or earlier 
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later 
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

fit_lda <- train(x, y, method = "lda", preProcess = "center")
fit_lda$results["Accuracy"]

t(fit_lda$finalModel$means) %>% data.frame() %>% 
  mutate(predictor_name=rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label=predictor_name)) + 
  geom_point() + 
  geom_text() +
  geom_abline()

# Q6 
library(dslabs)
library(caret)
data("tissue_gene_expression")

# set.seed(1993) # if using R 3.5 or earlier 
set.seed(1993, sample.kind="Rounding")
y <- tissue_gene_expression$y 
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

fit_lda <- train(x, y, method="lda", preProcess="center")
fit_lda$results$Accuracy

t(fit_lda$finalModel$means) %>% data.frame() %>% 
  mutate(predictor_name=rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label=predictor_name)) + 
  geom_point() +
  geom_text() +
  geom_abline()

