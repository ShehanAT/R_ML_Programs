# Q1

library(rpart)
library(caret)
library(dslabs)
data("tissue_gene_expression")

set.seed(1991) 

# Q2 

fit_rpart <- with(tissue_gene_expression,
            train(x, y, method="rpart",
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)), control = rpart.control(minsplit = 0)))

ggplot(fit_rpart)

confusionMatrix(fit_rpart)

# Q3

plot(fit$finalModel)
text(fit$finalModel)

# Q4

fit <- with(tissue_gene_expression,
            train(x, y, method = "rf", 
                  tuneGrid = data.frame(mtry = seq(50, 200, 25)), nodesize = 1))

ggplot(fit)

confusionMatrix(fit)

# Q5 
imp <- varImp(fit)
imp

# Q6
tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms
