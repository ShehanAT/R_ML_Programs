library(titanic) # Loads the titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)
library(caTools)
library(ROCR) 
library(sjPlot)

# 3 significant digits 
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package 
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>% # count family members 
  select(Survived, Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

# Splitting dataset
set.seed(42)

test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE)

train_set <- titanic_clean %>% slice(-test_index)
test_set <- titanic_clean %>% slice(test_index)

nrow(train_set)
nrow(test_set)

# To find the proportion of passengers in the train set that survived:
mean(train_set$Survived == 1)

# Q3 

set.seed(3, sample.kind = "Rounding")
guess_ <- sample(c(0, 1), nrow(test_set), replace=TRUE)
test_set %>%
  filter(Survived == guess_) %>%
  summarize(n() / nrow(test_set))

# OR

set.seed(3, sample.kind="Rounding")
guess <- sample(c(0, 1), nrow(test_set), replace=TRUE)
mean(guess == test_set$Survived)

# Q3a 
f_train <- train_set %>% filter(Sex == "female")
f_survived <- mean(f_train$Survived == 1)
f_survived
m_train <- train_set %>% filter(Sex == "male")
m_survived <- mean(m_train$Survived == 1)
m_survived

# Q3b 
sex_model <- ifelse(test_set$Sex == "female", 1, 0)
mean(sex_model == test_set$Survived)

# Q4a

class_results <- train_set %>% group_by(Pclass) %>%
  summarize(p1 = mean(Survived == 1))

# Q4b

class_model <- ifelse(test_set$Pclass == 1, 1, 0)
mean(class_model == test_set$Survived)

second_class_results <- ifelse(test_set$Pclass == 2, 1, 0)
mean(second_class_results == test_set$Survived)

third_class_results <- ifelse(test_set$Pclass == 3, 1, 0)
mean(third_class_results == test_set$Survived)

# Q4c 

passengers <- train_set %>% group_by(Pclass, Sex) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Survived > 0.5)
passengers

# Q4d 
sex_class_model <- ifelse((test_set$Pclass == 1 & test_set$Sex == "female") | (test_set$Pclass == 2 & test_set$Sex == "female"), 1, 0)
mean(sex_class_model == test_set$Survived)

# Q5a 
confusionMatrix(data = factor(sex_model), reference = factor(test_set$Survived))

confusionMatrix(data = factor(class_model), reference = factor(test_set$Survived))

confusionMatrix(data = factor(sex_class_model), reference = factor(test_set$Survived))

# Q6

F_meas(data = factor(sex_model), reference = factor(test_set$Survived))
F_meas(data = factor(class_model), reference = factor(test_set$Survived))
F_meas(data = factor(sex_class_model), reference = factor(test_set$Survived))

# Q7
set.seed(1)

# ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
# y <- droplevels(tissue_gene_expression$y[ind])
# x <- tissue_gene_expression$x[ind, ]
# x <- x[, sample(ncol(x), 10)]

fit_lda <- train(Survived ~ Fare, data=train_set, method="lda")
prediction <- predict(fit_lda, test_set)
mean(prediction == test_set$Survived)

fit_qda <- train(Survived ~ Fare, data=train_set, method="qda")
prediction <- predict(fit_qda, test_set)
mean(prediction == test_set$Survived)

# Q8

fit_glm <- train(Survived ~ Age, data = train_set, method="glm")
prediction <- predict(fit_glm, test_set)
mean(prediction == test_set$Survived)

fit_glm <- train(Survived ~ Age + Sex + Pclass + Fare, data = train_set, method="glm")
prediction <- predict(fit_glm, test_set)
mean(prediction == test_set$Survived)

fit_glm <- train(Survived ~ ., data = train_set, method="glm")
prediction <- predict(fit_glm, test_set)
mean(prediction == test_set$Survived)

# Q9

k = seq(3, 51, 2)
set.seed(6)

fit_knn <- train(Survived ~ ., data = train_set, method = "knn", tuneGrid = data.frame(k))
fit_knn

ggplot(fit_knn)

survived_hat <- predict(fit_knn, test_set) %>% factor(levels = levels(test_set$Survived))
cn_fit_knn <- confusionMatrix(data = survived_hat, reference = test_set$Survived)
cn_fit_knn$overall["Accuracy"]

# Q10
k = seq(3, 51, 2)

set.seed(8)

fit_knn10 <- train(Survived ~ ., 
      data = train_set, 
      method = "knn", 
      tuneGrid = data.frame(k),
      trControl = trainControl(method = "cv", number = 10, p = 0.9))
# p = 0.9 = 1 - 0.1

fit_knn10

survived_hat <- predict(fit_knn10, test_set)

cn_matrix <- confusionMatrix(data = survived_hat, reference = test_set$Survived)
cn_matrix$overall["Accuracy"]

# Optimal k value: 5
# Accuracy on the test set using the cross-validated kNN model: 0.648

# Q11
set.seed(10, sample.kind = "Rounding")

fit_rpart <- train(Survived ~ .,
                   data = train_set,
                   method = "rpart",
                   tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)))

plot(fit_rpart)

fit_rpart

survived_hat <- predict(fit_rpart, test_set)

rp_matrix <- confusionMatrix(data = survived_hat, reference = test_set$Survived)
fit_rpart$finalModel
plot(fit_rpart$finalModel, margin=0.1)
text(fit_rpart$finalModel, cex=0.75)
# The left hand side of decision tree branches are the 'Yes' option. Right hand side branches are the 'No' option.
# Optimal value of cp: 0.016
# Accuracy on the test set using the cross-validated kNN model: 0.838