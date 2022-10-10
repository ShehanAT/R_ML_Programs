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
f_survived <- ifelse(test_set$Sex == "female", 1, 0)
mean(f_survived == test_set$Survived)

# Q4a

class_results <- train_set %>% group_by(Pclass) %>%
  summarize(p1 = mean(Survived == 1))

# Q4b

class_results <- ifelse(test_set$Pclass == 1, 1, 0)
mean(class_results == test_set$Survived)

second_class_results <- ifelse(test_set$Pclass == 2, 1, 0)
mean(second_class_results == test_set$Survived)

third_class_results <- ifelse(test_set$Pclass == 3, 1, 0)
mean(third_class_results == test_set$Survived)

# Q4c 

passengers <- train_set %>% group_by(Pclass, Sex) %>%
  summarize(p1 = mean(Survived == 1))
passengers

# Q4d 
f_1_survived <- ifelse(test_set$Pclass == 2 & test_set$Sex == "female", 1, 0)
mean(f_1_survived == test_set$Survived)
