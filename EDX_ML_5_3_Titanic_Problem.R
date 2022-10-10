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

test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.8, list = FALSE)

train_set <- titanic_clean %>% slice(-test_index)
test_set <- titanic_clean %>% slice(test_index)

length(train_set$Survived)


