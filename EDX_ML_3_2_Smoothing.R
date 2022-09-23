library(dslabs)
library(tidyverse)
library(caret)


data("polls_2008")
qplot(day, margin, data = polls_2008)