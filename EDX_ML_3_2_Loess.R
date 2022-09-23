library(dslabs)
library(tidyverse)
library(caret)


polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() +
  geom_smooth(color="red", span=0.15, method="loess", method.args=list(degree=1))