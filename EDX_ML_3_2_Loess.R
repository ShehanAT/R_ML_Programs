library(dslabs)
library(tidyverse)
library(caret)

class(polls_2008$day)

polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() +
  geom_smooth(color="red", span=0.15, method="loess", method.args=list(degree=1))

class(polls_2008$margin)

?Comparison