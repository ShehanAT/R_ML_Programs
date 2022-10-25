library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")

movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") + 
  coord_trans(y = "sqrt") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

movielens %>% filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), year = 2018 - first(year), title=title[1], rating=mean(rating), avgRatingPerYear = n/year) %>% 
  mutate(rate = n/year) %>% 
  top_n(25, rate) %>% 
  arrange(desc(rate))