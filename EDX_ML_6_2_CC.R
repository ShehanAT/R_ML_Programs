library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")

# Q1

movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") + 
  coord_trans(y = "sqrt") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Q2

movielens %>% filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), year = 2018 - first(year), title=title[1], rating=mean(rating), avgRatingPerYear = n/year) %>% 
  mutate(rate = n/year) %>% 
  top_n(25, rate) %>% 
  arrange(desc(rate))


# Q3

movielens %>% filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year), title=title[1], rating=mean(rating)) %>% 
  mutate(rate = n/years) %>% 
  ggplot(aes(rate, rating)) + 
  geom_point() + 
  geom_smooth()