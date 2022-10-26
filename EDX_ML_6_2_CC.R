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

# Q4 

# When doing predictive analysis in which you need to fill in missing ratings with some values:
# Fill in the missing values with a lower value than the average rating across all movies 
# Because of a lack of ratings is associated with lower ratings, it would be most appropriate to 
# Fill in the missing value with a lower value than the average. You should try out different values
# To fill in the missing value and evaluate prediction in a test set. 

# Q5

movielens <- mutate(movielens, date = as.date(timestamp))

movielens <- mutate(movielens, date = as_datetime(timestamp)) # This is the correct answer for creating a new column `date` with the date

movielens <- mutate(movielens, date = as.data(timestamp))

movielens <- mutate(movielens, date = timestamp)

movielens

# Q6 

movielens %>% 
  mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating=mean(rating)) %>%
  ggplot(aes(date, rating)) + 
  geom_point() + 
  geom_smooth()

# Q7

# If we define d_{u,i} as the day for user's {u} rating of movie {i}, the following model would be the most appropriate:
# Y_{u, i} = µ + b_{i} + b_{u} + f(d_{u, i}) + ε_{u, i}
