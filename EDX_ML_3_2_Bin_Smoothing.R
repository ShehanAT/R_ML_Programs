library(dslabs)
library(tidyverse)
library(caret)


span <- 7
fit <- with(polls_2008, ksmooth(day, margin, x.points=day, kernel="box", bandwidth=span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) + 
  geom_point(size=3, alpha=0.5, color="grey") + 
  geom_line(aes(day, smooth), color="red")

# kernel 
span <- 7
fit <- with(polls_2008, ksmooth(day, margin, x.points=day, kernel="normal", bandwidth = span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size=3, alpha=0.5, color="grey") +
  geom_line(aes(day, smooth), color="red")