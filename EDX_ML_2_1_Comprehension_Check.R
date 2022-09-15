library(dslabs)
library(dplyr)
library(lubridate)
library(caret)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

categories <- dat %>%
  group_by(type) %>%
  summarise(propf = mean(sex == "Female"))
# print(categories[1, ])
# print(x)
# Q2
# print(y_hat)
ifelse(x == "inclass", "Female", "Male") %>%
  # print(y)
  # print(nlevels(y))
  factor(levels = levels(y)) -> y_hat
  # print(y_hat)
# print(mean(y_hat == y))

# print(sensitivity(y_hat, y))
# print(specificity(y_hat, y))

prevalence_cal <- dat %>%
  group_by(sex) %>%
  summarise(Prevalence = mean(y == "Female"))

print(prevalence_cal)