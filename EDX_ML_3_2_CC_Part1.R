# Q1

library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)

library(caret)
library(dslabs)


fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n=6) %>%
    .[,1:5] %>%
    as_data_frame() %>%
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN"=1, "FEB"=2, "MAR"=3, "APR"=4, "MAY"=5, "JUN"=6,
                        "JUL"=7, "AUG"=8, "SEP"=9, "OCT"=10, "NOV"=11, "DEC"=12)) %>%
  mutate(date = make_date(year, month, day)) %>%
    dplyr::filter(date <= "2018-05-01")

span <- 60 / as.numeric(diff(date(range(dat$date))))
fit <- dat %>% mutate(x = as.numeric(date)) %>% loess(data = ., span = span, degree = 1, deaths ~ x)
# dat %>% mutate(smooth=predict(fit, as.numeric(date))) %>%
#   ggplot() +
#   geom_point(aes(date, deaths))+
#   geom_line(aes(date, deaths), lwd= 3, col=2)  

# answer (a)
# dat %>% 
#   mutate(smooth = predict(fit), day = yday(date), year = as.character(year(date))) %>%
#   ggplot(aes(day, smooth, col = year)) + 
#   geom_line(lwd = 2)

# answer (b)
# dat %>%
#   mutate(smooth = predict(fit, as.numeric(date)), day=mday(date), year=as.character(year(date))) %>%
#   ggplot(aes(day, smooth, col=year)) +
#   geom_line(lwd=2)

# answer (c)
# dat %>%
#   mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
#   ggplot(aes(day, smooth)) +
#   geom_line(lwd = 2)

# answer (d)
dat %>%
  mutate(smooth=predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) + 
  geom_line(lwd = 2)

# Q3
library(broom)
library(dslabs)
data("mnist_27")
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()

qplot(x_2, y, data = mnist_27$train)

mnist_27$train %>% 
  mutate(y = ifelse(y == "7", 1, 0)) %>%
  ggplot(aes(x_2, y)) + 
  geom_smooth(method = "loess")