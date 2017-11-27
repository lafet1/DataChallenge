library(tidyverse)
library(feather)
library(keras)
library(tensorflow)
library(lubridate)
library(datetime)

set.seed(921021)


d <- read_feather("file_notext.feather")
d$district_id <- NULL
d$has_gg <- NULL
d$has_skype <- NULL
d$has_phone <- NULL
d$private_business <- NULL
d$created_at_first <- ymd_hms(d$created_at_first, tz = "Asia/Manila")

d <- d %>% 
  mutate(wday = wday(created_at_first, label = TRUE)) %>% 
  mutate(no_price = as.factor(is.na(price))) %>% 
  mutate(price = if_else(is.na(price), 0, price)) %>% 
  mutate(hour = as.factor(hour(created_at_first))) %>% 
  mutate(no_descr = as.factor(is.na(upper_descr))) %>% 
  mutate(region_id = as.factor(region_id)) %>%
  mutate(subregion_id = as.factor(subregion_id)) %>%
  mutate(category_id = as.factor(category_id)) 

d[is.na(d)] <- 0

d <- d %>%
  mutate(user_id = as.factor(user_id)) %>%
  mutate(visible_in_profile = as.factor(visible_in_profile)) %>%
  mutate(excl_descr = as.factor(excl_descr)) %>%
  mutate(has_capslock_descr = as.factor(has_capslock_descr)) %>%
  mutate(excl_title = as.factor(excl_title)) %>% 
  mutate(has_capslock_title = as.factor(has_capslock_title))
d$is_liquid <- as.factor(ifelse(d$is_liquid == "t", TRUE, FALSE))


d <- d %>% 
  mutate(bata_price = as.factor(if_else((price + 1) %% 10 == 0, TRUE, FALSE))) %>% 
  mutate(was_promoted = as.factor(was_promoted))



rnd <- runif(n = dim(d)[1])
d_test <- d[rnd < 0.15, ]
d_train <- d[rnd >= 0.15, ]

rm(d)
rm(rnd)

# setwd("~/Škola/UvA/Výuka/Machine Learning for Econometrics/Project/DataChallenge/Analysis")
# load("~/Škola/UvA/Výuka/Machine Learning for Econometrics/Project/DataChallenge/Analysis/.RData")



y_test <- to_categorical(d_test$was_promoted)
y_train <- to_categorical(d_train$was_promoted)
d_test$was_promoted <- NULL
d_train$was_promoted <- NULL





model <- keras_model_sequential()
model %>% 
  layer_dense(units = 2, activation = 'sigmoid', input_shape = c(106))

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)


history <- model %>% fit(
  d_train, y_train, 
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)







