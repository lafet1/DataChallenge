library(tidyverse)
library(feather)
library(keras)
library(tensorflow)
library(lubridate)
library(datetime)

set.seed(921021)

# setwd("~/Škola/UvA/Výuka/Machine Learning for Econometrics/Project/DataChallenge/Analysis")
d <- read_feather("file_notext.feather")
d$district_id <- NULL
d$has_gg <- NULL
d$has_skype <- NULL
d$has_phone <- NULL
d$private_business <- NULL
d$id <- NULL
d$created_at_first <- ymd_hms(d$created_at_first, tz = "Asia/Manila")
d$is_liquid <- as.numeric(ifelse(d$is_liquid == "t", 1, 0))
d$was_promoted <- as.numeric(ifelse(d$was_promoted == "t", 1, 0))


d <- d %>% 
  mutate(wday = wday(created_at_first)) %>% 
  mutate(no_price = as.numeric(is.na(price))) %>% 
  mutate(price = if_else(is.na(price), 0, price)) %>% 
  mutate(hour = as.numeric(hour(created_at_first))) %>% 
  mutate(no_descr = as.numeric(is.na(upper_descr))) %>% 
  mutate(region_id = as.numeric(region_id)) %>%
  mutate(subregion_id = as.numeric(subregion_id)) %>%
  mutate(category_id = as.numeric(category_id)) %>% 
  mutate(month = month(created_at_first))

d[is.na(d)] <- 0

d <- d %>%
  mutate(user_id = as.numeric(user_id)) %>%
  mutate(visible_in_profile = as.numeric(visible_in_profile)) %>%
  mutate(excl_descr = as.numeric(excl_descr)) %>%
  mutate(has_capslock_descr = as.numeric(has_capslock_descr)) %>%
  mutate(excl_title = as.numeric(excl_title)) %>% 
  mutate(has_capslock_title = as.numeric(has_capslock_title))

d <- d %>% 
  group_by(user_id) %>% 
  mutate(n_user_id = length(user_id)) %>% ungroup()

d <- d %>% 
  mutate(bata_price = as.numeric(if_else((price + 1) %% 10 == 0, TRUE, FALSE))) %>% 
  mutate(was_promoted = as.numeric(was_promoted))



rnd <- runif(n = dim(d)[1])
d_test <- d[rnd < 0.15, ]
d_train <- d[rnd >= 0.15, ]

rm(d)
rm(rnd)


# load("~/Škola/UvA/Výuka/Machine Learning for Econometrics/Project/DataChallenge/Analysis/.RData")



y_test <- to_categorical(d_test$was_promoted)
y_train <- to_categorical(d_train$was_promoted)
d_test$was_promoted <- NULL
d_train$was_promoted <- NULL
d_train$user_id <- NULL
d_train$created_at_first <- NULL

d_test$user_id <- NULL
d_test$created_at_first <- NULL
d_test$wday <- as.numeric(d_test$wday)

d_train <- as.matrix(d_train)

d_test <- as.matrix(d_test)

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 2, activation = 'sigmoid', input_shape = c(105))

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)





history <- model %>% fit(
  d_train, y_train, 
  epochs = 5, batch_size = 128, 
  validation_split = 0.2
)

sapply(d_train, class)
sum(sapply(d_train, function(x) sum(is.na(x))))


model %>% predict_classes(d_test) %>% head()
model %>% evaluate(d_test, y_test)

# 630998/630998 [==============================] - 14s 22us/step
# $loss
# [1] NaN
# 
# $acc
# [1] 0.9781552

sum(y_test[,1])/dim(y_test)[1]
# [1] 0.9781552


