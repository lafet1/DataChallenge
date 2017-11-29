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
  mutate(user_id = as.numeric(user_id),
         visible_in_profile = as.numeric(visible_in_profile),
         excl_descr = as.numeric(excl_descr),
         has_capslock_descr = as.numeric(has_capslock_descr),
         excl_title = as.numeric(excl_title),
         has_capslock_title = as.numeric(has_capslock_title),
         upper_share_descr = (upper_descr/length_descr),
         upper_share_title = (upper_title/length_title),
         punct_share_title = (punct_title/length_title),
         punct_share_descr = (punct_descr/length_descr),
         bata_price = as.numeric(if_else((price + 1) %% 10 == 0, TRUE, FALSE)),
         was_promoted = as.numeric(was_promoted))

d <- d %>% 
  group_by(user_id) %>% 
  mutate(n_user_id = length(user_id)) %>% 
  ungroup() %>% 
  group_by(region_id) %>% 
  mutate(n_region_id = length(region_id)) %>% 
  ungroup() %>% 
  group_by(subregion_id) %>% 
  mutate(n_subregion_id = n()) %>% 
  ungroup() %>% 
  group_by(city_id) %>% 
  mutate(n_city_id = n()) %>% 
  ungroup() %>% 
  group_by(category_id) %>% 
  mutate(n_category_id = n()) %>% 
  ungroup() %>% 
  select(-c(user_id, 
            region_id,
            subregion_id, 
            city_id, 
            category_id, 
            created_at_first))




# for (i in unique_cats){
#   column_name <- paste("cat", i, sep = "_")
#   d2 <- d2 %>% 
#     mutate(temporary = if_else(category_id == i, 1, 0))
#   colnames(d2)[dim(d2)[2]] <- column_name
# }


