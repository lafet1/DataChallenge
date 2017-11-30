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

d <- d %>% mutate(no_price = as.numeric(is.na(price))) %>% 
  mutate(price = if_else(is.na(price), 0, price)) %>% 
  mutate(hour = as.numeric(hour(created_at_first))) %>% 
  mutate(no_descr = as.numeric(is.na(upper_descr))) %>%
  mutate(month = month(created_at_first)) %>% 
  mutate(wday = wday(created_at_first))  

d <- d %>% 
  group_by(user_id) %>% 
  mutate(n_user_id = length(user_id)) %>% 
  ungroup() %>% 
  group_by(city_id) %>% 
  mutate(n_city_id = length(city_id)) %>% 
  ungroup() %>% 
  group_by(subregion_id) %>% 
  mutate(n_subregion_id = n()) %>% 
  ungroup() %>% 
  group_by(city_id) %>% 
  mutate(n_city_id = n()) %>% 
  ungroup() %>% 
  group_by(category_id) %>% 
  mutate(n_category_id = n()) %>% 
  ungroup() 

d[is.na(d)] <- 0

d <- d %>%
  mutate(upper_share_descr = (upper_descr/length_descr),
         upper_share_title = (upper_title/length_title),
         punct_share_title = (punct_title/length_title),
         punct_share_descr = (punct_descr/length_descr),
         bata_price = as.numeric(if_else((price + 1) %% 10 == 0, TRUE, FALSE)))





d$user_id <- NULL
d$created_at_first <- NULL

d$city_id <- as.factor(d$city_id)
d$subregion_id <- as.factor(d$subregion_id)
d$city_id <- as.factor(d$city_id)
d$category_id <- as.factor(d$category_id)
d$visible_in_profile <- as.factor(d$visible_in_profile)
d$month <- as.factor(d$month)
d$hour <- as.factor(d$hour)

d$excl_descr <- as.factor(d$excl_descr)
d$has_capslock_descr <- as.factor(d$has_capslock_descr)
d$excl_title <- as.factor(d$excl_title)
d$has_capslock_title <- as.factor(d$has_capslock_title)
d$is_liquid <- as.factor(d$is_liquid)
d$was_promoted <- as.factor(d$was_promoted)
d$no_descr <- as.factor(d$no_descr)
d$no_price <- as.factor(d$no_price)
d$bata_price <- as.factor(d$bata_price)


abcd <- d %>% 
  group_by(category_id, was_promoted) %>% 
  count(was_promoted) 

a <- abcd[abcd$was_promoted == 0, ]
b <- abcd[abcd$was_promoted == 1, ]

b <- b[(b$category_id %in% a$category_id), ]
a <- a[(a$category_id %in% b$category_id), ]

key <- b %>% 
  left_join(a, by = "category_id") %>%
  mutate(was_to_wasnt_category = n.x/n.y) %>% 
  arrange(desc(was_to_wasnt_category)) %>% ungroup() %>%
  select(category_id, was_to_wasnt_category)

d <- d %>% 
  left_join(key, by = "category_id")






d %>% select(category_id, was_to_wasnt_category) %>% group_by(was_to_wasnt_category) %>% count() %>% arrange(desc(was_to_wasnt_category))




abcd <- d %>% 
  group_by(city_id, was_promoted) %>% 
  count(was_promoted)

a <- abcd[abcd$was_promoted == 0, ]
b <- abcd[abcd$was_promoted == 1, ]

f <- b[!(b$city_id %in% a$city_id), ]
g <- a[!(a$city_id %in% b$city_id), ]
f
g



b <- b[(b$city_id %in% a$city_id), ]
a <- a[(a$city_id %in% b$city_id), ]

key <- b %>% 
  left_join(a, by = "city_id") %>%
  mutate(was_to_wasnt_city = n.x/n.y) %>% 
  arrange(desc(was_to_wasnt_city)) %>% ungroup() %>%
  select(city_id, was_to_wasnt_city)

d <- d %>% 
  left_join(key, by = "city_id")




abcd <- d %>% 
  group_by(city_id, was_promoted) %>% 
  count(was_promoted)

a <- abcd[abcd$was_promoted == 0, ]
b <- abcd[abcd$was_promoted == 1, ]

f <- b[!(b$city_id %in% a$city_id), ]
g <- a[!(a$city_id %in% b$city_id), ]
f
g



b <- b[(b$city_id %in% a$city_id), ]
a <- a[(a$city_id %in% b$city_id), ]

key <- b %>% 
  left_join(a, by = "city_id") %>%
  mutate(was_to_wasnt_city = n.x/n.y) %>% 
  arrange(desc(was_to_wasnt_city)) %>% ungroup() %>%
  select(city_id, was_to_wasnt_city)

d <- d %>% 
  left_join(key, by = "city_id")


d[is.na(d)] <- 0



d <- d %>% 
  select(-c(region_id, subregion_id, city_id, category_id))

d <- d %>% 
  mutate(eight_to_sixteen = (hour %in% c(8:16))) %>% 
  mutate(seventeen_to_twentyfour = (hour %in% c(17:24)))

d <- d %>% mutate(m1 = (month == 1), 
                  m2 = (month == 2), 
                  m3 = (month == 3), 
                  m4 = (month == 4),
                  m5 = (month == 5),
                  m6 = (month == 6),
                  m7 = (month == 7),
                  m8 = (month == 8),
                  m9 = (month == 9))
d <- d %>% mutate(is_weekend = (wday > 5))

d <- d %>% select(-wday)
d <- d %>% select(-month)
d <- d %>% select(-hour)

my_fun <- function(x) {if_else((x==1 | x==TRUE), 1, 0)}

d <- d %>% mutate_if(is.factor, my_fun)
d <- d %>% mutate_if(is.logical, as.numeric)
a <- d %>% select_if(is.factor)


rm(clust_categories_text)
rm(groups)
rm(groups_categories)
sapply(d, class)
# clust_cat <- d %>%
#   select(city_id, city_id, price) %>%
#   as.data.frame()
# 
# 
# 
# groups <- kproto(clust_cat, 10)
# abc <- groups$cluster[d$was_promoted == 1]
# abcd <- table(abc)
# 
# abcde <- groups$size
# 
# abcd/abcde
# 
# 
# #######
# clust_categories <- d %>%
#   select(category_id, price) %>%
#   as.data.frame()
# 
# 
# 
# groups_categories <- kproto(clust_categories, 10)
# abc <- groups_categories$cluster[d$was_promoted == 1]
# abcd <- table(abc)
# 
# 
# clust_categories_wp <- d %>%
#   select(category_id, price, was_promoted) %>%
#   as.data.frame() %>% kproto(10)
# 
# clust_categories_il <- d %>%
#   select(category_id, price, is_liquid) %>%
#   as.data.frame() %>% kproto(10)
# 
# clust_categories_il_wp <- d %>%
#   select(category_id, price, is_liquid, was_promoted) %>%
#   as.data.frame() %>% kproto(10)
# 
# 
# 
# clust_categories_text <- d %>%
#   select(category_id, starts_with("V",ignore.case = FALSE)) %>%
#   as.data.frame()
# 
# clust_categories_text_meta <- d %>%
#   select(category_id, ends_with("title"), ends_with("descr")) %>%
#   as.data.frame() %>% kproto(10)
# 
# clust_text_meta <- d %>%
#   select(ends_with("title"), ends_with("descr")) %>%
#   as.data.frame() %>% kproto(10)
# 
# rm(d)
# 
# groups_descr <- kproto(clust_categories_text, 8)
# 
# 
# groups_categories <- kproto(clust_categories, 10)
abc <- clust_categories_text$cluster[d$was_promoted == 1]
abcd <- table(abc)

abcde <- clust_categories_text$size
abc
abcd/abcde

d$bad_cl_text <- (clust_categories_text$cluster == 3)
d$good_cl_text <- (clust_categories_text$cluster == 8)


abc <- groups_categories$cluster[d$was_promoted == 1]
abcd <- table(abc)

abcde <- groups_categories$size
abc
abcd/abcde[-3]

d$good_cl_categs <- (groups_categories$cluster == 10)



# 
# 
# 
# abcde
# 
# abcd# for (i in unique_cats){
# #   column_name <- paste("cat", i, sep = "_")
#   d2 <- d2 %>% 
#     mutate(temporary = if_else(category_id == i, 1, 0))
#   colnames(d2)[dim(d2)[2]] <- column_name
# }


