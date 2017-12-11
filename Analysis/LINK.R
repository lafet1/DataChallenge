# rnd <- runif(n = dim(subdf)[1])
# subdf <- subdf[rnd < 0.005, ]

library(tidyverse)
library(feather)
library(text2vec)
library(tokenizers)
library(stringr)
library(slam)
library(lubridate)
library(textstem)
library(hunspell)
library(clustMixType)


lda_model = LDA$new(n_topics = 50, doc_topic_prior = 1/50, topic_word_prior = 0.01)
doc_topic_distr = lda_model$fit_transform(x = dtm, n_iter = 500, 
                                          convergence_tol = 0.001, n_check_convergence = 25)

doc_topic_distr <- doc_topic_distr %>% as.data.frame()
doc_topic_distr <- doc_topic_distr %>% rownames_to_column(var = "id")
subdf <- subdf %>% mutate(id = as.character(id))
subdf <- subdf %>% inner_join(doc_topic_distr, by = "id")


lda_model_title <- LDA$new(n_topics = 25, doc_topic_prior = 1/25, topic_word_prior = 0.01)
doc_topic_distr_title <- lda_model_title$fit_transform(x = dtm_title, 
                                                       n_iter = 250, 
                                                       convergence_tol = 0.001, 
                                                       n_check_convergence = 25)
doc_topic_distr_title <- doc_topic_distr_title %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "id")

colnames(doc_topic_distr_title) <- c("id", paste("title_V", 1:25, sep = ""))
colnames(doc_topic_distr_title)


subdf <- subdf %>% inner_join(doc_topic_distr_title, by = "id")
subdf$title <- NULL
subdf$description <- NULL
d <- subdf
d$district_id <- NULL





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







clust_categories_text <- d %>%
  select(category_id, starts_with("V",ignore.case = FALSE)) %>%
  as.data.frame() %>% kproto(10)



n_promoted_clust <- clust_categories_text$cluster[d$was_promoted == 1]
table_clust_promoted <- table(n_promoted_clust)
table_clust_promoted


table_clust_size <- clust_categories_text$size


table_clust_size
table_clust_promoted

up_ten <- 1:10
full_indices <- up_ten[(as.character(1:10)%in% names(table_clust_promoted))]
share_promoted_in_cluster <- table_clust_promoted/table_clust_size[full_indices]

bad_cluster <- as.numeric(names(share_promoted_in_cluster)[share_promoted_in_cluster == min(share_promoted_in_cluster)])
good_cluster <-as.numeric(names(share_promoted_in_cluster)[share_promoted_in_cluster == max(share_promoted_in_cluster)])


d$bad_cl_text <- (clust_categories_text$cluster == bad_cluster)
d$good_cl_text <- (clust_categories_text$cluster == good_cluster)














clust_categories <- d %>%
  select(category_id, price) %>%
  as.data.frame() %>% kproto(10)



n_promoted_clust <- clust_categories$cluster[d$was_promoted == 1]
table_clust_promoted <- table(n_promoted_clust)
table_clust_promoted


table_clust_size <- clust_categories$size


table_clust_size
table_clust_promoted

up_ten <- 1:10
full_indices <- up_ten[(as.character(1:10)%in% names(table_clust_promoted))]
share_promoted_in_cluster <- table_clust_promoted/table_clust_size[full_indices]

bad_cluster <- as.numeric(names(share_promoted_in_cluster)[share_promoted_in_cluster == min(share_promoted_in_cluster)])
good_cluster <-as.numeric(names(share_promoted_in_cluster)[share_promoted_in_cluster == max(share_promoted_in_cluster)])


d$bad_cl_categs <- (clust_categories$cluster == bad_cluster)
d$good_cl_categs <- (clust_categories$cluster == good_cluster)








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





abcd <- d %>% 
  group_by(city_id, was_promoted) %>% 
  count(was_promoted)

a <- abcd[abcd$was_promoted == 0, ]
b <- abcd[abcd$was_promoted == 1, ]
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
  group_by(region_id, was_promoted) %>% 
  count(was_promoted)

a <- abcd[abcd$was_promoted == 0, ]
b <- abcd[abcd$was_promoted == 1, ]
b <- b[(b$region_id %in% a$region_id), ]
a <- a[(a$region_id %in% b$region_id), ]

key <- b %>% 
  left_join(a, by = "region_id") %>%
  mutate(was_to_wasnt_region = n.x/n.y) %>% 
  arrange(desc(was_to_wasnt_region)) %>% ungroup() %>%
  select(region_id, was_to_wasnt_region)

d <- d %>% 
  left_join(key, by = "region_id")






abcd <- d %>% 
  group_by(subregion_id, was_promoted) %>% 
  count(was_promoted)

a <- abcd[abcd$was_promoted == 0, ]
b <- abcd[abcd$was_promoted == 1, ]
b <- b[(b$subregion_id %in% a$subregion_id), ]
a <- a[(a$subregion_id %in% b$subregion_id), ]

key <- b %>% 
  left_join(a, by = "subregion_id") %>%
  mutate(was_to_wasnt_subregion = n.x/n.y) %>% 
  arrange(desc(was_to_wasnt_subregion)) %>% ungroup() %>%
  select(subregion_id, was_to_wasnt_subregion)

d <- d %>% 
  left_join(key, by = "subregion_id")


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








