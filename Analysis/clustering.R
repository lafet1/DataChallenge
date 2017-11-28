library(clustMixType)
library(tidyverse)
library(feather)

set.seed(921021)

# totally_clean_dataset used

rnd <- runif(n = dim(d)[1])
d_play <- d[rnd < 0.05, ]

# rm(d)
# rm(rnd)

# changing numeric to factors for outlier detection
d_play$region_id <- as.factor(d_play$region_id)
d_play$subregion_id <- as.factor(d_play$subregion_id)
d_play$city_id <- as.factor(d_play$city_id)
d_play$category_id <- as.factor(d_play$category_id)
d_play$visible_in_profile <- as.factor(d_play$visible_in_profile)
d_play$user_id <- NULL
d_play$created_at_first <- NULL
d_play$excl_descr <- as.factor(d_play$excl_descr)
d_play$has_capslock_descr <- as.factor(d_play$has_capslock_descr)
d_play$excl_title <- as.factor(d_play$excl_title)
d_play$has_capslock_title <- as.factor(d_play$has_capslock_title)
d_play$is_liquid <- as.factor(d_play$is_liquid)
d_play$was_promoted <- as.factor(d_play$was_promoted)
d_play$month <- as.factor(d_play$month)
d_play$hour <- as.factor(d_play$hour)
d_play$no_descr <- as.factor(d_play$no_descr)
d_play$no_price <- as.factor(d_play$no_price)
d_play$bata_price <- as.factor(d_play$bata_price)


wss0 <- sapply(1:2, function(k){kproto(as.data.frame(d_play), k)$tot.withinss})
wss <- sapply(3:10, function(k){kproto(as.data.frame(d_play), k)$tot.withinss})
wss2 <- sapply(11:15, function(k){kproto(as.data.frame(d_play), k)$tot.withinss})
wss3 <- sapply(16:20, function(k){kproto(as.data.frame(d_play), k)$tot.withinss})

plot(1:15, c(wss0, wss, wss2),
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# 18 clusters seem to be ideal choice

trial <- kproto(as.data.frame(d_play), 
                k = 18)
