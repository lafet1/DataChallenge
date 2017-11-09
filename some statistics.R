# price by regions
expensive_region <- subdf %>% 
  select(region_id, price, was_promoted) %>%
  mutate(was_promoted = if_else(was_promoted == "t", 1, 0)) %>%
  group_by(region_id) %>%
  summarise(count = n(), 
            avg_price = mean(price, na.rm = TRUE), 
            was_promoted = sum(was_promoted, na.rm = TRUE)) %>% 
  arrange(desc(avg_price)) %>% 
  filter(!is.na(avg_price), count > quantile(count, 0.5))

subdf %>% 
  select(region_id, price, was_promoted) %>%
  mutate(was_promoted = if_else(was_promoted == "t", 1, 0)) %>%
  group_by(region_id) %>%
  summarise(count = n(), 
                 avg_price = mean(price, na.rm = TRUE), 
                 was_promoted = sum(was_promoted, na.rm = TRUE)) %>% 
  arrange(desc(avg_price)) %>% 
  filter(!is.na(avg_price), count <= quantile(count, 0.5)) %>%
  summarise(sum(count), sum(was_promoted))

summary(expensive_region$count)
expensive_region[is.na(expensive_region$avg_price),]
cor(log(expensive_region$avg_price), expensive_region$was_promoted)
summary(expensive_region$was_promoted)

ggplot(data = expensive_region, aes(x = log(avg_price))) + 
  geom_histogram(binwidth = 1, fill = "blue")


# what is "is_liquid"?
head(subdf[subdf$is_liquid == "t", ])

length(unique(subdf[subdf$was_promoted == "t", ]$id))

# are those who sell more more promoted?
user <- subdf %>%
  select(user_id, was_promoted) %>%
  mutate(was_promoted = if_else(was_promoted == "t", 1, 0)) %>%
  group_by(user_id) %>%
  summarise(count = n(),
            was_promoted = sum(was_promoted)) 
user %>% filter(count == 1) %>% summarise(sum(count), sum(was_promoted))
nrow(user %>% filter(count == 1, was_promoted > 0)) / 
  nrow(user %>% filter(count == 1, was_promoted == 0))
user %>% filter(count > 1) %>% summarise(sum(count), sum(was_promoted))
nrow(user %>% filter(count > 1, was_promoted > 0)) / 
  nrow(user %>% filter(count > 1, was_promoted == 0))

# are the dummy variables useful?
dummies <- subdf %>% 
  select(has_skype, has_gg, has_phone, visible_in_profile, price, was_promoted) %>%
  mutate(has_skype = if_else(has_skype == "t", 1, 0),
         has_gg = if_else(has_gg == "t", 1, 0),
         has_phone = if_else(has_phone == "t", 1, 0),
         visible_in_profile = if_else(visible_in_profile == "t", 1, 0),
         was_promoted = if_else(was_promoted == "t", 1, 0)) %>%
  group_by(has_skype, has_gg, has_phone, visible_in_profile) %>%
  summarise(count = n(), 
            avg_price = mean(price, na.rm = TRUE), 
            was_promoted = sum(was_promoted, na.rm = TRUE)) %>% 
  arrange(desc(avg_price)) %>% 
  filter(!is.na(avg_price))

# price by subregions
expensive_subregion <- subdf %>% 
  select(subregion_id, price, was_promoted) %>%
  mutate(was_promoted = if_else(was_promoted == "t", 1, 0)) %>%
  group_by(subregion_id) %>%
  summarise(count = n(), 
            avg_price = mean(price, na.rm = TRUE), 
            was_promoted = sum(was_promoted, na.rm = TRUE)) %>% 
  arrange(desc(avg_price)) %>% 
  filter(!is.na(avg_price), count > quantile(count, 0.5))

# SUBREGION IS SAME AS REGION

# price by city
expensive_city <- subdf %>% 
  select(city_id, price, was_promoted) %>%
  mutate(was_promoted = if_else(was_promoted == "t", 1, 0)) %>%
  group_by(city_id) %>%
  summarise(count = n(), 
            avg_price = mean(price, na.rm = TRUE), 
            was_promoted = sum(was_promoted, na.rm = TRUE)) %>% 
  arrange(desc(avg_price)) %>% 
  filter(!is.na(avg_price), count > quantile(count, 0.5))

subdf %>% 
  select(city_id, price, was_promoted) %>%
  mutate(was_promoted = if_else(was_promoted == "t", 1, 0)) %>%
  group_by(city_id) %>%
  summarise(count = n(), 
            avg_price = mean(price, na.rm = TRUE), 
            was_promoted = sum(was_promoted, na.rm = TRUE)) %>% 
  arrange(desc(avg_price)) %>% 
  filter(!is.na(avg_price), count <= quantile(count, 0.5)) %>% 
  summarise(sum(count), sum(was_promoted))

summary(expensive_city$count)
expensive_city[is.na(expensive_city$avg_price),]
cor(log(expensive_city$avg_price), expensive_city$was_promoted)
summary(expensive_city$was_promoted)

ggplot(data = expensive_city, aes(x = log(avg_price))) + 
  geom_histogram(binwidth = 1, fill = "blue")


# category and geographical relationship
cat_region <- subdf %>% 
  select(category_id, region_id, price, was_promoted) %>%
  mutate(was_promoted = if_else(was_promoted == "t", 1, 0)) %>%
  group_by(category_id, region_id) %>%
  summarise(count = log(n()), 
            avg_price = mean(price, na.rm = TRUE), 
            was_promoted = sum(was_promoted, na.rm = TRUE)) %>% 
  arrange(desc(avg_price)) %>%
  filter(!is.na(avg_price), count > quantile(count, 0.5)) 

ggplot(data = cat_region, aes(x = category_id, y = region_id)) + 
  geom_count(aes(size = was_promoted), shape = 21, colour = "indianred", fill = "indianred", 
             stroke = 1.05, alpha = 0.4)

ggplot(data = cat_region, aes(x = category_id, y = region_id)) + 
  geom_count(aes(size = count), shape = 21, colour = "indianred", fill = "indianred", 
             stroke = 1.05, alpha = 0.4)


cat_city <- subdf %>% 
  select(category_id, city_id, price, was_promoted) %>%
  mutate(was_promoted = if_else(was_promoted == "t", 1, 0)) %>%
  group_by(category_id, city_id) %>%
  summarise(count = log(n()), 
            avg_price = mean(price, na.rm = TRUE), 
            was_promoted = sum(was_promoted, na.rm = TRUE)) %>% 
  arrange(desc(avg_price)) %>%
  filter(!is.na(avg_price), count > quantile(count, 0.5))

ggplot(data = cat_city, aes(x = category_id, y = city_id)) + 
  geom_count(aes(size = was_promoted), shape = 21, colour = "indianred", fill = "indianred", 
             stroke = 1.05, alpha = 0.4)

ggplot(data = cat_city, aes(x = category_id, y = city_id)) + 
  geom_count(aes(size = count), shape = 21, colour = "indianred", fill = "indianred", 
             stroke = 1.05, alpha = 0.4)





