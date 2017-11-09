ggplot(data = subdf, aes(x=region_id,y=log(price))) + geom_col()



summary(subdf$price)


ggplot(data=subdf) + geom_qq(aes(sample=price))

expensive_categories <- subdf %>% 
  select(category_id, price, was_promoted) %>%
  mutate(was_promoted = if_else(was_promoted == "t", 1, 0)) %>%
  group_by(category_id) %>% 
  summarise(count = n(), 
            avg_price = mean(price, na.rm = TRUE), 
            was_promoted = mean(was_promoted, na.rm = TRUE)) %>% 
  arrange(desc(avg_price)) %>% filter(!is.na(avg_price))

summary(expensive_categories$count)
expensive_categories[is.na(expensive_categories$avg_price),]
cor(log(expensive_categories$avg_price), expensive_categories$was_promoted)
summary(expensive_categories$was_promoted)

