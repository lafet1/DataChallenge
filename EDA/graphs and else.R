categories <- subdf %>% 
  select(category_id, price, was_promoted) %>%
  mutate(was_promoted = if_else(was_promoted == "t", 1, 0)) %>%
  group_by(category_id) %>% 
  summarise(count = n(), 
            avg_price = mean(price, na.rm = TRUE), 
            was_promoted = sum(was_promoted, na.rm = TRUE)) %>% 
  arrange(desc(avg_price)) %>% 
  filter(!is.na(avg_price))


ggplot(data = categories, aes(log(avg_price))) + 
  geom_density(kernel='gaussian', color = "red", fill = 'red', alpha = 0.3) + 
  theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1), 
    plot.title = element_text(size = 17, 
        vjust = 1)) +labs(title = "Density of avg_price in category", 
    x = "log of average price")



ggplot(data = categories, aes(log(count))) + 
  geom_density(kernel='gaussian', color = "red", fill = 'red', alpha = 0.3) + 
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        plot.title = element_text(size = 17, 
                                  vjust = 1)) +labs(title = "Density of log(count) in category", 
                                                    x = "log of count")

ggplot(data = categories_big, aes(was_promoted/count)) + 
  geom_density(kernel='gaussian', color = "red", fill = 'red', alpha = 0.3) + 
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        plot.title = element_text(size = 17, 
                                  vjust = 1)) +labs(title = "Density of was_promoted count in category", 
                                                    x = "share of was_promoted")



ggplot(data=subdf) + geom_qq(aes(sample=price))


  


division_quantile <- 0.8

categories_big <- categories %>%
  filter(count > quantile(count, probs = division_quantile))

categories_small <- categories %>%
  filter(count < quantile(count, probs = division_quantile))

categories_small %>% 
  summarise(was_promoted = sum(was_promoted), count = sum(count))

categories_big %>% 
  summarise(was_promoted = sum(was_promoted), count = sum(count))


  