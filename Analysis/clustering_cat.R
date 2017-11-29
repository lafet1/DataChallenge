library(clustMixType)
library(tidyverse)

set.seed(921021)

# load after modeling data set

random <- runif(dim(d_train)[1])

rm(d_test)
rm(y_test)
rm(history)
rm(model)
rm(names_d_test)


d_train <- as.data.frame(d_train[random < 0.05, ])
clust_cat <- as.tibble(d_train) %>%
  select(category_id, region_id, city_id) %>%
  mutate_all(funs(factor))

clust_cat <- cbind(clust_cat, d_train$price)


groups <- kproto(clust_cat, 5)




