setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(kernlab)
library(mlr)
library(tidyverse)
library(caret)
gc()

load("prepared_dataset.RData")

## Needed in case for quick run:
size_of_random_subset <- 0.01
rnd <- runif(n = dim(d)[1])
d <- d[rnd < size_of_random_subset, ]

rnd <- runif(n = dim(d)[1])
d_test <- d[rnd < 0.15, ] # true data for final modeling
d_train <- d[rnd >= 0.15, ]
rm(d)

factors <- c("was_promoted", 
             "visible_in_profile", "is_liquid", "excl_descr", 
             "has_capslock_descr", "excl_title", "has_capslock_title", 
             "no_price", "no_descr", "bad_cl_text", "good_cl_text",           
             "good_cl_categs",          
             "eight_to_sixteen",        
             "seventeen_to_twentyfour", 
             "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9",
             "is_weekend")


d_factors <- d_train %>% mutate_if(colnames(.) %in% factors, as.factor)
rm(d_train)
gc()


##### over/under sample
task <- makeClassifTask(data = d_factors, target = "was_promoted")
task.under <- undersample(task, rate = 1/3)
task.over <- oversample(task.under, rate = 10)

table(getTaskTargets(task.over))
d_factors <- getTaskData(task.over)

my_fun <- function(x){as.numeric(as.vector(x))}
x_train <- d_factors %>% mutate_if(colnames(.) %in% factors, my_fun)
x_test <- d_test



y_train <- x_train %>%select(was_promoted) %>% transmute(y=if_else(was_promoted==0, -1, 1)) %>% as.matrix()
y_test <- x_test %>%select(was_promoted) %>% transmute(y=if_else(was_promoted==0, -1, 1)) %>% as.matrix()
x_test$was_promoted <- NULL
x_train$was_promoted <- NULL
x_train <- as.matrix(x_train)
x_test <- as.matrix(x_test)

on <- inlearn(124,kernel = "rbfdot",kpar = list(sigma=0.2),
              type="classification")
ind <- sample(1:nrow(x_train),nrow(x_train))

counter <- 1
for (i in ind){
  on <- onlearn(on,x_train[i, ],y_train[i],nu=0.03,lambda=0.1)
  print(counter)
  counter <- counter + 1
}

sign(predict(on, x_test)) -> abc
sum(y_test == abc)
length(abc)

confusionMatrix(abc, y_test)
