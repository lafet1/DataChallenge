library(mlr)
library(caret)
library(tidyverse)
library(feather)
library(keras)
library(tensorflow)
library(lubridate)
library(datetime)

set.seed(921021)

d <- d_orig
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rnd <- runif(n = dim(d)[1])
d <- d[rnd < 0.1, ]



rnd <- runif(n = dim(d)[1])
d_test <- d[rnd < 0.15, ]
d_train <- d[rnd >= 0.15, ]





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




##### over/under sample
task = makeClassifTask(data = d_factors, target = "was_promoted")
task.over = oversample(task, rate = 5.85)
task.under = undersample(task.over, rate = 1/7.5)
table(getTaskTargets(task.under))
d_resampled <- getTaskData(task.under)


####### SMOTE
# task = makeClassifTask(data = d_factors, target = "was_promoted")
# task.under = undersample(task, rate = 1/7)
# then <- Sys.time()
# task.smote = smote(task.under, rate = 5, nn = 5)
# print(Sys.time() - then)
# d_resampled <- getTaskData(task.smote)
# table(getTaskTargets(task.smote))

# 
# table(getTaskTargets(task))
# table(getTaskTargets(task.over))
# table(getTaskTargets(task.under))
# sum(table(getTaskTargets(task.under))) / nrow(d_factors)

###### KERAS PREP

my_fun <- function(x){as.numeric(as.vector(x))}
d_train <- d_resampled %>% mutate_if(colnames(.) %in% factors, my_fun)




# load("~/Škola/UvA/Výuka/Machine Learning for Econometrics/Project/DataChallenge/Analysis/.RData")

x_test <- d_test
x_train <- d_train



y_test <- to_categorical(x_test$was_promoted)
y_train <- to_categorical(x_train$was_promoted)

x_test$was_promoted <- NULL
x_train$was_promoted <- NULL


x_train <- as.matrix(x_train)
x_test <- as.matrix(x_test)










model <- keras_model_sequential()
model %>% 
  layer_dense(units = 64, activation = 'sigmoid', input_shape = c(dim(x_train)[2])) %>% 
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 32, activation = layer_activation_leaky_relu(alpha = 0.17))  %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = 'softmax')

class_weight = list('0' = 1, '1' = 1.43)



model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_adam(lr = 0.0009),
  metrics = "accuracy"
)




history <- model %>% fit(
  x_train, y_train, 
  epochs = 30, batch_size = 256, 
  validation_split = 0.22,
 class_weight = class_weight
)

plot(history)

predicted <- model %>% 
  predict_classes(x_test) 
model %>% evaluate(x_test, y_test)
true_values <- y_test[,2]

caret::confusionMatrix(data = predicted, reference = true_values)








# 630998/630998 [==============================] - 14s 22us/step
# $loss
# [1] NaN
# 
# $acc
# [1] 0.9781552

sum(y_test[,1])/dim(y_test)[1]
# [1] 0.9781552

