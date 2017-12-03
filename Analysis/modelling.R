library(tidyverse)
library(keras)
library(mlr)
library(caret)

set.seed(921021)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rnd <- runif(n = dim(d)[1])
d <- d[rnd < 0.01, ]



rnd <- runif(n = dim(d)[1])
# d_test <- d[rnd < 0.15, ] # true data for final modeling
# d_train <- d[rnd >= 0.15, ]

d_test <- d[rnd < 0.05, ]
d_train <- d[rnd >= 0.9, ]

rm(d)
rm(rnd)


# load("~/Škola/UvA/Výuka/Machine Learning for Econometrics/Project/DataChallenge/Analysis/.RData")


y_test <- to_categorical(d_test$was_promoted)
y_train <- to_categorical(d_train$was_promoted)
d_test$was_promoted <- NULL
d_train$was_promoted <- NULL

# balancing data set
df_train <- cbind(d_train, unname(y_train[, 2]))
names(df_train)[length(names(df_train))] <- "was_promoted"
df_train$was_promoted <- as.factor(df_train$was_promoted)
task = makeClassifTask(data = df_train, target = "was_promoted")
task.under = undersample(task, rate = 1/3)
task.over = oversample(task.under, rate = 10)

df_train <- getTaskData(task.over)
y2_train <- to_categorical(as.numeric(df_train$was_promoted) - 1)
df_train$was_promoted <- NULL

df_train <- as.matrix(df_train)
d_test <- as.matrix(d_test)


# keras
model <- keras_model_sequential()
model %>% 
  layer_dense(units = 512, input_shape = c(dim(df_train)[2])) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_activity_regularization(l1 = 0.00002) %>%
  layer_activation_leaky_relu(alpha = 0.55) %>%
  
  # sofar (probably) best combinations (all in layer 1) - alpha = 0.55, l1 = 0.00003;
  # alpha = 0.6, l1 = 0.00003; alpha = 0.6, l1 = 0.00002; alpha = 0.55, l1 = 0.00001
  
  layer_dense(units = 256) %>%
  layer_dropout(rate = 0.3) %>%
  layer_activity_regularization(l1 = 0.0000145) %>%
  layer_activation_leaky_relu(alpha = 0.5) %>%

  layer_dense(units = 32) %>%
  layer_dropout(rate = 0.25) %>%
  layer_activation_leaky_relu(alpha = 0.3) %>%
  
  layer_dense(units = 2, activation = 'softmax')


model %>% compile(
  loss = c('binary_crossentropy'),
  optimizer = optimizer_adam(lr = 0.00001),
  metrics = c('accuracy'))



history <- model %>% fit(
  df_train, y2_train, shuffle = T,
  epochs = 10, batch_size = 512, 
  validation_set = 0.2)


pred <- model %>% 
  predict_classes(d_test)

# model %>% evaluate(d_test, y_test)

confusionMatrix(as.numeric(pred), y_test[, 2], positive = "1")

#### STOP ####

# sum(y_test[, 1]) / sum(y_test) -- 0.978083

# 630998/630998 [==============================] - 14s 22us/step
# $loss
# [1] NaN
# 
# $acc
# [1] 0.9781552

sum(y_test[,1])/dim(y_test)[1]
# [1] 0.9781552

