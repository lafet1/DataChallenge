library(tidyverse)
library(keras)
library(DMwR)
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


d_train <- as.matrix(d_train)
d_test <- as.matrix(d_test)

# SMOTE
full_train <- as.data.frame(cbind(y_train[, 2], d_train))
names(full_train)[1] <- "target"
full_train$target <- as.factor(full_train$target)

smote <- SMOTE(target ~ ., full_train, perc.over = 500, perc.under = 300)
y_smote <- to_categorical(as.numeric(smote$target) - 1)
x_smote <- as.matrix(smote[, -1])

r <- runif(n = dim(d_train)[1])

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 64, activation = 'relu', input_shape = c(dim(x_smote)[2]))  %>% 
  #layer_dropout(rate = 0.25) %>%
  layer_dense(units = 16, activation = 'relu') %>%
  #layer_dropout(rate = 0.2) %>%
  layer_dense(units = 16, activation = 'sigmoid') %>%
  #layer_dropout(rate = 0.2) %>%
  layer_dense(units = 2, activation = 'softmax')

model %>% compile(
  loss = c('binary_crossentropy'),
  optimizer = optimizer_adam(lr = 0.0002),
  metrics = c('accuracy')
)




history <- model %>% fit(
  x_smote, y_smote, shuffle = T,
  epochs = 5, batch_size = 64, 
  validation_data = list(d_train[r < 0.1, ], y_train[r < 0.1, ])
)


model %>% 
  predict_classes(d_test) %>% head()

model %>% evaluate(d_test, y_test)

# sum(y_test[, 1]) / sum(y_test) -- 0.978083

# 630998/630998 [==============================] - 14s 22us/step
# $loss
# [1] NaN
# 
# $acc
# [1] 0.9781552

sum(y_test[,1])/dim(y_test)[1]
# [1] 0.9781552

