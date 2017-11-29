

rnd <- runif(n = dim(d)[1])
d_test <- d[rnd < 0.15, ]
d_train <- d[rnd >= 0.15, ]

rm(d)
rm(rnd)


# load("~/Škola/UvA/Výuka/Machine Learning for Econometrics/Project/DataChallenge/Analysis/.RData")



y_test <- to_categorical(d_test$was_promoted)
y_train <- to_categorical(d_train$was_promoted)
d_test$was_promoted <- NULL
d_train$was_promoted <- NULL


d_train <- as.matrix(d_train)

d_test <- as.matrix(d_test)

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 50, activation = 'sigmoid', input_shape = c(dim(d_train)[2]))  %>% 
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 25, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = 'softmax')

model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_sgd(),
  metrics = c('accuracy')
)





history <- model %>% fit(
  d_train, y_train, 
  epochs = 5, batch_size = 128, 
  validation_split = 0.2
)

sapply(d_train, class)
sum(sapply(d_train, function(x) sum(is.na(x))))


model %>% predict_classes(d_test) %>% head()
model %>% evaluate(d_test, y_test)

# 630998/630998 [==============================] - 14s 22us/step
# $loss
# [1] NaN
# 
# $acc
# [1] 0.9781552

sum(y_test[,1])/dim(y_test)[1]
# [1] 0.9781552

