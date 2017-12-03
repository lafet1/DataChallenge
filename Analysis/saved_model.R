# A

model %>% 
  layer_dense(units = 512, input_shape = c(dim(df_train)[2])) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_activity_regularization(l1 = 0.00001) %>%
  layer_activation_leaky_relu(alpha = 0.5) %>%
  
  layer_dense(units = 128) %>%
  layer_dropout(rate = 0.25) %>%
  layer_activation_leaky_relu(alpha = 0.3) %>%
  
  layer_dense(units = 32) %>%
  layer_dropout(rate = 0.25) %>%
  layer_activation_leaky_relu(alpha = 0.3) %>%
  
  layer_dense(units = 2, activation = 'softmax')


model %>% compile(
  loss = c('binary_crossentropy'),
  optimizer = optimizer_adam(lr = 0.0001),
  metrics = c('accuracy'))

# B

model %>% 
  layer_dense(units = 512, input_shape = c(dim(df_train)[2])) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_activity_regularization(l1 = 0.0001) %>%
  layer_activation_leaky_relu(alpha = 0.4) %>%
  
  layer_dense(units = 128) %>%
  layer_dropout(rate = 0.25) %>%
  layer_activation_leaky_relu(alpha = 0.3) %>%
  
  layer_dense(units = 32) %>%
  layer_dropout(rate = 0.25) %>%
  layer_activation_leaky_relu(alpha = 0.3) %>%
  
  layer_dense(units = 2, activation = 'softmax')

# C

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 512, input_shape = c(dim(df_train)[2])) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_activity_regularization(l1 = 0.00002) %>%
  layer_activation_leaky_relu(alpha = 0.35) %>%
  
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

confusionMatrix(as.numeric(pred), y_test[, 2])